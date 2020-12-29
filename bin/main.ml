let () = 
    let open Ocaml_portaudio in    
    Portaudio.initialize();
    print_endline Portaudio.(get_version_text());
    let sample_rate = 44100. in
    let dt = 1. /. sample_rate in
    let global_time = ref 0. in
    let stream = Portaudio.Stream.open_default_stream 
        ~num_input_channels:0
        ~num_output_channels:2
        ~format:Portaudio.SampleFormat.N_Int24
        ~sample_rate
        ~frames_per_buffer:0
        ~callback:(fun _ out ~time_info:_ ~status:_ -> 
            let len = Portaudio.View.length out.(0) in

            let get_pitch t =
                if t > 3. then 587.33
                else if t > 2. then 523.25
                else if t > 1. then 493.88
                else if t <= 0.  then 0.
                else 440.
            in

            let get_chan t =
                if t > 3. then 0
                else if t > 2. then 1
                else if t > 1. then 0
                else 1
            in

            for i=0 to len -1 do
                let t = !global_time +. (float i *. dt) in
                let pitch = get_pitch t in
                let chan = get_chan t in
                let v = Float.(sin (t *. 2.*. pi *. pitch)) *. (4096. *. 2048.) in
                Portaudio.View.set out.((chan + 1) mod 2) i 0;
                Portaudio.View.set out.(chan) i Float.(to_int v);
            done;
            global_time := !global_time +. (float len *. dt);
            Portaudio.Stream.Callback.Result.Continue
        )
        ()
    in
    Portaudio.Stream.start stream;
    print_endline "Sleeping";
    Portaudio.sleep 4000;
    print_endline "Done Sleeping";
    Portaudio.Stream.stop stream;
    let stream = Portaudio.Stream.open_default_stream
        ~num_input_channels:0
        ~num_output_channels:2
        ~format:Portaudio.SampleFormat.N_Float32
        ~sample_rate
        ~frames_per_buffer:0
        ()
    in
    let module V = Portaudio.View in
    let sample_rate = Float.to_int sample_rate in
    let data = V.create Ctypes.float ~len:sample_rate in
    let data2 = V.create Ctypes.float ~len:sample_rate in
    for i=0 to V.length data - 1 do
        let t = float i*.dt in
        V.set data i Float.(sin (2. *. pi *. t *. 440.));
        V.set data2 i Float.(sin (2. *. pi *. t *. 880.));
    done;
    Portaudio.Stream.start stream;
    Portaudio.Stream.write_non_interleaved stream [|data2; data|];
    Portaudio.sleep 1000;
    Portaudio.Stream.stop stream;
    Portaudio.terminate();
;;
