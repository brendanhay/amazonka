# A NixOS test (https://nixos.org/manual/nixos/stable/#sec-nixos-tests)
# which runs amazonka-s3 against MinIO.
let
  # NixOS tests currently only run on Linux.
  pkgs-x86_64-linux = import ../nix/nixpkgs.nix { system = "x86_64-linux"; };

  # Some bogus credentials.
  accessKey = "BKIKJAA5BMMU2RHO6IBB";
  secretKey = "V7f1CwQqAcwo80UEIJEjc5gVQUSSx5ohQ9GSrr12";

  minioPort = 9000;

  endpoint = "http://s3:${toString minioPort}";

  bucket = "my-bucket";
in
pkgs-x86_64-linux.nixosTest {
  name = "test-s3";
  nodes = {
    # A machine providing an S3 service using MinIO.
    # Note this machine will have IP 192.168.1.1
    s3 = { pkgs, ... }: {
      # Minio requires at least 1GiB of free disk space to run.
      virtualisation = {
        diskSize = 2 * 1024;
        memorySize = 1024;
      };
      networking.firewall.allowedTCPPorts = [ minioPort ];
      services.minio = {
        enable = true;
        rootCredentialsFile = pkgs.writeText "minio-credentials" ''
          MINIO_ROOT_USER=${accessKey}
          MINIO_ROOT_PASSWORD=${secretKey}
        '';
      };
    };
    # A machine simulating a user (amazonka)
    # interacting with the S3 service above.
    # Note this machine will have IP 192.168.1.2
    user = { pkgs, ... }: {
      environment.systemPackages = [
        pkgs.curl
        pkgs.minio-client
        pkgs.tcpdump
        pkgs.amazonka-s3-test-app
      ];
    };
  };
  testScript = ''
    s3.start()
    user.start()
    user.wait_until_succeeds("curl --fail --silent '${endpoint}/minio/health/live'")
    user.succeed("mc config host add minio ${endpoint} ${accessKey} ${secretKey} --api S3v4")
    user.succeed("mc mb minio/${bucket}")
    print(
      user.succeed(
        """
          set -x
          echo 'Hello World!' > ./some-file
          export AWS_ACCESS_KEY_ID="${accessKey}"
          export AWS_SECRET_ACCESS_KEY="${secretKey}"
          tcpdump -i eth1 -w http.cap &
          tcpdump_pid="$!"
          sleep 1
          amazonka-s3-test-app ./some-file ${endpoint} ${bucket} some-file || (
            sleep 1
            kill -s 2 $tcpdump_pid
            tcpdump -r http.cap -n -A -s 0 \
              'tcp port 9000 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)'
            exit 1
          )
        """
      )
    )
  '';
}
