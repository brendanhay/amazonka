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
    s3 = { pkgs, ... }: {
      # Minio requires at least 1GiB of free disk space to run.
      virtualisation = {
        diskSize = 2 * 1024;
        memorySize = 1024;
      };
      services.minio = {
        enable = true;
        rootCredentialsFile = pkgs.writeText "minio-credentials" ''
          MINIO_ROOT_USER=${accessKey}
          MINIO_ROOT_PASSWORD=${secretKey}
        '';
      };
      environment.systemPackages = [
        pkgs.curl
        pkgs.minio-client
        pkgs.amazonka-s3-test-app
      ];
    };
  };
  testScript = ''
    s3.start()
    s3.wait_until_succeeds("curl --fail --silent '${endpoint}/minio/health/live'")
    s3.succeed("mc config host add minio ${endpoint} ${accessKey} ${secretKey} --api S3v4")
    s3.succeed("mc mb minio/${bucket}")
    s3.succeed(
      """
        echo 'Hello World!' > ./some-file
        export AWS_ACCESS_KEY_ID="${accessKey}"
        export AWS_SECRET_ACCESS_KEY="${secretKey}"
        amazonka-s3-test-app ./some-file ${endpoint} ${bucket} some-file
      """
    )
  '';
}
