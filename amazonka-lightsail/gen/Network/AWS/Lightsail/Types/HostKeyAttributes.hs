{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HostKeyAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HostKeyAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the public SSH host keys or the RDP certificate.
--
-- /See:/ 'newHostKeyAttributes' smart constructor.
data HostKeyAttributes = HostKeyAttributes'
  { -- | The SSH host key algorithm or the RDP certificate format.
    --
    -- For SSH host keys, the algorithm may be @ssh-rsa@,
    -- @ecdsa-sha2-nistp256@, @ssh-ed25519@, etc. For RDP certificates, the
    -- algorithm is always @x509-cert@.
    algorithm :: Prelude.Maybe Prelude.Text,
    -- | The public SSH host key or the RDP certificate.
    publicKey :: Prelude.Maybe Prelude.Text,
    -- | The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
    --
    -- -   Example of an SHA-256 SSH fingerprint:
    --
    --     @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@
    --
    -- -   Example of an SHA-256 RDP fingerprint:
    --
    --     @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
    fingerprintSHA256 :: Prelude.Maybe Prelude.Text,
    -- | The returned RDP certificate is valid after this point in time.
    --
    -- This value is listed only for RDP certificates.
    notValidBefore :: Prelude.Maybe Prelude.POSIX,
    -- | The returned RDP certificate is not valid after this point in time.
    --
    -- This value is listed only for RDP certificates.
    notValidAfter :: Prelude.Maybe Prelude.POSIX,
    -- | The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
    --
    -- -   Example of an SHA-1 SSH fingerprint:
    --
    --     @SHA1:1CHH6FaAaXjtFOsR\/t83vf91SR0@
    --
    -- -   Example of an SHA-1 RDP fingerprint:
    --
    --     @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
    fingerprintSHA1 :: Prelude.Maybe Prelude.Text,
    -- | The time that the SSH host key or RDP certificate was recorded by
    -- Lightsail.
    witnessedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HostKeyAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'hostKeyAttributes_algorithm' - The SSH host key algorithm or the RDP certificate format.
--
-- For SSH host keys, the algorithm may be @ssh-rsa@,
-- @ecdsa-sha2-nistp256@, @ssh-ed25519@, etc. For RDP certificates, the
-- algorithm is always @x509-cert@.
--
-- 'publicKey', 'hostKeyAttributes_publicKey' - The public SSH host key or the RDP certificate.
--
-- 'fingerprintSHA256', 'hostKeyAttributes_fingerprintSHA256' - The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
--
-- -   Example of an SHA-256 SSH fingerprint:
--
--     @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@
--
-- -   Example of an SHA-256 RDP fingerprint:
--
--     @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
--
-- 'notValidBefore', 'hostKeyAttributes_notValidBefore' - The returned RDP certificate is valid after this point in time.
--
-- This value is listed only for RDP certificates.
--
-- 'notValidAfter', 'hostKeyAttributes_notValidAfter' - The returned RDP certificate is not valid after this point in time.
--
-- This value is listed only for RDP certificates.
--
-- 'fingerprintSHA1', 'hostKeyAttributes_fingerprintSHA1' - The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
--
-- -   Example of an SHA-1 SSH fingerprint:
--
--     @SHA1:1CHH6FaAaXjtFOsR\/t83vf91SR0@
--
-- -   Example of an SHA-1 RDP fingerprint:
--
--     @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
--
-- 'witnessedAt', 'hostKeyAttributes_witnessedAt' - The time that the SSH host key or RDP certificate was recorded by
-- Lightsail.
newHostKeyAttributes ::
  HostKeyAttributes
newHostKeyAttributes =
  HostKeyAttributes'
    { algorithm = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      fingerprintSHA256 = Prelude.Nothing,
      notValidBefore = Prelude.Nothing,
      notValidAfter = Prelude.Nothing,
      fingerprintSHA1 = Prelude.Nothing,
      witnessedAt = Prelude.Nothing
    }

-- | The SSH host key algorithm or the RDP certificate format.
--
-- For SSH host keys, the algorithm may be @ssh-rsa@,
-- @ecdsa-sha2-nistp256@, @ssh-ed25519@, etc. For RDP certificates, the
-- algorithm is always @x509-cert@.
hostKeyAttributes_algorithm :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.Text)
hostKeyAttributes_algorithm = Lens.lens (\HostKeyAttributes' {algorithm} -> algorithm) (\s@HostKeyAttributes' {} a -> s {algorithm = a} :: HostKeyAttributes)

-- | The public SSH host key or the RDP certificate.
hostKeyAttributes_publicKey :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.Text)
hostKeyAttributes_publicKey = Lens.lens (\HostKeyAttributes' {publicKey} -> publicKey) (\s@HostKeyAttributes' {} a -> s {publicKey = a} :: HostKeyAttributes)

-- | The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
--
-- -   Example of an SHA-256 SSH fingerprint:
--
--     @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@
--
-- -   Example of an SHA-256 RDP fingerprint:
--
--     @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
hostKeyAttributes_fingerprintSHA256 :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.Text)
hostKeyAttributes_fingerprintSHA256 = Lens.lens (\HostKeyAttributes' {fingerprintSHA256} -> fingerprintSHA256) (\s@HostKeyAttributes' {} a -> s {fingerprintSHA256 = a} :: HostKeyAttributes)

-- | The returned RDP certificate is valid after this point in time.
--
-- This value is listed only for RDP certificates.
hostKeyAttributes_notValidBefore :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.UTCTime)
hostKeyAttributes_notValidBefore = Lens.lens (\HostKeyAttributes' {notValidBefore} -> notValidBefore) (\s@HostKeyAttributes' {} a -> s {notValidBefore = a} :: HostKeyAttributes) Prelude.. Lens.mapping Prelude._Time

-- | The returned RDP certificate is not valid after this point in time.
--
-- This value is listed only for RDP certificates.
hostKeyAttributes_notValidAfter :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.UTCTime)
hostKeyAttributes_notValidAfter = Lens.lens (\HostKeyAttributes' {notValidAfter} -> notValidAfter) (\s@HostKeyAttributes' {} a -> s {notValidAfter = a} :: HostKeyAttributes) Prelude.. Lens.mapping Prelude._Time

-- | The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
--
-- -   Example of an SHA-1 SSH fingerprint:
--
--     @SHA1:1CHH6FaAaXjtFOsR\/t83vf91SR0@
--
-- -   Example of an SHA-1 RDP fingerprint:
--
--     @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
hostKeyAttributes_fingerprintSHA1 :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.Text)
hostKeyAttributes_fingerprintSHA1 = Lens.lens (\HostKeyAttributes' {fingerprintSHA1} -> fingerprintSHA1) (\s@HostKeyAttributes' {} a -> s {fingerprintSHA1 = a} :: HostKeyAttributes)

-- | The time that the SSH host key or RDP certificate was recorded by
-- Lightsail.
hostKeyAttributes_witnessedAt :: Lens.Lens' HostKeyAttributes (Prelude.Maybe Prelude.UTCTime)
hostKeyAttributes_witnessedAt = Lens.lens (\HostKeyAttributes' {witnessedAt} -> witnessedAt) (\s@HostKeyAttributes' {} a -> s {witnessedAt = a} :: HostKeyAttributes) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON HostKeyAttributes where
  parseJSON =
    Prelude.withObject
      "HostKeyAttributes"
      ( \x ->
          HostKeyAttributes'
            Prelude.<$> (x Prelude..:? "algorithm")
            Prelude.<*> (x Prelude..:? "publicKey")
            Prelude.<*> (x Prelude..:? "fingerprintSHA256")
            Prelude.<*> (x Prelude..:? "notValidBefore")
            Prelude.<*> (x Prelude..:? "notValidAfter")
            Prelude.<*> (x Prelude..:? "fingerprintSHA1")
            Prelude.<*> (x Prelude..:? "witnessedAt")
      )

instance Prelude.Hashable HostKeyAttributes

instance Prelude.NFData HostKeyAttributes
