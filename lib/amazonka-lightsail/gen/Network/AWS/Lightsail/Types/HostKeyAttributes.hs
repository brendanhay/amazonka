{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HostKeyAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HostKeyAttributes
  ( HostKeyAttributes (..),

    -- * Smart constructor
    mkHostKeyAttributes,

    -- * Lenses
    hkaNotValidAfter,
    hkaNotValidBefore,
    hkaFingerprintSHA1,
    hkaPublicKey,
    hkaAlgorithm,
    hkaWitnessedAt,
    hkaFingerprintSHA256,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the public SSH host keys or the RDP certificate.
--
-- /See:/ 'mkHostKeyAttributes' smart constructor.
data HostKeyAttributes = HostKeyAttributes'
  { -- | The returned RDP certificate is not valid after this point in time.
    --
    -- This value is listed only for RDP certificates.
    notValidAfter :: Lude.Maybe Lude.Timestamp,
    -- | The returned RDP certificate is valid after this point in time.
    --
    -- This value is listed only for RDP certificates.
    notValidBefore :: Lude.Maybe Lude.Timestamp,
    -- | The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
    --
    --
    --     * Example of an SHA-1 SSH fingerprint:
    -- @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@
    --
    --
    --     * Example of an SHA-1 RDP fingerprint:
    -- @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
    fingerprintSHA1 :: Lude.Maybe Lude.Text,
    -- | The public SSH host key or the RDP certificate.
    publicKey :: Lude.Maybe Lude.Text,
    -- | The SSH host key algorithm or the RDP certificate format.
    --
    -- For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
    algorithm :: Lude.Maybe Lude.Text,
    -- | The time that the SSH host key or RDP certificate was recorded by Lightsail.
    witnessedAt :: Lude.Maybe Lude.Timestamp,
    -- | The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
    --
    --
    --     * Example of an SHA-256 SSH fingerprint:
    -- @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@
    --
    --
    --     * Example of an SHA-256 RDP fingerprint:
    -- @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
    fingerprintSHA256 :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostKeyAttributes' with the minimum fields required to make a request.
--
-- * 'notValidAfter' - The returned RDP certificate is not valid after this point in time.
--
-- This value is listed only for RDP certificates.
-- * 'notValidBefore' - The returned RDP certificate is valid after this point in time.
--
-- This value is listed only for RDP certificates.
-- * 'fingerprintSHA1' - The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
--
--
--     * Example of an SHA-1 SSH fingerprint:
-- @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@
--
--
--     * Example of an SHA-1 RDP fingerprint:
-- @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
--
--
-- * 'publicKey' - The public SSH host key or the RDP certificate.
-- * 'algorithm' - The SSH host key algorithm or the RDP certificate format.
--
-- For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
-- * 'witnessedAt' - The time that the SSH host key or RDP certificate was recorded by Lightsail.
-- * 'fingerprintSHA256' - The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
--
--
--     * Example of an SHA-256 SSH fingerprint:
-- @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@
--
--
--     * Example of an SHA-256 RDP fingerprint:
-- @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
mkHostKeyAttributes ::
  HostKeyAttributes
mkHostKeyAttributes =
  HostKeyAttributes'
    { notValidAfter = Lude.Nothing,
      notValidBefore = Lude.Nothing,
      fingerprintSHA1 = Lude.Nothing,
      publicKey = Lude.Nothing,
      algorithm = Lude.Nothing,
      witnessedAt = Lude.Nothing,
      fingerprintSHA256 = Lude.Nothing
    }

-- | The returned RDP certificate is not valid after this point in time.
--
-- This value is listed only for RDP certificates.
--
-- /Note:/ Consider using 'notValidAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaNotValidAfter :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Timestamp)
hkaNotValidAfter = Lens.lens (notValidAfter :: HostKeyAttributes -> Lude.Maybe Lude.Timestamp) (\s a -> s {notValidAfter = a} :: HostKeyAttributes)
{-# DEPRECATED hkaNotValidAfter "Use generic-lens or generic-optics with 'notValidAfter' instead." #-}

-- | The returned RDP certificate is valid after this point in time.
--
-- This value is listed only for RDP certificates.
--
-- /Note:/ Consider using 'notValidBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaNotValidBefore :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Timestamp)
hkaNotValidBefore = Lens.lens (notValidBefore :: HostKeyAttributes -> Lude.Maybe Lude.Timestamp) (\s a -> s {notValidBefore = a} :: HostKeyAttributes)
{-# DEPRECATED hkaNotValidBefore "Use generic-lens or generic-optics with 'notValidBefore' instead." #-}

-- | The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
--
--
--     * Example of an SHA-1 SSH fingerprint:
-- @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@
--
--
--     * Example of an SHA-1 RDP fingerprint:
-- @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
--
--
--
-- /Note:/ Consider using 'fingerprintSHA1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaFingerprintSHA1 :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Text)
hkaFingerprintSHA1 = Lens.lens (fingerprintSHA1 :: HostKeyAttributes -> Lude.Maybe Lude.Text) (\s a -> s {fingerprintSHA1 = a} :: HostKeyAttributes)
{-# DEPRECATED hkaFingerprintSHA1 "Use generic-lens or generic-optics with 'fingerprintSHA1' instead." #-}

-- | The public SSH host key or the RDP certificate.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaPublicKey :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Text)
hkaPublicKey = Lens.lens (publicKey :: HostKeyAttributes -> Lude.Maybe Lude.Text) (\s a -> s {publicKey = a} :: HostKeyAttributes)
{-# DEPRECATED hkaPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The SSH host key algorithm or the RDP certificate format.
--
-- For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaAlgorithm :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Text)
hkaAlgorithm = Lens.lens (algorithm :: HostKeyAttributes -> Lude.Maybe Lude.Text) (\s a -> s {algorithm = a} :: HostKeyAttributes)
{-# DEPRECATED hkaAlgorithm "Use generic-lens or generic-optics with 'algorithm' instead." #-}

-- | The time that the SSH host key or RDP certificate was recorded by Lightsail.
--
-- /Note:/ Consider using 'witnessedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaWitnessedAt :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Timestamp)
hkaWitnessedAt = Lens.lens (witnessedAt :: HostKeyAttributes -> Lude.Maybe Lude.Timestamp) (\s a -> s {witnessedAt = a} :: HostKeyAttributes)
{-# DEPRECATED hkaWitnessedAt "Use generic-lens or generic-optics with 'witnessedAt' instead." #-}

-- | The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
--
--
--     * Example of an SHA-256 SSH fingerprint:
-- @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@
--
--
--     * Example of an SHA-256 RDP fingerprint:
-- @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
--
--
--
-- /Note:/ Consider using 'fingerprintSHA256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaFingerprintSHA256 :: Lens.Lens' HostKeyAttributes (Lude.Maybe Lude.Text)
hkaFingerprintSHA256 = Lens.lens (fingerprintSHA256 :: HostKeyAttributes -> Lude.Maybe Lude.Text) (\s a -> s {fingerprintSHA256 = a} :: HostKeyAttributes)
{-# DEPRECATED hkaFingerprintSHA256 "Use generic-lens or generic-optics with 'fingerprintSHA256' instead." #-}

instance Lude.FromJSON HostKeyAttributes where
  parseJSON =
    Lude.withObject
      "HostKeyAttributes"
      ( \x ->
          HostKeyAttributes'
            Lude.<$> (x Lude..:? "notValidAfter")
            Lude.<*> (x Lude..:? "notValidBefore")
            Lude.<*> (x Lude..:? "fingerprintSHA1")
            Lude.<*> (x Lude..:? "publicKey")
            Lude.<*> (x Lude..:? "algorithm")
            Lude.<*> (x Lude..:? "witnessedAt")
            Lude.<*> (x Lude..:? "fingerprintSHA256")
      )
