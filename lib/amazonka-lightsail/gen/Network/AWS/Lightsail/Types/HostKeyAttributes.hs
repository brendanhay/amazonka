{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HostKeyAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.HostKeyAttributes
  ( HostKeyAttributes (..)
  -- * Smart constructor
  , mkHostKeyAttributes
  -- * Lenses
  , hkaAlgorithm
  , hkaFingerprintSHA1
  , hkaFingerprintSHA256
  , hkaNotValidAfter
  , hkaNotValidBefore
  , hkaPublicKey
  , hkaWitnessedAt
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the public SSH host keys or the RDP certificate.
--
-- /See:/ 'mkHostKeyAttributes' smart constructor.
data HostKeyAttributes = HostKeyAttributes'
  { algorithm :: Core.Maybe Core.Text
    -- ^ The SSH host key algorithm or the RDP certificate format.
--
-- For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
  , fingerprintSHA1 :: Core.Maybe Core.Text
    -- ^ The SHA-1 fingerprint of the returned SSH host key or RDP certificate.
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
  , fingerprintSHA256 :: Core.Maybe Core.Text
    -- ^ The SHA-256 fingerprint of the returned SSH host key or RDP certificate.
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
  , notValidAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ The returned RDP certificate is not valid after this point in time.
--
-- This value is listed only for RDP certificates.
  , notValidBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ The returned RDP certificate is valid after this point in time.
--
-- This value is listed only for RDP certificates.
  , publicKey :: Core.Maybe Core.Text
    -- ^ The public SSH host key or the RDP certificate.
  , witnessedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the SSH host key or RDP certificate was recorded by Lightsail.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HostKeyAttributes' value with any optional fields omitted.
mkHostKeyAttributes
    :: HostKeyAttributes
mkHostKeyAttributes
  = HostKeyAttributes'{algorithm = Core.Nothing,
                       fingerprintSHA1 = Core.Nothing, fingerprintSHA256 = Core.Nothing,
                       notValidAfter = Core.Nothing, notValidBefore = Core.Nothing,
                       publicKey = Core.Nothing, witnessedAt = Core.Nothing}

-- | The SSH host key algorithm or the RDP certificate format.
--
-- For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaAlgorithm :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.Text)
hkaAlgorithm = Lens.field @"algorithm"
{-# INLINEABLE hkaAlgorithm #-}
{-# DEPRECATED algorithm "Use generic-lens or generic-optics with 'algorithm' instead"  #-}

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
hkaFingerprintSHA1 :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.Text)
hkaFingerprintSHA1 = Lens.field @"fingerprintSHA1"
{-# INLINEABLE hkaFingerprintSHA1 #-}
{-# DEPRECATED fingerprintSHA1 "Use generic-lens or generic-optics with 'fingerprintSHA1' instead"  #-}

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
hkaFingerprintSHA256 :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.Text)
hkaFingerprintSHA256 = Lens.field @"fingerprintSHA256"
{-# INLINEABLE hkaFingerprintSHA256 #-}
{-# DEPRECATED fingerprintSHA256 "Use generic-lens or generic-optics with 'fingerprintSHA256' instead"  #-}

-- | The returned RDP certificate is not valid after this point in time.
--
-- This value is listed only for RDP certificates.
--
-- /Note:/ Consider using 'notValidAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaNotValidAfter :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.NominalDiffTime)
hkaNotValidAfter = Lens.field @"notValidAfter"
{-# INLINEABLE hkaNotValidAfter #-}
{-# DEPRECATED notValidAfter "Use generic-lens or generic-optics with 'notValidAfter' instead"  #-}

-- | The returned RDP certificate is valid after this point in time.
--
-- This value is listed only for RDP certificates.
--
-- /Note:/ Consider using 'notValidBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaNotValidBefore :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.NominalDiffTime)
hkaNotValidBefore = Lens.field @"notValidBefore"
{-# INLINEABLE hkaNotValidBefore #-}
{-# DEPRECATED notValidBefore "Use generic-lens or generic-optics with 'notValidBefore' instead"  #-}

-- | The public SSH host key or the RDP certificate.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaPublicKey :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.Text)
hkaPublicKey = Lens.field @"publicKey"
{-# INLINEABLE hkaPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

-- | The time that the SSH host key or RDP certificate was recorded by Lightsail.
--
-- /Note:/ Consider using 'witnessedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkaWitnessedAt :: Lens.Lens' HostKeyAttributes (Core.Maybe Core.NominalDiffTime)
hkaWitnessedAt = Lens.field @"witnessedAt"
{-# INLINEABLE hkaWitnessedAt #-}
{-# DEPRECATED witnessedAt "Use generic-lens or generic-optics with 'witnessedAt' instead"  #-}

instance Core.FromJSON HostKeyAttributes where
        parseJSON
          = Core.withObject "HostKeyAttributes" Core.$
              \ x ->
                HostKeyAttributes' Core.<$>
                  (x Core..:? "algorithm") Core.<*> x Core..:? "fingerprintSHA1"
                    Core.<*> x Core..:? "fingerprintSHA256"
                    Core.<*> x Core..:? "notValidAfter"
                    Core.<*> x Core..:? "notValidBefore"
                    Core.<*> x Core..:? "publicKey"
                    Core.<*> x Core..:? "witnessedAt"
