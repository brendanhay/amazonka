{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CustomCodeSigning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CustomCodeSigning
  ( CustomCodeSigning (..)
  -- * Smart constructor
  , mkCustomCodeSigning
  -- * Lenses
  , ccsCertificateChain
  , ccsHashAlgorithm
  , ccsSignature
  , ccsSignatureAlgorithm
  ) where

import qualified Network.AWS.IoT.Types.CodeSigningCertificateChain as Types
import qualified Network.AWS.IoT.Types.CodeSigningSignature as Types
import qualified Network.AWS.IoT.Types.HashAlgorithm as Types
import qualified Network.AWS.IoT.Types.SignatureAlgorithm as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a custom method used to code sign a file.
--
-- /See:/ 'mkCustomCodeSigning' smart constructor.
data CustomCodeSigning = CustomCodeSigning'
  { certificateChain :: Core.Maybe Types.CodeSigningCertificateChain
    -- ^ The certificate chain.
  , hashAlgorithm :: Core.Maybe Types.HashAlgorithm
    -- ^ The hash algorithm used to code sign the file.
  , signature :: Core.Maybe Types.CodeSigningSignature
    -- ^ The signature for the file.
  , signatureAlgorithm :: Core.Maybe Types.SignatureAlgorithm
    -- ^ The signature algorithm used to code sign the file.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomCodeSigning' value with any optional fields omitted.
mkCustomCodeSigning
    :: CustomCodeSigning
mkCustomCodeSigning
  = CustomCodeSigning'{certificateChain = Core.Nothing,
                       hashAlgorithm = Core.Nothing, signature = Core.Nothing,
                       signatureAlgorithm = Core.Nothing}

-- | The certificate chain.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsCertificateChain :: Lens.Lens' CustomCodeSigning (Core.Maybe Types.CodeSigningCertificateChain)
ccsCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE ccsCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | The hash algorithm used to code sign the file.
--
-- /Note:/ Consider using 'hashAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsHashAlgorithm :: Lens.Lens' CustomCodeSigning (Core.Maybe Types.HashAlgorithm)
ccsHashAlgorithm = Lens.field @"hashAlgorithm"
{-# INLINEABLE ccsHashAlgorithm #-}
{-# DEPRECATED hashAlgorithm "Use generic-lens or generic-optics with 'hashAlgorithm' instead"  #-}

-- | The signature for the file.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSignature :: Lens.Lens' CustomCodeSigning (Core.Maybe Types.CodeSigningSignature)
ccsSignature = Lens.field @"signature"
{-# INLINEABLE ccsSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

-- | The signature algorithm used to code sign the file.
--
-- /Note:/ Consider using 'signatureAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSignatureAlgorithm :: Lens.Lens' CustomCodeSigning (Core.Maybe Types.SignatureAlgorithm)
ccsSignatureAlgorithm = Lens.field @"signatureAlgorithm"
{-# INLINEABLE ccsSignatureAlgorithm #-}
{-# DEPRECATED signatureAlgorithm "Use generic-lens or generic-optics with 'signatureAlgorithm' instead"  #-}

instance Core.FromJSON CustomCodeSigning where
        toJSON CustomCodeSigning{..}
          = Core.object
              (Core.catMaybes
                 [("certificateChain" Core..=) Core.<$> certificateChain,
                  ("hashAlgorithm" Core..=) Core.<$> hashAlgorithm,
                  ("signature" Core..=) Core.<$> signature,
                  ("signatureAlgorithm" Core..=) Core.<$> signatureAlgorithm])

instance Core.FromJSON CustomCodeSigning where
        parseJSON
          = Core.withObject "CustomCodeSigning" Core.$
              \ x ->
                CustomCodeSigning' Core.<$>
                  (x Core..:? "certificateChain") Core.<*> x Core..:? "hashAlgorithm"
                    Core.<*> x Core..:? "signature"
                    Core.<*> x Core..:? "signatureAlgorithm"
