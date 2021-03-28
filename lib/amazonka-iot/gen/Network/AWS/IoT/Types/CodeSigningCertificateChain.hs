{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigningCertificateChain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CodeSigningCertificateChain
  ( CodeSigningCertificateChain (..)
  -- * Smart constructor
  , mkCodeSigningCertificateChain
  -- * Lenses
  , csccCertificateName
  , csccInlineDocument
  ) where

import qualified Network.AWS.IoT.Types.CertificateName as Types
import qualified Network.AWS.IoT.Types.InlineDocument as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the certificate chain being used when code signing a file.
--
-- /See:/ 'mkCodeSigningCertificateChain' smart constructor.
data CodeSigningCertificateChain = CodeSigningCertificateChain'
  { certificateName :: Core.Maybe Types.CertificateName
    -- ^ The name of the certificate.
  , inlineDocument :: Core.Maybe Types.InlineDocument
    -- ^ A base64 encoded binary representation of the code signing certificate chain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeSigningCertificateChain' value with any optional fields omitted.
mkCodeSigningCertificateChain
    :: CodeSigningCertificateChain
mkCodeSigningCertificateChain
  = CodeSigningCertificateChain'{certificateName = Core.Nothing,
                                 inlineDocument = Core.Nothing}

-- | The name of the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csccCertificateName :: Lens.Lens' CodeSigningCertificateChain (Core.Maybe Types.CertificateName)
csccCertificateName = Lens.field @"certificateName"
{-# INLINEABLE csccCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | A base64 encoded binary representation of the code signing certificate chain.
--
-- /Note:/ Consider using 'inlineDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csccInlineDocument :: Lens.Lens' CodeSigningCertificateChain (Core.Maybe Types.InlineDocument)
csccInlineDocument = Lens.field @"inlineDocument"
{-# INLINEABLE csccInlineDocument #-}
{-# DEPRECATED inlineDocument "Use generic-lens or generic-optics with 'inlineDocument' instead"  #-}

instance Core.FromJSON CodeSigningCertificateChain where
        toJSON CodeSigningCertificateChain{..}
          = Core.object
              (Core.catMaybes
                 [("certificateName" Core..=) Core.<$> certificateName,
                  ("inlineDocument" Core..=) Core.<$> inlineDocument])

instance Core.FromJSON CodeSigningCertificateChain where
        parseJSON
          = Core.withObject "CodeSigningCertificateChain" Core.$
              \ x ->
                CodeSigningCertificateChain' Core.<$>
                  (x Core..:? "certificateName") Core.<*> x Core..:? "inlineDocument"
