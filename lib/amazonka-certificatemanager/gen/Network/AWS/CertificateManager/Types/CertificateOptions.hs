{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.CertificateOptions
  ( CertificateOptions (..)
  -- * Smart constructor
  , mkCertificateOptions
  -- * Lenses
  , coCertificateTransparencyLoggingPreference
  ) where

import qualified Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Structure that contains options for your certificate. Currently, you can use this only to specify whether to opt in to or out of certificate transparency logging. Some browsers require that public certificates issued for your domain be recorded in a log. Certificates that are not logged typically generate a browser error. Transparency makes it possible for you to detect SSL/TLS certificates that have been mistakenly or maliciously issued for your domain. For general information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-concepts.html#concept-transparency Certificate Transparency Logging> . 
--
-- /See:/ 'mkCertificateOptions' smart constructor.
newtype CertificateOptions = CertificateOptions'
  { certificateTransparencyLoggingPreference :: Core.Maybe Types.CertificateTransparencyLoggingPreference
    -- ^ You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CertificateOptions' value with any optional fields omitted.
mkCertificateOptions
    :: CertificateOptions
mkCertificateOptions
  = CertificateOptions'{certificateTransparencyLoggingPreference =
                          Core.Nothing}

-- | You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ . 
--
-- /Note:/ Consider using 'certificateTransparencyLoggingPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCertificateTransparencyLoggingPreference :: Lens.Lens' CertificateOptions (Core.Maybe Types.CertificateTransparencyLoggingPreference)
coCertificateTransparencyLoggingPreference = Lens.field @"certificateTransparencyLoggingPreference"
{-# INLINEABLE coCertificateTransparencyLoggingPreference #-}
{-# DEPRECATED certificateTransparencyLoggingPreference "Use generic-lens or generic-optics with 'certificateTransparencyLoggingPreference' instead"  #-}

instance Core.FromJSON CertificateOptions where
        toJSON CertificateOptions{..}
          = Core.object
              (Core.catMaybes
                 [("CertificateTransparencyLoggingPreference" Core..=) Core.<$>
                    certificateTransparencyLoggingPreference])

instance Core.FromJSON CertificateOptions where
        parseJSON
          = Core.withObject "CertificateOptions" Core.$
              \ x ->
                CertificateOptions' Core.<$>
                  (x Core..:? "CertificateTransparencyLoggingPreference")
