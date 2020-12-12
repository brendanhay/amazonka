{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateOptions
  ( CertificateOptions (..),

    -- * Smart constructor
    mkCertificateOptions,

    -- * Lenses
    coCertificateTransparencyLoggingPreference,
  )
where

import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Structure that contains options for your certificate. Currently, you can use this only to specify whether to opt in to or out of certificate transparency logging. Some browsers require that public certificates issued for your domain be recorded in a log. Certificates that are not logged typically generate a browser error. Transparency makes it possible for you to detect SSL/TLS certificates that have been mistakenly or maliciously issued for your domain. For general information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-concepts.html#concept-transparency Certificate Transparency Logging> .
--
-- /See:/ 'mkCertificateOptions' smart constructor.
newtype CertificateOptions = CertificateOptions'
  { certificateTransparencyLoggingPreference ::
      Lude.Maybe
        CertificateTransparencyLoggingPreference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateOptions' with the minimum fields required to make a request.
--
-- * 'certificateTransparencyLoggingPreference' - You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ .
mkCertificateOptions ::
  CertificateOptions
mkCertificateOptions =
  CertificateOptions'
    { certificateTransparencyLoggingPreference =
        Lude.Nothing
    }

-- | You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ .
--
-- /Note:/ Consider using 'certificateTransparencyLoggingPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCertificateTransparencyLoggingPreference :: Lens.Lens' CertificateOptions (Lude.Maybe CertificateTransparencyLoggingPreference)
coCertificateTransparencyLoggingPreference = Lens.lens (certificateTransparencyLoggingPreference :: CertificateOptions -> Lude.Maybe CertificateTransparencyLoggingPreference) (\s a -> s {certificateTransparencyLoggingPreference = a} :: CertificateOptions)
{-# DEPRECATED coCertificateTransparencyLoggingPreference "Use generic-lens or generic-optics with 'certificateTransparencyLoggingPreference' instead." #-}

instance Lude.FromJSON CertificateOptions where
  parseJSON =
    Lude.withObject
      "CertificateOptions"
      ( \x ->
          CertificateOptions'
            Lude.<$> (x Lude..:? "CertificateTransparencyLoggingPreference")
      )

instance Lude.ToJSON CertificateOptions where
  toJSON CertificateOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CertificateTransparencyLoggingPreference" Lude..=)
              Lude.<$> certificateTransparencyLoggingPreference
          ]
      )
