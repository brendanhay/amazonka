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
-- Module      : Network.AWS.CertificateManager.Types.CertificateOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateOptions where

import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Structure that contains options for your certificate. Currently, you can
-- use this only to specify whether to opt in to or out of certificate
-- transparency logging. Some browsers require that public certificates
-- issued for your domain be recorded in a log. Certificates that are not
-- logged typically generate a browser error. Transparency makes it
-- possible for you to detect SSL\/TLS certificates that have been
-- mistakenly or maliciously issued for your domain. For general
-- information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-concepts.html#concept-transparency Certificate Transparency Logging>.
--
-- /See:/ 'newCertificateOptions' smart constructor.
data CertificateOptions = CertificateOptions'
  { -- | You can opt out of certificate transparency logging by specifying the
    -- @DISABLED@ option. Opt in by specifying @ENABLED@.
    certificateTransparencyLoggingPreference :: Prelude.Maybe CertificateTransparencyLoggingPreference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CertificateOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateTransparencyLoggingPreference', 'certificateOptions_certificateTransparencyLoggingPreference' - You can opt out of certificate transparency logging by specifying the
-- @DISABLED@ option. Opt in by specifying @ENABLED@.
newCertificateOptions ::
  CertificateOptions
newCertificateOptions =
  CertificateOptions'
    { certificateTransparencyLoggingPreference =
        Prelude.Nothing
    }

-- | You can opt out of certificate transparency logging by specifying the
-- @DISABLED@ option. Opt in by specifying @ENABLED@.
certificateOptions_certificateTransparencyLoggingPreference :: Lens.Lens' CertificateOptions (Prelude.Maybe CertificateTransparencyLoggingPreference)
certificateOptions_certificateTransparencyLoggingPreference = Lens.lens (\CertificateOptions' {certificateTransparencyLoggingPreference} -> certificateTransparencyLoggingPreference) (\s@CertificateOptions' {} a -> s {certificateTransparencyLoggingPreference = a} :: CertificateOptions)

instance Prelude.FromJSON CertificateOptions where
  parseJSON =
    Prelude.withObject
      "CertificateOptions"
      ( \x ->
          CertificateOptions'
            Prelude.<$> ( x
                            Prelude..:? "CertificateTransparencyLoggingPreference"
                        )
      )

instance Prelude.Hashable CertificateOptions

instance Prelude.NFData CertificateOptions

instance Prelude.ToJSON CertificateOptions where
  toJSON CertificateOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ( "CertificateTransparencyLoggingPreference"
                Prelude..=
            )
              Prelude.<$> certificateTransparencyLoggingPreference
          ]
      )
