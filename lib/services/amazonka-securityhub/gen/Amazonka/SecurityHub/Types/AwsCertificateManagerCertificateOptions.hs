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
-- Module      : Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains other options for the certificate.
--
-- /See:/ 'newAwsCertificateManagerCertificateOptions' smart constructor.
data AwsCertificateManagerCertificateOptions = AwsCertificateManagerCertificateOptions'
  { -- | Whether to add the certificate to a transparency log.
    --
    -- Valid values: @DISABLED@ | @ENABLED@
    certificateTransparencyLoggingPreference :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCertificateManagerCertificateOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateTransparencyLoggingPreference', 'awsCertificateManagerCertificateOptions_certificateTransparencyLoggingPreference' - Whether to add the certificate to a transparency log.
--
-- Valid values: @DISABLED@ | @ENABLED@
newAwsCertificateManagerCertificateOptions ::
  AwsCertificateManagerCertificateOptions
newAwsCertificateManagerCertificateOptions =
  AwsCertificateManagerCertificateOptions'
    { certificateTransparencyLoggingPreference =
        Prelude.Nothing
    }

-- | Whether to add the certificate to a transparency log.
--
-- Valid values: @DISABLED@ | @ENABLED@
awsCertificateManagerCertificateOptions_certificateTransparencyLoggingPreference :: Lens.Lens' AwsCertificateManagerCertificateOptions (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateOptions_certificateTransparencyLoggingPreference = Lens.lens (\AwsCertificateManagerCertificateOptions' {certificateTransparencyLoggingPreference} -> certificateTransparencyLoggingPreference) (\s@AwsCertificateManagerCertificateOptions' {} a -> s {certificateTransparencyLoggingPreference = a} :: AwsCertificateManagerCertificateOptions)

instance
  Data.FromJSON
    AwsCertificateManagerCertificateOptions
  where
  parseJSON =
    Data.withObject
      "AwsCertificateManagerCertificateOptions"
      ( \x ->
          AwsCertificateManagerCertificateOptions'
            Prelude.<$> ( x
                            Data..:? "CertificateTransparencyLoggingPreference"
                        )
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateOptions
  where
  hashWithSalt
    _salt
    AwsCertificateManagerCertificateOptions' {..} =
      _salt
        `Prelude.hashWithSalt` certificateTransparencyLoggingPreference

instance
  Prelude.NFData
    AwsCertificateManagerCertificateOptions
  where
  rnf AwsCertificateManagerCertificateOptions' {..} =
    Prelude.rnf
      certificateTransparencyLoggingPreference

instance
  Data.ToJSON
    AwsCertificateManagerCertificateOptions
  where
  toJSON AwsCertificateManagerCertificateOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateTransparencyLoggingPreference" Data..=)
              Prelude.<$> certificateTransparencyLoggingPreference
          ]
      )
