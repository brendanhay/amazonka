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
-- Module      : Amazonka.ApiGatewayV2.Types.MutualTlsAuthenticationInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.MutualTlsAuthenticationInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newMutualTlsAuthenticationInput' smart constructor.
data MutualTlsAuthenticationInput = MutualTlsAuthenticationInput'
  { -- | An Amazon S3 URL that specifies the truststore for mutual TLS
    -- authentication, for example, s3:\/\/bucket-name\/key-name. The
    -- truststore can contain certificates from public or private certificate
    -- authorities. To update the truststore, upload a new version to S3, and
    -- then update your custom domain name to use the new version. To update
    -- the truststore, you must have permissions to access the S3 object.
    truststoreUri :: Prelude.Maybe Prelude.Text,
    -- | The version of the S3 object that contains your truststore. To specify a
    -- version, you must have versioning enabled for the S3 bucket.
    truststoreVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MutualTlsAuthenticationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truststoreUri', 'mutualTlsAuthenticationInput_truststoreUri' - An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example, s3:\/\/bucket-name\/key-name. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
--
-- 'truststoreVersion', 'mutualTlsAuthenticationInput_truststoreVersion' - The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
newMutualTlsAuthenticationInput ::
  MutualTlsAuthenticationInput
newMutualTlsAuthenticationInput =
  MutualTlsAuthenticationInput'
    { truststoreUri =
        Prelude.Nothing,
      truststoreVersion = Prelude.Nothing
    }

-- | An Amazon S3 URL that specifies the truststore for mutual TLS
-- authentication, for example, s3:\/\/bucket-name\/key-name. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
mutualTlsAuthenticationInput_truststoreUri :: Lens.Lens' MutualTlsAuthenticationInput (Prelude.Maybe Prelude.Text)
mutualTlsAuthenticationInput_truststoreUri = Lens.lens (\MutualTlsAuthenticationInput' {truststoreUri} -> truststoreUri) (\s@MutualTlsAuthenticationInput' {} a -> s {truststoreUri = a} :: MutualTlsAuthenticationInput)

-- | The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
mutualTlsAuthenticationInput_truststoreVersion :: Lens.Lens' MutualTlsAuthenticationInput (Prelude.Maybe Prelude.Text)
mutualTlsAuthenticationInput_truststoreVersion = Lens.lens (\MutualTlsAuthenticationInput' {truststoreVersion} -> truststoreVersion) (\s@MutualTlsAuthenticationInput' {} a -> s {truststoreVersion = a} :: MutualTlsAuthenticationInput)

instance
  Prelude.Hashable
    MutualTlsAuthenticationInput
  where
  hashWithSalt _salt MutualTlsAuthenticationInput' {..} =
    _salt `Prelude.hashWithSalt` truststoreUri
      `Prelude.hashWithSalt` truststoreVersion

instance Prelude.NFData MutualTlsAuthenticationInput where
  rnf MutualTlsAuthenticationInput' {..} =
    Prelude.rnf truststoreUri
      `Prelude.seq` Prelude.rnf truststoreVersion

instance Data.ToJSON MutualTlsAuthenticationInput where
  toJSON MutualTlsAuthenticationInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("truststoreUri" Data..=) Prelude.<$> truststoreUri,
            ("truststoreVersion" Data..=)
              Prelude.<$> truststoreVersion
          ]
      )
