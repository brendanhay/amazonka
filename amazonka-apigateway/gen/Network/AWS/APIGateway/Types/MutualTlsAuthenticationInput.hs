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
-- Module      : Network.AWS.APIGateway.Types.MutualTlsAuthenticationInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MutualTlsAuthenticationInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | If specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your custom domain name.
--
-- /See:/ 'newMutualTlsAuthenticationInput' smart constructor.
data MutualTlsAuthenticationInput = MutualTlsAuthenticationInput'
  { -- | The version of the S3 object that contains your truststore. To specify a
    -- version, you must have versioning enabled for the S3 bucket.
    truststoreVersion :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 resource ARN that specifies the truststore for mutual TLS
    -- authentication, for example, @s3:\/\/bucket-name\/key-name@. The
    -- truststore can contain certificates from public or private certificate
    -- authorities. To update the truststore, upload a new version to S3, and
    -- then update your custom domain name to use the new version. To update
    -- the truststore, you must have permissions to access the S3 object.
    truststoreUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MutualTlsAuthenticationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truststoreVersion', 'mutualTlsAuthenticationInput_truststoreVersion' - The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
--
-- 'truststoreUri', 'mutualTlsAuthenticationInput_truststoreUri' - An Amazon S3 resource ARN that specifies the truststore for mutual TLS
-- authentication, for example, @s3:\/\/bucket-name\/key-name@. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
newMutualTlsAuthenticationInput ::
  MutualTlsAuthenticationInput
newMutualTlsAuthenticationInput =
  MutualTlsAuthenticationInput'
    { truststoreVersion =
        Prelude.Nothing,
      truststoreUri = Prelude.Nothing
    }

-- | The version of the S3 object that contains your truststore. To specify a
-- version, you must have versioning enabled for the S3 bucket.
mutualTlsAuthenticationInput_truststoreVersion :: Lens.Lens' MutualTlsAuthenticationInput (Prelude.Maybe Prelude.Text)
mutualTlsAuthenticationInput_truststoreVersion = Lens.lens (\MutualTlsAuthenticationInput' {truststoreVersion} -> truststoreVersion) (\s@MutualTlsAuthenticationInput' {} a -> s {truststoreVersion = a} :: MutualTlsAuthenticationInput)

-- | An Amazon S3 resource ARN that specifies the truststore for mutual TLS
-- authentication, for example, @s3:\/\/bucket-name\/key-name@. The
-- truststore can contain certificates from public or private certificate
-- authorities. To update the truststore, upload a new version to S3, and
-- then update your custom domain name to use the new version. To update
-- the truststore, you must have permissions to access the S3 object.
mutualTlsAuthenticationInput_truststoreUri :: Lens.Lens' MutualTlsAuthenticationInput (Prelude.Maybe Prelude.Text)
mutualTlsAuthenticationInput_truststoreUri = Lens.lens (\MutualTlsAuthenticationInput' {truststoreUri} -> truststoreUri) (\s@MutualTlsAuthenticationInput' {} a -> s {truststoreUri = a} :: MutualTlsAuthenticationInput)

instance
  Prelude.Hashable
    MutualTlsAuthenticationInput

instance Prelude.NFData MutualTlsAuthenticationInput

instance Prelude.ToJSON MutualTlsAuthenticationInput where
  toJSON MutualTlsAuthenticationInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("truststoreVersion" Prelude..=)
              Prelude.<$> truststoreVersion,
            ("truststoreUri" Prelude..=)
              Prelude.<$> truststoreUri
          ]
      )
