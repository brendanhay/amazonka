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
-- Module      : Amazonka.MediaTailor.Types.AccessConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AccessConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.AccessType
import Amazonka.MediaTailor.Types.SecretsManagerAccessTokenConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Access configuration parameters.
--
-- /See:/ 'newAccessConfiguration' smart constructor.
data AccessConfiguration = AccessConfiguration'
  { -- | The type of authentication used to access content from
    -- @HttpConfiguration::BaseUrl@ on your source location. Accepted value:
    -- @S3_SIGV4@.
    --
    -- @S3_SIGV4@ - AWS Signature Version 4 authentication for Amazon S3 hosted
    -- virtual-style access. If your source location base URL is an Amazon S3
    -- bucket, MediaTailor can use AWS Signature Version 4 (SigV4)
    -- authentication to access the bucket where your source content is stored.
    -- Your MediaTailor source location baseURL must follow the S3 virtual
    -- hosted-style request URL format. For example,
    -- https:\/\/bucket-name.s3.Region.amazonaws.com\/key-name.
    --
    -- Before you can use @S3_SIGV4@, you must meet these requirements:
    --
    -- • You must allow MediaTailor to access your S3 bucket by granting
    -- mediatailor.amazonaws.com principal access in IAM. For information about
    -- configuring access in IAM, see Access management in the IAM User Guide.
    --
    -- • The mediatailor.amazonaws.com service principal must have permissions
    -- to read all top level manifests referenced by the VodSource packaging
    -- configurations.
    --
    -- • The caller of the API must have s3:GetObject IAM permissions to read
    -- all top level manifests referenced by your MediaTailor VodSource
    -- packaging configurations.
    accessType :: Prelude.Maybe AccessType,
    -- | AWS Secrets Manager access token configuration parameters.
    secretsManagerAccessTokenConfiguration :: Prelude.Maybe SecretsManagerAccessTokenConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessType', 'accessConfiguration_accessType' - The type of authentication used to access content from
-- @HttpConfiguration::BaseUrl@ on your source location. Accepted value:
-- @S3_SIGV4@.
--
-- @S3_SIGV4@ - AWS Signature Version 4 authentication for Amazon S3 hosted
-- virtual-style access. If your source location base URL is an Amazon S3
-- bucket, MediaTailor can use AWS Signature Version 4 (SigV4)
-- authentication to access the bucket where your source content is stored.
-- Your MediaTailor source location baseURL must follow the S3 virtual
-- hosted-style request URL format. For example,
-- https:\/\/bucket-name.s3.Region.amazonaws.com\/key-name.
--
-- Before you can use @S3_SIGV4@, you must meet these requirements:
--
-- • You must allow MediaTailor to access your S3 bucket by granting
-- mediatailor.amazonaws.com principal access in IAM. For information about
-- configuring access in IAM, see Access management in the IAM User Guide.
--
-- • The mediatailor.amazonaws.com service principal must have permissions
-- to read all top level manifests referenced by the VodSource packaging
-- configurations.
--
-- • The caller of the API must have s3:GetObject IAM permissions to read
-- all top level manifests referenced by your MediaTailor VodSource
-- packaging configurations.
--
-- 'secretsManagerAccessTokenConfiguration', 'accessConfiguration_secretsManagerAccessTokenConfiguration' - AWS Secrets Manager access token configuration parameters.
newAccessConfiguration ::
  AccessConfiguration
newAccessConfiguration =
  AccessConfiguration'
    { accessType = Prelude.Nothing,
      secretsManagerAccessTokenConfiguration =
        Prelude.Nothing
    }

-- | The type of authentication used to access content from
-- @HttpConfiguration::BaseUrl@ on your source location. Accepted value:
-- @S3_SIGV4@.
--
-- @S3_SIGV4@ - AWS Signature Version 4 authentication for Amazon S3 hosted
-- virtual-style access. If your source location base URL is an Amazon S3
-- bucket, MediaTailor can use AWS Signature Version 4 (SigV4)
-- authentication to access the bucket where your source content is stored.
-- Your MediaTailor source location baseURL must follow the S3 virtual
-- hosted-style request URL format. For example,
-- https:\/\/bucket-name.s3.Region.amazonaws.com\/key-name.
--
-- Before you can use @S3_SIGV4@, you must meet these requirements:
--
-- • You must allow MediaTailor to access your S3 bucket by granting
-- mediatailor.amazonaws.com principal access in IAM. For information about
-- configuring access in IAM, see Access management in the IAM User Guide.
--
-- • The mediatailor.amazonaws.com service principal must have permissions
-- to read all top level manifests referenced by the VodSource packaging
-- configurations.
--
-- • The caller of the API must have s3:GetObject IAM permissions to read
-- all top level manifests referenced by your MediaTailor VodSource
-- packaging configurations.
accessConfiguration_accessType :: Lens.Lens' AccessConfiguration (Prelude.Maybe AccessType)
accessConfiguration_accessType = Lens.lens (\AccessConfiguration' {accessType} -> accessType) (\s@AccessConfiguration' {} a -> s {accessType = a} :: AccessConfiguration)

-- | AWS Secrets Manager access token configuration parameters.
accessConfiguration_secretsManagerAccessTokenConfiguration :: Lens.Lens' AccessConfiguration (Prelude.Maybe SecretsManagerAccessTokenConfiguration)
accessConfiguration_secretsManagerAccessTokenConfiguration = Lens.lens (\AccessConfiguration' {secretsManagerAccessTokenConfiguration} -> secretsManagerAccessTokenConfiguration) (\s@AccessConfiguration' {} a -> s {secretsManagerAccessTokenConfiguration = a} :: AccessConfiguration)

instance Data.FromJSON AccessConfiguration where
  parseJSON =
    Data.withObject
      "AccessConfiguration"
      ( \x ->
          AccessConfiguration'
            Prelude.<$> (x Data..:? "AccessType")
            Prelude.<*> ( x
                            Data..:? "SecretsManagerAccessTokenConfiguration"
                        )
      )

instance Prelude.Hashable AccessConfiguration where
  hashWithSalt _salt AccessConfiguration' {..} =
    _salt `Prelude.hashWithSalt` accessType
      `Prelude.hashWithSalt` secretsManagerAccessTokenConfiguration

instance Prelude.NFData AccessConfiguration where
  rnf AccessConfiguration' {..} =
    Prelude.rnf accessType
      `Prelude.seq` Prelude.rnf secretsManagerAccessTokenConfiguration

instance Data.ToJSON AccessConfiguration where
  toJSON AccessConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessType" Data..=) Prelude.<$> accessType,
            ("SecretsManagerAccessTokenConfiguration" Data..=)
              Prelude.<$> secretsManagerAccessTokenConfiguration
          ]
      )
