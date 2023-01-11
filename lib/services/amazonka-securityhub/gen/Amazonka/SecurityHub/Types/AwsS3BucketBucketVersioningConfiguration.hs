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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketVersioningConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketVersioningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the versioning state of an S3 bucket.
--
-- /See:/ 'newAwsS3BucketBucketVersioningConfiguration' smart constructor.
data AwsS3BucketBucketVersioningConfiguration = AwsS3BucketBucketVersioningConfiguration'
  { -- | Specifies whether MFA delete is currently enabled in the S3 bucket
    -- versioning configuration. If the S3 bucket was never configured with MFA
    -- delete, then this attribute is not included.
    isMfaDeleteEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The versioning status of the S3 bucket. Valid values are @Enabled@ or
    -- @Suspended@.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketVersioningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMfaDeleteEnabled', 'awsS3BucketBucketVersioningConfiguration_isMfaDeleteEnabled' - Specifies whether MFA delete is currently enabled in the S3 bucket
-- versioning configuration. If the S3 bucket was never configured with MFA
-- delete, then this attribute is not included.
--
-- 'status', 'awsS3BucketBucketVersioningConfiguration_status' - The versioning status of the S3 bucket. Valid values are @Enabled@ or
-- @Suspended@.
newAwsS3BucketBucketVersioningConfiguration ::
  AwsS3BucketBucketVersioningConfiguration
newAwsS3BucketBucketVersioningConfiguration =
  AwsS3BucketBucketVersioningConfiguration'
    { isMfaDeleteEnabled =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies whether MFA delete is currently enabled in the S3 bucket
-- versioning configuration. If the S3 bucket was never configured with MFA
-- delete, then this attribute is not included.
awsS3BucketBucketVersioningConfiguration_isMfaDeleteEnabled :: Lens.Lens' AwsS3BucketBucketVersioningConfiguration (Prelude.Maybe Prelude.Bool)
awsS3BucketBucketVersioningConfiguration_isMfaDeleteEnabled = Lens.lens (\AwsS3BucketBucketVersioningConfiguration' {isMfaDeleteEnabled} -> isMfaDeleteEnabled) (\s@AwsS3BucketBucketVersioningConfiguration' {} a -> s {isMfaDeleteEnabled = a} :: AwsS3BucketBucketVersioningConfiguration)

-- | The versioning status of the S3 bucket. Valid values are @Enabled@ or
-- @Suspended@.
awsS3BucketBucketVersioningConfiguration_status :: Lens.Lens' AwsS3BucketBucketVersioningConfiguration (Prelude.Maybe Prelude.Text)
awsS3BucketBucketVersioningConfiguration_status = Lens.lens (\AwsS3BucketBucketVersioningConfiguration' {status} -> status) (\s@AwsS3BucketBucketVersioningConfiguration' {} a -> s {status = a} :: AwsS3BucketBucketVersioningConfiguration)

instance
  Data.FromJSON
    AwsS3BucketBucketVersioningConfiguration
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketBucketVersioningConfiguration"
      ( \x ->
          AwsS3BucketBucketVersioningConfiguration'
            Prelude.<$> (x Data..:? "IsMfaDeleteEnabled")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketVersioningConfiguration
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketVersioningConfiguration' {..} =
      _salt `Prelude.hashWithSalt` isMfaDeleteEnabled
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsS3BucketBucketVersioningConfiguration
  where
  rnf AwsS3BucketBucketVersioningConfiguration' {..} =
    Prelude.rnf isMfaDeleteEnabled
      `Prelude.seq` Prelude.rnf status

instance
  Data.ToJSON
    AwsS3BucketBucketVersioningConfiguration
  where
  toJSON AwsS3BucketBucketVersioningConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsMfaDeleteEnabled" Data..=)
              Prelude.<$> isMfaDeleteEnabled,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
