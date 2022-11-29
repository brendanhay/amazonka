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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about what Amazon S3 does when a multipart upload is
-- incomplete.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails = AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails'
  { -- | The number of days after which Amazon S3 cancels an incomplete multipart
    -- upload.
    daysAfterInitiation :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'daysAfterInitiation', 'awsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails_daysAfterInitiation' - The number of days after which Amazon S3 cancels an incomplete multipart
-- upload.
newAwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
newAwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails'
    { daysAfterInitiation =
        Prelude.Nothing
    }

-- | The number of days after which Amazon S3 cancels an incomplete multipart
-- upload.
awsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails_daysAfterInitiation :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails (Prelude.Maybe Prelude.Int)
awsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails_daysAfterInitiation = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' {daysAfterInitiation} -> daysAfterInitiation) (\s@AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' {} a -> s {daysAfterInitiation = a} :: AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails)

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails'
            Prelude.<$> (x Core..:? "DaysAfterInitiation")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' {..} =
      _salt `Prelude.hashWithSalt` daysAfterInitiation

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' {..} =
      Prelude.rnf daysAfterInitiation

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("DaysAfterInitiation" Core..=)
                Prelude.<$> daysAfterInitiation
            ]
        )
