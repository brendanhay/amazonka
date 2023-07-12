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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilter

-- | Filtering information for the notifications. The filtering is based on
-- Amazon S3 key names.
--
-- /See:/ 'newAwsS3BucketNotificationConfigurationFilter' smart constructor.
data AwsS3BucketNotificationConfigurationFilter = AwsS3BucketNotificationConfigurationFilter'
  { -- | Details for an Amazon S3 filter.
    s3KeyFilter :: Prelude.Maybe AwsS3BucketNotificationConfigurationS3KeyFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketNotificationConfigurationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyFilter', 'awsS3BucketNotificationConfigurationFilter_s3KeyFilter' - Details for an Amazon S3 filter.
newAwsS3BucketNotificationConfigurationFilter ::
  AwsS3BucketNotificationConfigurationFilter
newAwsS3BucketNotificationConfigurationFilter =
  AwsS3BucketNotificationConfigurationFilter'
    { s3KeyFilter =
        Prelude.Nothing
    }

-- | Details for an Amazon S3 filter.
awsS3BucketNotificationConfigurationFilter_s3KeyFilter :: Lens.Lens' AwsS3BucketNotificationConfigurationFilter (Prelude.Maybe AwsS3BucketNotificationConfigurationS3KeyFilter)
awsS3BucketNotificationConfigurationFilter_s3KeyFilter = Lens.lens (\AwsS3BucketNotificationConfigurationFilter' {s3KeyFilter} -> s3KeyFilter) (\s@AwsS3BucketNotificationConfigurationFilter' {} a -> s {s3KeyFilter = a} :: AwsS3BucketNotificationConfigurationFilter)

instance
  Data.FromJSON
    AwsS3BucketNotificationConfigurationFilter
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketNotificationConfigurationFilter"
      ( \x ->
          AwsS3BucketNotificationConfigurationFilter'
            Prelude.<$> (x Data..:? "S3KeyFilter")
      )

instance
  Prelude.Hashable
    AwsS3BucketNotificationConfigurationFilter
  where
  hashWithSalt
    _salt
    AwsS3BucketNotificationConfigurationFilter' {..} =
      _salt `Prelude.hashWithSalt` s3KeyFilter

instance
  Prelude.NFData
    AwsS3BucketNotificationConfigurationFilter
  where
  rnf AwsS3BucketNotificationConfigurationFilter' {..} =
    Prelude.rnf s3KeyFilter

instance
  Data.ToJSON
    AwsS3BucketNotificationConfigurationFilter
  where
  toJSON
    AwsS3BucketNotificationConfigurationFilter' {..} =
      Data.object
        ( Prelude.catMaybes
            [("S3KeyFilter" Data..=) Prelude.<$> s3KeyFilter]
        )
