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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A transition rule that describes when noncurrent objects transition to a
-- specified storage class.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails = AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails'
  { -- | The number of days that an object is noncurrent before Amazon S3 can
    -- perform the associated action.
    days :: Prelude.Maybe Prelude.Int,
    -- | The class of storage to change the object to after the object is
    -- noncurrent for the specified number of days.
    storageClass :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_days' - The number of days that an object is noncurrent before Amazon S3 can
-- perform the associated action.
--
-- 'storageClass', 'awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_storageClass' - The class of storage to change the object to after the object is
-- noncurrent for the specified number of days.
newAwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
newAwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails'
    { days =
        Prelude.Nothing,
      storageClass =
        Prelude.Nothing
    }

-- | The number of days that an object is noncurrent before Amazon S3 can
-- perform the associated action.
awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_days :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails (Prelude.Maybe Prelude.Int)
awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_days = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {days} -> days) (\s@AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {} a -> s {days = a} :: AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails)

-- | The class of storage to change the object to after the object is
-- noncurrent for the specified number of days.
awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_storageClass :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_storageClass = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {storageClass} -> storageClass) (\s@AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {} a -> s {storageClass = a} :: AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails)

instance
  Data.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails'
            Prelude.<$> (x Data..:? "Days")
              Prelude.<*> (x Data..:? "StorageClass")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {..} =
      _salt `Prelude.hashWithSalt` days
        `Prelude.hashWithSalt` storageClass

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {..} =
      Prelude.rnf days
        `Prelude.seq` Prelude.rnf storageClass

instance
  Data.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Days" Data..=) Prelude.<$> days,
              ("StorageClass" Data..=) Prelude.<$> storageClass
            ]
        )
