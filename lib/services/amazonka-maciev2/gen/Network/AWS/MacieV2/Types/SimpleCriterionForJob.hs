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
-- Module      : Network.AWS.MacieV2.Types.SimpleCriterionForJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.SimpleCriterionForJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.JobComparator
import Network.AWS.MacieV2.Types.SimpleCriterionKeyForJob
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a property-based condition that determines whether an S3
-- bucket is included or excluded from a classification job.
--
-- /See:/ 'newSimpleCriterionForJob' smart constructor.
data SimpleCriterionForJob = SimpleCriterionForJob'
  { -- | An array that lists one or more values to use in the condition. If you
    -- specify multiple values, Amazon Macie uses OR logic to join the values.
    -- Valid values for each supported property (key) are:
    --
    -- -   ACCOUNT_ID - A string that represents the unique identifier for the
    --     Amazon Web Services account that owns the bucket.
    --
    -- -   S3_BUCKET_EFFECTIVE_PERMISSION - A string that represents an
    --     enumerated value that Macie defines for the
    --     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketpublicaccess-effectivepermission BucketPublicAccess.effectivePermission>
    --     property of a bucket.
    --
    -- -   S3_BUCKET_NAME - A string that represents the name of a bucket.
    --
    -- -   S3_BUCKET_SHARED_ACCESS - A string that represents an enumerated
    --     value that Macie defines for the
    --     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketmetadata-sharedaccess BucketMetadata.sharedAccess>
    --     property of a bucket.
    --
    -- Values are case sensitive. Also, Macie doesn\'t support use of partial
    -- values or wildcard characters in these values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The property to use in the condition.
    key :: Prelude.Maybe SimpleCriterionKeyForJob,
    -- | The operator to use in the condition. Valid values are EQ (equals) and
    -- NE (not equals).
    comparator :: Prelude.Maybe JobComparator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleCriterionForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'simpleCriterionForJob_values' - An array that lists one or more values to use in the condition. If you
-- specify multiple values, Amazon Macie uses OR logic to join the values.
-- Valid values for each supported property (key) are:
--
-- -   ACCOUNT_ID - A string that represents the unique identifier for the
--     Amazon Web Services account that owns the bucket.
--
-- -   S3_BUCKET_EFFECTIVE_PERMISSION - A string that represents an
--     enumerated value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketpublicaccess-effectivepermission BucketPublicAccess.effectivePermission>
--     property of a bucket.
--
-- -   S3_BUCKET_NAME - A string that represents the name of a bucket.
--
-- -   S3_BUCKET_SHARED_ACCESS - A string that represents an enumerated
--     value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketmetadata-sharedaccess BucketMetadata.sharedAccess>
--     property of a bucket.
--
-- Values are case sensitive. Also, Macie doesn\'t support use of partial
-- values or wildcard characters in these values.
--
-- 'key', 'simpleCriterionForJob_key' - The property to use in the condition.
--
-- 'comparator', 'simpleCriterionForJob_comparator' - The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
newSimpleCriterionForJob ::
  SimpleCriterionForJob
newSimpleCriterionForJob =
  SimpleCriterionForJob'
    { values = Prelude.Nothing,
      key = Prelude.Nothing,
      comparator = Prelude.Nothing
    }

-- | An array that lists one or more values to use in the condition. If you
-- specify multiple values, Amazon Macie uses OR logic to join the values.
-- Valid values for each supported property (key) are:
--
-- -   ACCOUNT_ID - A string that represents the unique identifier for the
--     Amazon Web Services account that owns the bucket.
--
-- -   S3_BUCKET_EFFECTIVE_PERMISSION - A string that represents an
--     enumerated value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketpublicaccess-effectivepermission BucketPublicAccess.effectivePermission>
--     property of a bucket.
--
-- -   S3_BUCKET_NAME - A string that represents the name of a bucket.
--
-- -   S3_BUCKET_SHARED_ACCESS - A string that represents an enumerated
--     value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketmetadata-sharedaccess BucketMetadata.sharedAccess>
--     property of a bucket.
--
-- Values are case sensitive. Also, Macie doesn\'t support use of partial
-- values or wildcard characters in these values.
simpleCriterionForJob_values :: Lens.Lens' SimpleCriterionForJob (Prelude.Maybe [Prelude.Text])
simpleCriterionForJob_values = Lens.lens (\SimpleCriterionForJob' {values} -> values) (\s@SimpleCriterionForJob' {} a -> s {values = a} :: SimpleCriterionForJob) Prelude.. Lens.mapping Lens.coerced

-- | The property to use in the condition.
simpleCriterionForJob_key :: Lens.Lens' SimpleCriterionForJob (Prelude.Maybe SimpleCriterionKeyForJob)
simpleCriterionForJob_key = Lens.lens (\SimpleCriterionForJob' {key} -> key) (\s@SimpleCriterionForJob' {} a -> s {key = a} :: SimpleCriterionForJob)

-- | The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
simpleCriterionForJob_comparator :: Lens.Lens' SimpleCriterionForJob (Prelude.Maybe JobComparator)
simpleCriterionForJob_comparator = Lens.lens (\SimpleCriterionForJob' {comparator} -> comparator) (\s@SimpleCriterionForJob' {} a -> s {comparator = a} :: SimpleCriterionForJob)

instance Core.FromJSON SimpleCriterionForJob where
  parseJSON =
    Core.withObject
      "SimpleCriterionForJob"
      ( \x ->
          SimpleCriterionForJob'
            Prelude.<$> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "key")
            Prelude.<*> (x Core..:? "comparator")
      )

instance Prelude.Hashable SimpleCriterionForJob

instance Prelude.NFData SimpleCriterionForJob

instance Core.ToJSON SimpleCriterionForJob where
  toJSON SimpleCriterionForJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("key" Core..=) Prelude.<$> key,
            ("comparator" Core..=) Prelude.<$> comparator
          ]
      )
