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
-- Module      : Amazonka.MacieV2.Types.SimpleCriterionForJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SimpleCriterionForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.SimpleCriterionKeyForJob
import qualified Amazonka.Prelude as Prelude

-- | Specifies a property-based condition that determines whether an S3
-- bucket is included or excluded from a classification job.
--
-- /See:/ 'newSimpleCriterionForJob' smart constructor.
data SimpleCriterionForJob = SimpleCriterionForJob'
  { -- | The operator to use in the condition. Valid values are EQ (equals) and
    -- NE (not equals).
    comparator :: Prelude.Maybe JobComparator,
    -- | The property to use in the condition.
    key :: Prelude.Maybe SimpleCriterionKeyForJob,
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
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'comparator', 'simpleCriterionForJob_comparator' - The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
--
-- 'key', 'simpleCriterionForJob_key' - The property to use in the condition.
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
newSimpleCriterionForJob ::
  SimpleCriterionForJob
newSimpleCriterionForJob =
  SimpleCriterionForJob'
    { comparator =
        Prelude.Nothing,
      key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
simpleCriterionForJob_comparator :: Lens.Lens' SimpleCriterionForJob (Prelude.Maybe JobComparator)
simpleCriterionForJob_comparator = Lens.lens (\SimpleCriterionForJob' {comparator} -> comparator) (\s@SimpleCriterionForJob' {} a -> s {comparator = a} :: SimpleCriterionForJob)

-- | The property to use in the condition.
simpleCriterionForJob_key :: Lens.Lens' SimpleCriterionForJob (Prelude.Maybe SimpleCriterionKeyForJob)
simpleCriterionForJob_key = Lens.lens (\SimpleCriterionForJob' {key} -> key) (\s@SimpleCriterionForJob' {} a -> s {key = a} :: SimpleCriterionForJob)

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

instance Data.FromJSON SimpleCriterionForJob where
  parseJSON =
    Data.withObject
      "SimpleCriterionForJob"
      ( \x ->
          SimpleCriterionForJob'
            Prelude.<$> (x Data..:? "comparator")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SimpleCriterionForJob where
  hashWithSalt _salt SimpleCriterionForJob' {..} =
    _salt
      `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData SimpleCriterionForJob where
  rnf SimpleCriterionForJob' {..} =
    Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SimpleCriterionForJob where
  toJSON SimpleCriterionForJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comparator" Data..=) Prelude.<$> comparator,
            ("key" Data..=) Prelude.<$> key,
            ("values" Data..=) Prelude.<$> values
          ]
      )
