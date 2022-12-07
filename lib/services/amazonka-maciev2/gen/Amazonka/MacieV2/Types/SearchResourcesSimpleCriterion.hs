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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesSimpleCriterion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesSimpleCriterion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SearchResourcesComparator
import Amazonka.MacieV2.Types.SearchResourcesSimpleCriterionKey
import qualified Amazonka.Prelude as Prelude

-- | Specifies a property-based filter condition that determines which Amazon
-- Web Services resources are included or excluded from the query results.
--
-- /See:/ 'newSearchResourcesSimpleCriterion' smart constructor.
data SearchResourcesSimpleCriterion = SearchResourcesSimpleCriterion'
  { -- | The property to use in the condition.
    key :: Prelude.Maybe SearchResourcesSimpleCriterionKey,
    -- | The operator to use in the condition. Valid values are EQ (equals) and
    -- NE (not equals).
    comparator :: Prelude.Maybe SearchResourcesComparator,
    -- | An array that lists one or more values to use in the condition. If you
    -- specify multiple values, Amazon Macie uses OR logic to join the values.
    -- Valid values for each supported property (key) are:
    --
    -- -   ACCOUNT_ID - A string that represents the unique identifier for the
    --     Amazon Web Services account that owns the resource.
    --
    -- -   S3_BUCKET_EFFECTIVE_PERMISSION - A string that represents an
    --     enumerated value that Macie defines for the
    --     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketpublicaccess-effectivepermission BucketPublicAccess.effectivePermission>
    --     property of an S3 bucket.
    --
    -- -   S3_BUCKET_NAME - A string that represents the name of an S3 bucket.
    --
    -- -   S3_BUCKET_SHARED_ACCESS - A string that represents an enumerated
    --     value that Macie defines for the
    --     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketmetadata-sharedaccess BucketMetadata.sharedAccess>
    --     property of an S3 bucket.
    --
    -- Values are case sensitive. Also, Macie doesn\'t support use of partial
    -- values or wildcard characters in values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesSimpleCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'searchResourcesSimpleCriterion_key' - The property to use in the condition.
--
-- 'comparator', 'searchResourcesSimpleCriterion_comparator' - The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
--
-- 'values', 'searchResourcesSimpleCriterion_values' - An array that lists one or more values to use in the condition. If you
-- specify multiple values, Amazon Macie uses OR logic to join the values.
-- Valid values for each supported property (key) are:
--
-- -   ACCOUNT_ID - A string that represents the unique identifier for the
--     Amazon Web Services account that owns the resource.
--
-- -   S3_BUCKET_EFFECTIVE_PERMISSION - A string that represents an
--     enumerated value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketpublicaccess-effectivepermission BucketPublicAccess.effectivePermission>
--     property of an S3 bucket.
--
-- -   S3_BUCKET_NAME - A string that represents the name of an S3 bucket.
--
-- -   S3_BUCKET_SHARED_ACCESS - A string that represents an enumerated
--     value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketmetadata-sharedaccess BucketMetadata.sharedAccess>
--     property of an S3 bucket.
--
-- Values are case sensitive. Also, Macie doesn\'t support use of partial
-- values or wildcard characters in values.
newSearchResourcesSimpleCriterion ::
  SearchResourcesSimpleCriterion
newSearchResourcesSimpleCriterion =
  SearchResourcesSimpleCriterion'
    { key =
        Prelude.Nothing,
      comparator = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The property to use in the condition.
searchResourcesSimpleCriterion_key :: Lens.Lens' SearchResourcesSimpleCriterion (Prelude.Maybe SearchResourcesSimpleCriterionKey)
searchResourcesSimpleCriterion_key = Lens.lens (\SearchResourcesSimpleCriterion' {key} -> key) (\s@SearchResourcesSimpleCriterion' {} a -> s {key = a} :: SearchResourcesSimpleCriterion)

-- | The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
searchResourcesSimpleCriterion_comparator :: Lens.Lens' SearchResourcesSimpleCriterion (Prelude.Maybe SearchResourcesComparator)
searchResourcesSimpleCriterion_comparator = Lens.lens (\SearchResourcesSimpleCriterion' {comparator} -> comparator) (\s@SearchResourcesSimpleCriterion' {} a -> s {comparator = a} :: SearchResourcesSimpleCriterion)

-- | An array that lists one or more values to use in the condition. If you
-- specify multiple values, Amazon Macie uses OR logic to join the values.
-- Valid values for each supported property (key) are:
--
-- -   ACCOUNT_ID - A string that represents the unique identifier for the
--     Amazon Web Services account that owns the resource.
--
-- -   S3_BUCKET_EFFECTIVE_PERMISSION - A string that represents an
--     enumerated value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketpublicaccess-effectivepermission BucketPublicAccess.effectivePermission>
--     property of an S3 bucket.
--
-- -   S3_BUCKET_NAME - A string that represents the name of an S3 bucket.
--
-- -   S3_BUCKET_SHARED_ACCESS - A string that represents an enumerated
--     value that Macie defines for the
--     <https://docs.aws.amazon.com/macie/latest/APIReference/datasources-s3.html#datasources-s3-prop-bucketmetadata-sharedaccess BucketMetadata.sharedAccess>
--     property of an S3 bucket.
--
-- Values are case sensitive. Also, Macie doesn\'t support use of partial
-- values or wildcard characters in values.
searchResourcesSimpleCriterion_values :: Lens.Lens' SearchResourcesSimpleCriterion (Prelude.Maybe [Prelude.Text])
searchResourcesSimpleCriterion_values = Lens.lens (\SearchResourcesSimpleCriterion' {values} -> values) (\s@SearchResourcesSimpleCriterion' {} a -> s {values = a} :: SearchResourcesSimpleCriterion) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SearchResourcesSimpleCriterion
  where
  hashWithSalt
    _salt
    SearchResourcesSimpleCriterion' {..} =
      _salt `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` comparator
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    SearchResourcesSimpleCriterion
  where
  rnf SearchResourcesSimpleCriterion' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SearchResourcesSimpleCriterion where
  toJSON SearchResourcesSimpleCriterion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("comparator" Data..=) Prelude.<$> comparator,
            ("values" Data..=) Prelude.<$> values
          ]
      )
