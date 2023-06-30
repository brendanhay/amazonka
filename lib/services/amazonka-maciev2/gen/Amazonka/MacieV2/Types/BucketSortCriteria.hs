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
-- Module      : Amazonka.MacieV2.Types.BucketSortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketSortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.OrderBy
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for sorting the results of a query for information
-- about S3 buckets.
--
-- /See:/ 'newBucketSortCriteria' smart constructor.
data BucketSortCriteria = BucketSortCriteria'
  { -- | The name of the bucket property to sort the results by. This value can
    -- be one of the following properties that Amazon Macie defines as bucket
    -- metadata: accountId, bucketName, classifiableObjectCount,
    -- classifiableSizeInBytes, objectCount, sensitivityScore, or sizeInBytes.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The sort order to apply to the results, based on the value specified by
    -- the attributeName property. Valid values are: ASC, sort the results in
    -- ascending order; and, DESC, sort the results in descending order.
    orderBy :: Prelude.Maybe OrderBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'bucketSortCriteria_attributeName' - The name of the bucket property to sort the results by. This value can
-- be one of the following properties that Amazon Macie defines as bucket
-- metadata: accountId, bucketName, classifiableObjectCount,
-- classifiableSizeInBytes, objectCount, sensitivityScore, or sizeInBytes.
--
-- 'orderBy', 'bucketSortCriteria_orderBy' - The sort order to apply to the results, based on the value specified by
-- the attributeName property. Valid values are: ASC, sort the results in
-- ascending order; and, DESC, sort the results in descending order.
newBucketSortCriteria ::
  BucketSortCriteria
newBucketSortCriteria =
  BucketSortCriteria'
    { attributeName =
        Prelude.Nothing,
      orderBy = Prelude.Nothing
    }

-- | The name of the bucket property to sort the results by. This value can
-- be one of the following properties that Amazon Macie defines as bucket
-- metadata: accountId, bucketName, classifiableObjectCount,
-- classifiableSizeInBytes, objectCount, sensitivityScore, or sizeInBytes.
bucketSortCriteria_attributeName :: Lens.Lens' BucketSortCriteria (Prelude.Maybe Prelude.Text)
bucketSortCriteria_attributeName = Lens.lens (\BucketSortCriteria' {attributeName} -> attributeName) (\s@BucketSortCriteria' {} a -> s {attributeName = a} :: BucketSortCriteria)

-- | The sort order to apply to the results, based on the value specified by
-- the attributeName property. Valid values are: ASC, sort the results in
-- ascending order; and, DESC, sort the results in descending order.
bucketSortCriteria_orderBy :: Lens.Lens' BucketSortCriteria (Prelude.Maybe OrderBy)
bucketSortCriteria_orderBy = Lens.lens (\BucketSortCriteria' {orderBy} -> orderBy) (\s@BucketSortCriteria' {} a -> s {orderBy = a} :: BucketSortCriteria)

instance Prelude.Hashable BucketSortCriteria where
  hashWithSalt _salt BucketSortCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData BucketSortCriteria where
  rnf BucketSortCriteria' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf orderBy

instance Data.ToJSON BucketSortCriteria where
  toJSON BucketSortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributeName" Data..=) Prelude.<$> attributeName,
            ("orderBy" Data..=) Prelude.<$> orderBy
          ]
      )
