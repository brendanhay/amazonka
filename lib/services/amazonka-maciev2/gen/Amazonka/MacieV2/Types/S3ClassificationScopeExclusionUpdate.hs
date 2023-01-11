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
-- Module      : Amazonka.MacieV2.Types.S3ClassificationScopeExclusionUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3ClassificationScopeExclusionUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.ClassificationScopeUpdateOperation
import qualified Amazonka.Prelude as Prelude

-- | Specifies S3 buckets to add or remove from the exclusion list defined by
-- the classification scope for an Amazon Macie account.
--
-- /See:/ 'newS3ClassificationScopeExclusionUpdate' smart constructor.
data S3ClassificationScopeExclusionUpdate = S3ClassificationScopeExclusionUpdate'
  { -- | Depending on the value specified for the update operation
    -- (ClassificationScopeUpdateOperation), an array of strings that: lists
    -- the names of buckets to add or remove from the list, or specifies a new
    -- set of bucket names that overwrites all existing names in the list. Each
    -- string must be the full name of an S3 bucket. Values are case sensitive.
    bucketNames :: [Prelude.Text],
    -- | Specifies how to apply the changes to the exclusion list. Valid values
    -- are:
    --
    -- -   ADD - Append the specified bucket names to the current list.
    --
    -- -   REMOVE - Remove the specified bucket names from the current list.
    --
    -- -   REPLACE - Overwrite the current list with the specified list of
    --     bucket names. If you specify this value, Amazon Macie removes all
    --     existing names from the list and adds all the specified names to the
    --     list.
    operation :: ClassificationScopeUpdateOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ClassificationScopeExclusionUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketNames', 's3ClassificationScopeExclusionUpdate_bucketNames' - Depending on the value specified for the update operation
-- (ClassificationScopeUpdateOperation), an array of strings that: lists
-- the names of buckets to add or remove from the list, or specifies a new
-- set of bucket names that overwrites all existing names in the list. Each
-- string must be the full name of an S3 bucket. Values are case sensitive.
--
-- 'operation', 's3ClassificationScopeExclusionUpdate_operation' - Specifies how to apply the changes to the exclusion list. Valid values
-- are:
--
-- -   ADD - Append the specified bucket names to the current list.
--
-- -   REMOVE - Remove the specified bucket names from the current list.
--
-- -   REPLACE - Overwrite the current list with the specified list of
--     bucket names. If you specify this value, Amazon Macie removes all
--     existing names from the list and adds all the specified names to the
--     list.
newS3ClassificationScopeExclusionUpdate ::
  -- | 'operation'
  ClassificationScopeUpdateOperation ->
  S3ClassificationScopeExclusionUpdate
newS3ClassificationScopeExclusionUpdate pOperation_ =
  S3ClassificationScopeExclusionUpdate'
    { bucketNames =
        Prelude.mempty,
      operation = pOperation_
    }

-- | Depending on the value specified for the update operation
-- (ClassificationScopeUpdateOperation), an array of strings that: lists
-- the names of buckets to add or remove from the list, or specifies a new
-- set of bucket names that overwrites all existing names in the list. Each
-- string must be the full name of an S3 bucket. Values are case sensitive.
s3ClassificationScopeExclusionUpdate_bucketNames :: Lens.Lens' S3ClassificationScopeExclusionUpdate [Prelude.Text]
s3ClassificationScopeExclusionUpdate_bucketNames = Lens.lens (\S3ClassificationScopeExclusionUpdate' {bucketNames} -> bucketNames) (\s@S3ClassificationScopeExclusionUpdate' {} a -> s {bucketNames = a} :: S3ClassificationScopeExclusionUpdate) Prelude.. Lens.coerced

-- | Specifies how to apply the changes to the exclusion list. Valid values
-- are:
--
-- -   ADD - Append the specified bucket names to the current list.
--
-- -   REMOVE - Remove the specified bucket names from the current list.
--
-- -   REPLACE - Overwrite the current list with the specified list of
--     bucket names. If you specify this value, Amazon Macie removes all
--     existing names from the list and adds all the specified names to the
--     list.
s3ClassificationScopeExclusionUpdate_operation :: Lens.Lens' S3ClassificationScopeExclusionUpdate ClassificationScopeUpdateOperation
s3ClassificationScopeExclusionUpdate_operation = Lens.lens (\S3ClassificationScopeExclusionUpdate' {operation} -> operation) (\s@S3ClassificationScopeExclusionUpdate' {} a -> s {operation = a} :: S3ClassificationScopeExclusionUpdate)

instance
  Prelude.Hashable
    S3ClassificationScopeExclusionUpdate
  where
  hashWithSalt
    _salt
    S3ClassificationScopeExclusionUpdate' {..} =
      _salt `Prelude.hashWithSalt` bucketNames
        `Prelude.hashWithSalt` operation

instance
  Prelude.NFData
    S3ClassificationScopeExclusionUpdate
  where
  rnf S3ClassificationScopeExclusionUpdate' {..} =
    Prelude.rnf bucketNames
      `Prelude.seq` Prelude.rnf operation

instance
  Data.ToJSON
    S3ClassificationScopeExclusionUpdate
  where
  toJSON S3ClassificationScopeExclusionUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucketNames" Data..= bucketNames),
            Prelude.Just ("operation" Data..= operation)
          ]
      )
