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
-- Module      : Network.AWS.Kendra.Types.QuerySuggestionsBlockListSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.QuerySuggestionsBlockListSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.QuerySuggestionsBlockListStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information on a query suggestions block list.
--
-- This includes information on the block list ID, block list name, when
-- the block list was created, when the block list was last updated, and
-- the count of block words\/phrases in the block list.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- /See:/ 'newQuerySuggestionsBlockListSummary' smart constructor.
data QuerySuggestionsBlockListSummary = QuerySuggestionsBlockListSummary'
  { -- | The status of the block list.
    status :: Prelude.Maybe QuerySuggestionsBlockListStatus,
    -- | The date-time summary information for a query suggestions block list was
    -- last created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the block list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a block list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date-time the block list was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The number of items in the block list file.
    itemCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuerySuggestionsBlockListSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'querySuggestionsBlockListSummary_status' - The status of the block list.
--
-- 'createdAt', 'querySuggestionsBlockListSummary_createdAt' - The date-time summary information for a query suggestions block list was
-- last created.
--
-- 'name', 'querySuggestionsBlockListSummary_name' - The name of the block list.
--
-- 'id', 'querySuggestionsBlockListSummary_id' - The identifier of a block list.
--
-- 'updatedAt', 'querySuggestionsBlockListSummary_updatedAt' - The date-time the block list was last updated.
--
-- 'itemCount', 'querySuggestionsBlockListSummary_itemCount' - The number of items in the block list file.
newQuerySuggestionsBlockListSummary ::
  QuerySuggestionsBlockListSummary
newQuerySuggestionsBlockListSummary =
  QuerySuggestionsBlockListSummary'
    { status =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      itemCount = Prelude.Nothing
    }

-- | The status of the block list.
querySuggestionsBlockListSummary_status :: Lens.Lens' QuerySuggestionsBlockListSummary (Prelude.Maybe QuerySuggestionsBlockListStatus)
querySuggestionsBlockListSummary_status = Lens.lens (\QuerySuggestionsBlockListSummary' {status} -> status) (\s@QuerySuggestionsBlockListSummary' {} a -> s {status = a} :: QuerySuggestionsBlockListSummary)

-- | The date-time summary information for a query suggestions block list was
-- last created.
querySuggestionsBlockListSummary_createdAt :: Lens.Lens' QuerySuggestionsBlockListSummary (Prelude.Maybe Prelude.UTCTime)
querySuggestionsBlockListSummary_createdAt = Lens.lens (\QuerySuggestionsBlockListSummary' {createdAt} -> createdAt) (\s@QuerySuggestionsBlockListSummary' {} a -> s {createdAt = a} :: QuerySuggestionsBlockListSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the block list.
querySuggestionsBlockListSummary_name :: Lens.Lens' QuerySuggestionsBlockListSummary (Prelude.Maybe Prelude.Text)
querySuggestionsBlockListSummary_name = Lens.lens (\QuerySuggestionsBlockListSummary' {name} -> name) (\s@QuerySuggestionsBlockListSummary' {} a -> s {name = a} :: QuerySuggestionsBlockListSummary)

-- | The identifier of a block list.
querySuggestionsBlockListSummary_id :: Lens.Lens' QuerySuggestionsBlockListSummary (Prelude.Maybe Prelude.Text)
querySuggestionsBlockListSummary_id = Lens.lens (\QuerySuggestionsBlockListSummary' {id} -> id) (\s@QuerySuggestionsBlockListSummary' {} a -> s {id = a} :: QuerySuggestionsBlockListSummary)

-- | The date-time the block list was last updated.
querySuggestionsBlockListSummary_updatedAt :: Lens.Lens' QuerySuggestionsBlockListSummary (Prelude.Maybe Prelude.UTCTime)
querySuggestionsBlockListSummary_updatedAt = Lens.lens (\QuerySuggestionsBlockListSummary' {updatedAt} -> updatedAt) (\s@QuerySuggestionsBlockListSummary' {} a -> s {updatedAt = a} :: QuerySuggestionsBlockListSummary) Prelude.. Lens.mapping Core._Time

-- | The number of items in the block list file.
querySuggestionsBlockListSummary_itemCount :: Lens.Lens' QuerySuggestionsBlockListSummary (Prelude.Maybe Prelude.Int)
querySuggestionsBlockListSummary_itemCount = Lens.lens (\QuerySuggestionsBlockListSummary' {itemCount} -> itemCount) (\s@QuerySuggestionsBlockListSummary' {} a -> s {itemCount = a} :: QuerySuggestionsBlockListSummary)

instance
  Core.FromJSON
    QuerySuggestionsBlockListSummary
  where
  parseJSON =
    Core.withObject
      "QuerySuggestionsBlockListSummary"
      ( \x ->
          QuerySuggestionsBlockListSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "ItemCount")
      )

instance
  Prelude.Hashable
    QuerySuggestionsBlockListSummary

instance
  Prelude.NFData
    QuerySuggestionsBlockListSummary
