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
-- Module      : Amazonka.ConnectCases.Types.SearchRelatedItemsResponseItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.SearchRelatedItemsResponseItem where

import Amazonka.ConnectCases.Types.RelatedItemContent
import Amazonka.ConnectCases.Types.RelatedItemType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of items that represent RelatedItems.
--
-- /See:/ 'newSearchRelatedItemsResponseItem' smart constructor.
data SearchRelatedItemsResponseItem = SearchRelatedItemsResponseItem'
  { -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Time at which a related item was associated with a case.
    associationTime :: Data.ISO8601,
    -- | Represents the content of a particular type of related item.
    content :: RelatedItemContent,
    -- | Unique identifier of a related item.
    relatedItemId :: Prelude.Text,
    -- | Type of a related item.
    type' :: RelatedItemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRelatedItemsResponseItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'searchRelatedItemsResponseItem_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'associationTime', 'searchRelatedItemsResponseItem_associationTime' - Time at which a related item was associated with a case.
--
-- 'content', 'searchRelatedItemsResponseItem_content' - Represents the content of a particular type of related item.
--
-- 'relatedItemId', 'searchRelatedItemsResponseItem_relatedItemId' - Unique identifier of a related item.
--
-- 'type'', 'searchRelatedItemsResponseItem_type' - Type of a related item.
newSearchRelatedItemsResponseItem ::
  -- | 'associationTime'
  Prelude.UTCTime ->
  -- | 'content'
  RelatedItemContent ->
  -- | 'relatedItemId'
  Prelude.Text ->
  -- | 'type''
  RelatedItemType ->
  SearchRelatedItemsResponseItem
newSearchRelatedItemsResponseItem
  pAssociationTime_
  pContent_
  pRelatedItemId_
  pType_ =
    SearchRelatedItemsResponseItem'
      { tags =
          Prelude.Nothing,
        associationTime =
          Data._Time Lens.# pAssociationTime_,
        content = pContent_,
        relatedItemId = pRelatedItemId_,
        type' = pType_
      }

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
searchRelatedItemsResponseItem_tags :: Lens.Lens' SearchRelatedItemsResponseItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
searchRelatedItemsResponseItem_tags = Lens.lens (\SearchRelatedItemsResponseItem' {tags} -> tags) (\s@SearchRelatedItemsResponseItem' {} a -> s {tags = a} :: SearchRelatedItemsResponseItem) Prelude.. Lens.mapping Lens.coerced

-- | Time at which a related item was associated with a case.
searchRelatedItemsResponseItem_associationTime :: Lens.Lens' SearchRelatedItemsResponseItem Prelude.UTCTime
searchRelatedItemsResponseItem_associationTime = Lens.lens (\SearchRelatedItemsResponseItem' {associationTime} -> associationTime) (\s@SearchRelatedItemsResponseItem' {} a -> s {associationTime = a} :: SearchRelatedItemsResponseItem) Prelude.. Data._Time

-- | Represents the content of a particular type of related item.
searchRelatedItemsResponseItem_content :: Lens.Lens' SearchRelatedItemsResponseItem RelatedItemContent
searchRelatedItemsResponseItem_content = Lens.lens (\SearchRelatedItemsResponseItem' {content} -> content) (\s@SearchRelatedItemsResponseItem' {} a -> s {content = a} :: SearchRelatedItemsResponseItem)

-- | Unique identifier of a related item.
searchRelatedItemsResponseItem_relatedItemId :: Lens.Lens' SearchRelatedItemsResponseItem Prelude.Text
searchRelatedItemsResponseItem_relatedItemId = Lens.lens (\SearchRelatedItemsResponseItem' {relatedItemId} -> relatedItemId) (\s@SearchRelatedItemsResponseItem' {} a -> s {relatedItemId = a} :: SearchRelatedItemsResponseItem)

-- | Type of a related item.
searchRelatedItemsResponseItem_type :: Lens.Lens' SearchRelatedItemsResponseItem RelatedItemType
searchRelatedItemsResponseItem_type = Lens.lens (\SearchRelatedItemsResponseItem' {type'} -> type') (\s@SearchRelatedItemsResponseItem' {} a -> s {type' = a} :: SearchRelatedItemsResponseItem)

instance Data.FromJSON SearchRelatedItemsResponseItem where
  parseJSON =
    Data.withObject
      "SearchRelatedItemsResponseItem"
      ( \x ->
          SearchRelatedItemsResponseItem'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "associationTime")
            Prelude.<*> (x Data..: "content")
            Prelude.<*> (x Data..: "relatedItemId")
            Prelude.<*> (x Data..: "type")
      )

instance
  Prelude.Hashable
    SearchRelatedItemsResponseItem
  where
  hashWithSalt
    _salt
    SearchRelatedItemsResponseItem' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` associationTime
        `Prelude.hashWithSalt` content
        `Prelude.hashWithSalt` relatedItemId
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    SearchRelatedItemsResponseItem
  where
  rnf SearchRelatedItemsResponseItem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf associationTime
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf relatedItemId
      `Prelude.seq` Prelude.rnf type'
