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
-- Module      : Amazonka.WorkDocs.Types.Filters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.Filters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ContentCategoryType
import Amazonka.WorkDocs.Types.DateRangeType
import Amazonka.WorkDocs.Types.LanguageCodeType
import Amazonka.WorkDocs.Types.LongRangeType
import Amazonka.WorkDocs.Types.SearchCollectionType
import Amazonka.WorkDocs.Types.SearchPrincipalType
import Amazonka.WorkDocs.Types.SearchResourceType

-- | Filters results based on entity metadata.
--
-- /See:/ 'newFilters' smart constructor.
data Filters = Filters'
  { -- | Filter based on resource’s path.
    ancestorIds :: Prelude.Maybe [Prelude.Text],
    -- | Filters by content category.
    contentCategories :: Prelude.Maybe [ContentCategoryType],
    -- | Filter based on resource’s creation timestamp.
    createdRange :: Prelude.Maybe DateRangeType,
    -- | Filter by labels using exact match.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | Filter based on resource’s modified timestamp.
    modifiedRange :: Prelude.Maybe DateRangeType,
    -- | Filter based on UserIds or GroupIds.
    principals :: Prelude.Maybe [SearchPrincipalType],
    -- | Filters based on entity type.
    resourceTypes :: Prelude.Maybe [SearchResourceType],
    -- | Filter based on file groupings.
    searchCollectionTypes :: Prelude.Maybe [SearchCollectionType],
    -- | Filter based on size (in bytes).
    sizeRange :: Prelude.Maybe LongRangeType,
    -- | Filters by the locale of the content or comment.
    textLocales :: Prelude.Maybe [LanguageCodeType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ancestorIds', 'filters_ancestorIds' - Filter based on resource’s path.
--
-- 'contentCategories', 'filters_contentCategories' - Filters by content category.
--
-- 'createdRange', 'filters_createdRange' - Filter based on resource’s creation timestamp.
--
-- 'labels', 'filters_labels' - Filter by labels using exact match.
--
-- 'modifiedRange', 'filters_modifiedRange' - Filter based on resource’s modified timestamp.
--
-- 'principals', 'filters_principals' - Filter based on UserIds or GroupIds.
--
-- 'resourceTypes', 'filters_resourceTypes' - Filters based on entity type.
--
-- 'searchCollectionTypes', 'filters_searchCollectionTypes' - Filter based on file groupings.
--
-- 'sizeRange', 'filters_sizeRange' - Filter based on size (in bytes).
--
-- 'textLocales', 'filters_textLocales' - Filters by the locale of the content or comment.
newFilters ::
  Filters
newFilters =
  Filters'
    { ancestorIds = Prelude.Nothing,
      contentCategories = Prelude.Nothing,
      createdRange = Prelude.Nothing,
      labels = Prelude.Nothing,
      modifiedRange = Prelude.Nothing,
      principals = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      searchCollectionTypes = Prelude.Nothing,
      sizeRange = Prelude.Nothing,
      textLocales = Prelude.Nothing
    }

-- | Filter based on resource’s path.
filters_ancestorIds :: Lens.Lens' Filters (Prelude.Maybe [Prelude.Text])
filters_ancestorIds = Lens.lens (\Filters' {ancestorIds} -> ancestorIds) (\s@Filters' {} a -> s {ancestorIds = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | Filters by content category.
filters_contentCategories :: Lens.Lens' Filters (Prelude.Maybe [ContentCategoryType])
filters_contentCategories = Lens.lens (\Filters' {contentCategories} -> contentCategories) (\s@Filters' {} a -> s {contentCategories = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | Filter based on resource’s creation timestamp.
filters_createdRange :: Lens.Lens' Filters (Prelude.Maybe DateRangeType)
filters_createdRange = Lens.lens (\Filters' {createdRange} -> createdRange) (\s@Filters' {} a -> s {createdRange = a} :: Filters)

-- | Filter by labels using exact match.
filters_labels :: Lens.Lens' Filters (Prelude.Maybe [Prelude.Text])
filters_labels = Lens.lens (\Filters' {labels} -> labels) (\s@Filters' {} a -> s {labels = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | Filter based on resource’s modified timestamp.
filters_modifiedRange :: Lens.Lens' Filters (Prelude.Maybe DateRangeType)
filters_modifiedRange = Lens.lens (\Filters' {modifiedRange} -> modifiedRange) (\s@Filters' {} a -> s {modifiedRange = a} :: Filters)

-- | Filter based on UserIds or GroupIds.
filters_principals :: Lens.Lens' Filters (Prelude.Maybe [SearchPrincipalType])
filters_principals = Lens.lens (\Filters' {principals} -> principals) (\s@Filters' {} a -> s {principals = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | Filters based on entity type.
filters_resourceTypes :: Lens.Lens' Filters (Prelude.Maybe [SearchResourceType])
filters_resourceTypes = Lens.lens (\Filters' {resourceTypes} -> resourceTypes) (\s@Filters' {} a -> s {resourceTypes = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | Filter based on file groupings.
filters_searchCollectionTypes :: Lens.Lens' Filters (Prelude.Maybe [SearchCollectionType])
filters_searchCollectionTypes = Lens.lens (\Filters' {searchCollectionTypes} -> searchCollectionTypes) (\s@Filters' {} a -> s {searchCollectionTypes = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

-- | Filter based on size (in bytes).
filters_sizeRange :: Lens.Lens' Filters (Prelude.Maybe LongRangeType)
filters_sizeRange = Lens.lens (\Filters' {sizeRange} -> sizeRange) (\s@Filters' {} a -> s {sizeRange = a} :: Filters)

-- | Filters by the locale of the content or comment.
filters_textLocales :: Lens.Lens' Filters (Prelude.Maybe [LanguageCodeType])
filters_textLocales = Lens.lens (\Filters' {textLocales} -> textLocales) (\s@Filters' {} a -> s {textLocales = a} :: Filters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Filters where
  hashWithSalt _salt Filters' {..} =
    _salt
      `Prelude.hashWithSalt` ancestorIds
      `Prelude.hashWithSalt` contentCategories
      `Prelude.hashWithSalt` createdRange
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` modifiedRange
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` searchCollectionTypes
      `Prelude.hashWithSalt` sizeRange
      `Prelude.hashWithSalt` textLocales

instance Prelude.NFData Filters where
  rnf Filters' {..} =
    Prelude.rnf ancestorIds
      `Prelude.seq` Prelude.rnf contentCategories
      `Prelude.seq` Prelude.rnf createdRange
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf modifiedRange
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf searchCollectionTypes
      `Prelude.seq` Prelude.rnf sizeRange
      `Prelude.seq` Prelude.rnf textLocales

instance Data.ToJSON Filters where
  toJSON Filters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AncestorIds" Data..=) Prelude.<$> ancestorIds,
            ("ContentCategories" Data..=)
              Prelude.<$> contentCategories,
            ("CreatedRange" Data..=) Prelude.<$> createdRange,
            ("Labels" Data..=) Prelude.<$> labels,
            ("ModifiedRange" Data..=) Prelude.<$> modifiedRange,
            ("Principals" Data..=) Prelude.<$> principals,
            ("ResourceTypes" Data..=) Prelude.<$> resourceTypes,
            ("SearchCollectionTypes" Data..=)
              Prelude.<$> searchCollectionTypes,
            ("SizeRange" Data..=) Prelude.<$> sizeRange,
            ("TextLocales" Data..=) Prelude.<$> textLocales
          ]
      )
