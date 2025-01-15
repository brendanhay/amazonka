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
-- Module      : Amazonka.Kendra.Types.SortingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SortingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Specifies the document attribute to use to sort the response to a Amazon
-- Kendra query. You can specify a single attribute for sorting. The
-- attribute must have the @Sortable@ flag set to @true@, otherwise Amazon
-- Kendra returns an exception.
--
-- You can sort attributes of the following types.
--
-- -   Date value
--
-- -   Long value
--
-- -   String value
--
-- You can\'t sort attributes of the following type.
--
-- -   String list value
--
-- /See:/ 'newSortingConfiguration' smart constructor.
data SortingConfiguration = SortingConfiguration'
  { -- | The name of the document attribute used to sort the response. You can
    -- use any field that has the @Sortable@ flag set to true.
    --
    -- You can also sort by any of the following built-in attributes:
    --
    -- -   _category
    --
    -- -   _created_at
    --
    -- -   _last_updated_at
    --
    -- -   _version
    --
    -- -   _view_count
    documentAttributeKey :: Prelude.Text,
    -- | The order that the results should be returned in. In case of ties, the
    -- relevance assigned to the result by Amazon Kendra is used as the
    -- tie-breaker.
    sortOrder :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentAttributeKey', 'sortingConfiguration_documentAttributeKey' - The name of the document attribute used to sort the response. You can
-- use any field that has the @Sortable@ flag set to true.
--
-- You can also sort by any of the following built-in attributes:
--
-- -   _category
--
-- -   _created_at
--
-- -   _last_updated_at
--
-- -   _version
--
-- -   _view_count
--
-- 'sortOrder', 'sortingConfiguration_sortOrder' - The order that the results should be returned in. In case of ties, the
-- relevance assigned to the result by Amazon Kendra is used as the
-- tie-breaker.
newSortingConfiguration ::
  -- | 'documentAttributeKey'
  Prelude.Text ->
  -- | 'sortOrder'
  SortOrder ->
  SortingConfiguration
newSortingConfiguration
  pDocumentAttributeKey_
  pSortOrder_ =
    SortingConfiguration'
      { documentAttributeKey =
          pDocumentAttributeKey_,
        sortOrder = pSortOrder_
      }

-- | The name of the document attribute used to sort the response. You can
-- use any field that has the @Sortable@ flag set to true.
--
-- You can also sort by any of the following built-in attributes:
--
-- -   _category
--
-- -   _created_at
--
-- -   _last_updated_at
--
-- -   _version
--
-- -   _view_count
sortingConfiguration_documentAttributeKey :: Lens.Lens' SortingConfiguration Prelude.Text
sortingConfiguration_documentAttributeKey = Lens.lens (\SortingConfiguration' {documentAttributeKey} -> documentAttributeKey) (\s@SortingConfiguration' {} a -> s {documentAttributeKey = a} :: SortingConfiguration)

-- | The order that the results should be returned in. In case of ties, the
-- relevance assigned to the result by Amazon Kendra is used as the
-- tie-breaker.
sortingConfiguration_sortOrder :: Lens.Lens' SortingConfiguration SortOrder
sortingConfiguration_sortOrder = Lens.lens (\SortingConfiguration' {sortOrder} -> sortOrder) (\s@SortingConfiguration' {} a -> s {sortOrder = a} :: SortingConfiguration)

instance Prelude.Hashable SortingConfiguration where
  hashWithSalt _salt SortingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` documentAttributeKey
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData SortingConfiguration where
  rnf SortingConfiguration' {..} =
    Prelude.rnf documentAttributeKey `Prelude.seq`
      Prelude.rnf sortOrder

instance Data.ToJSON SortingConfiguration where
  toJSON SortingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DocumentAttributeKey"
                  Data..= documentAttributeKey
              ),
            Prelude.Just ("SortOrder" Data..= sortOrder)
          ]
      )
