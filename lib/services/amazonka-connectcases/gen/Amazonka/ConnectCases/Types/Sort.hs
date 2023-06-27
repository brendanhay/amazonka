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
-- Module      : Amazonka.ConnectCases.Types.Sort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.Sort where

import Amazonka.ConnectCases.Types.Order
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structured set of sort terms.
--
-- /See:/ 'newSort' smart constructor.
data Sort = Sort'
  { -- | Unique identifier of a field.
    fieldId :: Prelude.Text,
    -- | A structured set of sort terms
    sortOrder :: Order
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'sort_fieldId' - Unique identifier of a field.
--
-- 'sortOrder', 'sort_sortOrder' - A structured set of sort terms
newSort ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'sortOrder'
  Order ->
  Sort
newSort pFieldId_ pSortOrder_ =
  Sort' {fieldId = pFieldId_, sortOrder = pSortOrder_}

-- | Unique identifier of a field.
sort_fieldId :: Lens.Lens' Sort Prelude.Text
sort_fieldId = Lens.lens (\Sort' {fieldId} -> fieldId) (\s@Sort' {} a -> s {fieldId = a} :: Sort)

-- | A structured set of sort terms
sort_sortOrder :: Lens.Lens' Sort Order
sort_sortOrder = Lens.lens (\Sort' {sortOrder} -> sortOrder) (\s@Sort' {} a -> s {sortOrder = a} :: Sort)

instance Prelude.Hashable Sort where
  hashWithSalt _salt Sort' {..} =
    _salt
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData Sort where
  rnf Sort' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON Sort where
  toJSON Sort' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("fieldId" Data..= fieldId),
            Prelude.Just ("sortOrder" Data..= sortOrder)
          ]
      )
