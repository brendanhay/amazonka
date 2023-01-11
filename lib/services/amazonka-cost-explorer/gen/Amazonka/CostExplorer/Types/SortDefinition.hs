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
-- Module      : Amazonka.CostExplorer.Types.SortDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SortDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.SortOrder
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for how to sort the data.
--
-- /See:/ 'newSortDefinition' smart constructor.
data SortDefinition = SortDefinition'
  { -- | The order that\'s used to sort the data.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The key that\'s used to sort the data.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'sortDefinition_sortOrder' - The order that\'s used to sort the data.
--
-- 'key', 'sortDefinition_key' - The key that\'s used to sort the data.
newSortDefinition ::
  -- | 'key'
  Prelude.Text ->
  SortDefinition
newSortDefinition pKey_ =
  SortDefinition'
    { sortOrder = Prelude.Nothing,
      key = pKey_
    }

-- | The order that\'s used to sort the data.
sortDefinition_sortOrder :: Lens.Lens' SortDefinition (Prelude.Maybe SortOrder)
sortDefinition_sortOrder = Lens.lens (\SortDefinition' {sortOrder} -> sortOrder) (\s@SortDefinition' {} a -> s {sortOrder = a} :: SortDefinition)

-- | The key that\'s used to sort the data.
sortDefinition_key :: Lens.Lens' SortDefinition Prelude.Text
sortDefinition_key = Lens.lens (\SortDefinition' {key} -> key) (\s@SortDefinition' {} a -> s {key = a} :: SortDefinition)

instance Prelude.Hashable SortDefinition where
  hashWithSalt _salt SortDefinition' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` key

instance Prelude.NFData SortDefinition where
  rnf SortDefinition' {..} =
    Prelude.rnf sortOrder `Prelude.seq` Prelude.rnf key

instance Data.ToJSON SortDefinition where
  toJSON SortDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just ("Key" Data..= key)
          ]
      )
