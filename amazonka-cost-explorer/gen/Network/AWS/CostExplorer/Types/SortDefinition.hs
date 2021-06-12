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
-- Module      : Network.AWS.CostExplorer.Types.SortDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SortDefinition where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.SortOrder
import qualified Network.AWS.Lens as Lens

-- | The details of how to sort the data.
--
-- /See:/ 'newSortDefinition' smart constructor.
data SortDefinition = SortDefinition'
  { -- | The order in which to sort the data.
    sortOrder :: Core.Maybe SortOrder,
    -- | The key by which to sort the data.
    key :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SortDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'sortDefinition_sortOrder' - The order in which to sort the data.
--
-- 'key', 'sortDefinition_key' - The key by which to sort the data.
newSortDefinition ::
  -- | 'key'
  Core.Text ->
  SortDefinition
newSortDefinition pKey_ =
  SortDefinition'
    { sortOrder = Core.Nothing,
      key = pKey_
    }

-- | The order in which to sort the data.
sortDefinition_sortOrder :: Lens.Lens' SortDefinition (Core.Maybe SortOrder)
sortDefinition_sortOrder = Lens.lens (\SortDefinition' {sortOrder} -> sortOrder) (\s@SortDefinition' {} a -> s {sortOrder = a} :: SortDefinition)

-- | The key by which to sort the data.
sortDefinition_key :: Lens.Lens' SortDefinition Core.Text
sortDefinition_key = Lens.lens (\SortDefinition' {key} -> key) (\s@SortDefinition' {} a -> s {key = a} :: SortDefinition)

instance Core.Hashable SortDefinition

instance Core.NFData SortDefinition

instance Core.ToJSON SortDefinition where
  toJSON SortDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            Core.Just ("Key" Core..= key)
          ]
      )
