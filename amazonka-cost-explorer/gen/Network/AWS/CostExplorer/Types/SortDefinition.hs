{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types.SortOrder
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of how to sort the data.
--
-- /See:/ 'newSortDefinition' smart constructor.
data SortDefinition = SortDefinition'
  { -- | The order in which to sort the data.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The key by which to sort the data.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SortDefinition
newSortDefinition pKey_ =
  SortDefinition'
    { sortOrder = Prelude.Nothing,
      key = pKey_
    }

-- | The order in which to sort the data.
sortDefinition_sortOrder :: Lens.Lens' SortDefinition (Prelude.Maybe SortOrder)
sortDefinition_sortOrder = Lens.lens (\SortDefinition' {sortOrder} -> sortOrder) (\s@SortDefinition' {} a -> s {sortOrder = a} :: SortDefinition)

-- | The key by which to sort the data.
sortDefinition_key :: Lens.Lens' SortDefinition Prelude.Text
sortDefinition_key = Lens.lens (\SortDefinition' {key} -> key) (\s@SortDefinition' {} a -> s {key = a} :: SortDefinition)

instance Prelude.Hashable SortDefinition

instance Prelude.NFData SortDefinition

instance Prelude.ToJSON SortDefinition where
  toJSON SortDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            Prelude.Just ("Key" Prelude..= key)
          ]
      )
