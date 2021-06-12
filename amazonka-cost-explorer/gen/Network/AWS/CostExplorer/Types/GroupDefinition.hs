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
-- Module      : Network.AWS.CostExplorer.Types.GroupDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.GroupDefinition where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.GroupDefinitionType
import qualified Network.AWS.Lens as Lens

-- | Represents a group when you specify a group by criteria or in the
-- response to a query with a specific grouping.
--
-- /See:/ 'newGroupDefinition' smart constructor.
data GroupDefinition = GroupDefinition'
  { -- | The string that represents a key for a specified group.
    key :: Core.Maybe Core.Text,
    -- | The string that represents the type of group.
    type' :: Core.Maybe GroupDefinitionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'groupDefinition_key' - The string that represents a key for a specified group.
--
-- 'type'', 'groupDefinition_type' - The string that represents the type of group.
newGroupDefinition ::
  GroupDefinition
newGroupDefinition =
  GroupDefinition'
    { key = Core.Nothing,
      type' = Core.Nothing
    }

-- | The string that represents a key for a specified group.
groupDefinition_key :: Lens.Lens' GroupDefinition (Core.Maybe Core.Text)
groupDefinition_key = Lens.lens (\GroupDefinition' {key} -> key) (\s@GroupDefinition' {} a -> s {key = a} :: GroupDefinition)

-- | The string that represents the type of group.
groupDefinition_type :: Lens.Lens' GroupDefinition (Core.Maybe GroupDefinitionType)
groupDefinition_type = Lens.lens (\GroupDefinition' {type'} -> type') (\s@GroupDefinition' {} a -> s {type' = a} :: GroupDefinition)

instance Core.FromJSON GroupDefinition where
  parseJSON =
    Core.withObject
      "GroupDefinition"
      ( \x ->
          GroupDefinition'
            Core.<$> (x Core..:? "Key") Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable GroupDefinition

instance Core.NFData GroupDefinition

instance Core.ToJSON GroupDefinition where
  toJSON GroupDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Type" Core..=) Core.<$> type'
          ]
      )
