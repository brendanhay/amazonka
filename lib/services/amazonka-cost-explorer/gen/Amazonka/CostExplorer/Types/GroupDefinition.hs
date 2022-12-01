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
-- Module      : Amazonka.CostExplorer.Types.GroupDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.GroupDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.GroupDefinitionType
import qualified Amazonka.Prelude as Prelude

-- | Represents a group when you specify a group by criteria or in the
-- response to a query with a specific grouping.
--
-- /See:/ 'newGroupDefinition' smart constructor.
data GroupDefinition = GroupDefinition'
  { -- | The string that represents a key for a specified group.
    key :: Prelude.Maybe Prelude.Text,
    -- | The string that represents the type of group.
    type' :: Prelude.Maybe GroupDefinitionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { key = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The string that represents a key for a specified group.
groupDefinition_key :: Lens.Lens' GroupDefinition (Prelude.Maybe Prelude.Text)
groupDefinition_key = Lens.lens (\GroupDefinition' {key} -> key) (\s@GroupDefinition' {} a -> s {key = a} :: GroupDefinition)

-- | The string that represents the type of group.
groupDefinition_type :: Lens.Lens' GroupDefinition (Prelude.Maybe GroupDefinitionType)
groupDefinition_type = Lens.lens (\GroupDefinition' {type'} -> type') (\s@GroupDefinition' {} a -> s {type' = a} :: GroupDefinition)

instance Core.FromJSON GroupDefinition where
  parseJSON =
    Core.withObject
      "GroupDefinition"
      ( \x ->
          GroupDefinition'
            Prelude.<$> (x Core..:? "Key") Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable GroupDefinition where
  hashWithSalt _salt GroupDefinition' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` type'

instance Prelude.NFData GroupDefinition where
  rnf GroupDefinition' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON GroupDefinition where
  toJSON GroupDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
