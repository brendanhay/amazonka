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
-- Module      : Network.AWS.Connect.Types.HierarchyLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a hierarchy level.
--
-- /See:/ 'newHierarchyLevel' smart constructor.
data HierarchyLevel = HierarchyLevel'
  { -- | The Amazon Resource Name (ARN) of the hierarchy level.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the hierarchy level.
    id :: Core.Maybe Core.Text,
    -- | The name of the hierarchy level.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HierarchyLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'hierarchyLevel_arn' - The Amazon Resource Name (ARN) of the hierarchy level.
--
-- 'id', 'hierarchyLevel_id' - The identifier of the hierarchy level.
--
-- 'name', 'hierarchyLevel_name' - The name of the hierarchy level.
newHierarchyLevel ::
  HierarchyLevel
newHierarchyLevel =
  HierarchyLevel'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy level.
hierarchyLevel_arn :: Lens.Lens' HierarchyLevel (Core.Maybe Core.Text)
hierarchyLevel_arn = Lens.lens (\HierarchyLevel' {arn} -> arn) (\s@HierarchyLevel' {} a -> s {arn = a} :: HierarchyLevel)

-- | The identifier of the hierarchy level.
hierarchyLevel_id :: Lens.Lens' HierarchyLevel (Core.Maybe Core.Text)
hierarchyLevel_id = Lens.lens (\HierarchyLevel' {id} -> id) (\s@HierarchyLevel' {} a -> s {id = a} :: HierarchyLevel)

-- | The name of the hierarchy level.
hierarchyLevel_name :: Lens.Lens' HierarchyLevel (Core.Maybe Core.Text)
hierarchyLevel_name = Lens.lens (\HierarchyLevel' {name} -> name) (\s@HierarchyLevel' {} a -> s {name = a} :: HierarchyLevel)

instance Core.FromJSON HierarchyLevel where
  parseJSON =
    Core.withObject
      "HierarchyLevel"
      ( \x ->
          HierarchyLevel'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable HierarchyLevel

instance Core.NFData HierarchyLevel
