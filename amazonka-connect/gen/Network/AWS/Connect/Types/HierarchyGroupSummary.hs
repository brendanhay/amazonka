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
-- Module      : Network.AWS.Connect.Types.HierarchyGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyGroupSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a hierarchy group.
--
-- /See:/ 'newHierarchyGroupSummary' smart constructor.
data HierarchyGroupSummary = HierarchyGroupSummary'
  { -- | The Amazon Resource Name (ARN) of the hierarchy group.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the hierarchy group.
    id :: Core.Maybe Core.Text,
    -- | The name of the hierarchy group.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HierarchyGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'hierarchyGroupSummary_arn' - The Amazon Resource Name (ARN) of the hierarchy group.
--
-- 'id', 'hierarchyGroupSummary_id' - The identifier of the hierarchy group.
--
-- 'name', 'hierarchyGroupSummary_name' - The name of the hierarchy group.
newHierarchyGroupSummary ::
  HierarchyGroupSummary
newHierarchyGroupSummary =
  HierarchyGroupSummary'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
hierarchyGroupSummary_arn :: Lens.Lens' HierarchyGroupSummary (Core.Maybe Core.Text)
hierarchyGroupSummary_arn = Lens.lens (\HierarchyGroupSummary' {arn} -> arn) (\s@HierarchyGroupSummary' {} a -> s {arn = a} :: HierarchyGroupSummary)

-- | The identifier of the hierarchy group.
hierarchyGroupSummary_id :: Lens.Lens' HierarchyGroupSummary (Core.Maybe Core.Text)
hierarchyGroupSummary_id = Lens.lens (\HierarchyGroupSummary' {id} -> id) (\s@HierarchyGroupSummary' {} a -> s {id = a} :: HierarchyGroupSummary)

-- | The name of the hierarchy group.
hierarchyGroupSummary_name :: Lens.Lens' HierarchyGroupSummary (Core.Maybe Core.Text)
hierarchyGroupSummary_name = Lens.lens (\HierarchyGroupSummary' {name} -> name) (\s@HierarchyGroupSummary' {} a -> s {name = a} :: HierarchyGroupSummary)

instance Core.FromJSON HierarchyGroupSummary where
  parseJSON =
    Core.withObject
      "HierarchyGroupSummary"
      ( \x ->
          HierarchyGroupSummary'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable HierarchyGroupSummary

instance Core.NFData HierarchyGroupSummary
