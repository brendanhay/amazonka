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
-- Module      : Network.AWS.Config.Types.GroupedResourceCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.GroupedResourceCount where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The count of resources that are grouped by the group name.
--
-- /See:/ 'newGroupedResourceCount' smart constructor.
data GroupedResourceCount = GroupedResourceCount'
  { -- | The name of the group that can be region, account ID, or resource type.
    -- For example, region1, region2 if the region was chosen as @GroupByKey@.
    groupName :: Core.Text,
    -- | The number of resources in the group.
    resourceCount :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupedResourceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'groupedResourceCount_groupName' - The name of the group that can be region, account ID, or resource type.
-- For example, region1, region2 if the region was chosen as @GroupByKey@.
--
-- 'resourceCount', 'groupedResourceCount_resourceCount' - The number of resources in the group.
newGroupedResourceCount ::
  -- | 'groupName'
  Core.Text ->
  -- | 'resourceCount'
  Core.Integer ->
  GroupedResourceCount
newGroupedResourceCount pGroupName_ pResourceCount_ =
  GroupedResourceCount'
    { groupName = pGroupName_,
      resourceCount = pResourceCount_
    }

-- | The name of the group that can be region, account ID, or resource type.
-- For example, region1, region2 if the region was chosen as @GroupByKey@.
groupedResourceCount_groupName :: Lens.Lens' GroupedResourceCount Core.Text
groupedResourceCount_groupName = Lens.lens (\GroupedResourceCount' {groupName} -> groupName) (\s@GroupedResourceCount' {} a -> s {groupName = a} :: GroupedResourceCount)

-- | The number of resources in the group.
groupedResourceCount_resourceCount :: Lens.Lens' GroupedResourceCount Core.Integer
groupedResourceCount_resourceCount = Lens.lens (\GroupedResourceCount' {resourceCount} -> resourceCount) (\s@GroupedResourceCount' {} a -> s {resourceCount = a} :: GroupedResourceCount)

instance Core.FromJSON GroupedResourceCount where
  parseJSON =
    Core.withObject
      "GroupedResourceCount"
      ( \x ->
          GroupedResourceCount'
            Core.<$> (x Core..: "GroupName")
            Core.<*> (x Core..: "ResourceCount")
      )

instance Core.Hashable GroupedResourceCount

instance Core.NFData GroupedResourceCount
