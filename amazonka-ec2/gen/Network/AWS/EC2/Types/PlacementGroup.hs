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
-- Module      : Network.AWS.EC2.Types.PlacementGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PlacementGroupState
import Network.AWS.EC2.Types.PlacementStrategy
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a placement group.
--
-- /See:/ 'newPlacementGroup' smart constructor.
data PlacementGroup = PlacementGroup'
  { -- | The placement strategy.
    strategy :: Core.Maybe PlacementStrategy,
    -- | The name of the placement group.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the placement group.
    groupId :: Core.Maybe Core.Text,
    -- | The state of the placement group.
    state :: Core.Maybe PlacementGroupState,
    -- | Any tags applied to the placement group.
    tags :: Core.Maybe [Tag],
    -- | The number of partitions. Valid only if __strategy__ is set to
    -- @partition@.
    partitionCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlacementGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'strategy', 'placementGroup_strategy' - The placement strategy.
--
-- 'groupName', 'placementGroup_groupName' - The name of the placement group.
--
-- 'groupId', 'placementGroup_groupId' - The ID of the placement group.
--
-- 'state', 'placementGroup_state' - The state of the placement group.
--
-- 'tags', 'placementGroup_tags' - Any tags applied to the placement group.
--
-- 'partitionCount', 'placementGroup_partitionCount' - The number of partitions. Valid only if __strategy__ is set to
-- @partition@.
newPlacementGroup ::
  PlacementGroup
newPlacementGroup =
  PlacementGroup'
    { strategy = Core.Nothing,
      groupName = Core.Nothing,
      groupId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      partitionCount = Core.Nothing
    }

-- | The placement strategy.
placementGroup_strategy :: Lens.Lens' PlacementGroup (Core.Maybe PlacementStrategy)
placementGroup_strategy = Lens.lens (\PlacementGroup' {strategy} -> strategy) (\s@PlacementGroup' {} a -> s {strategy = a} :: PlacementGroup)

-- | The name of the placement group.
placementGroup_groupName :: Lens.Lens' PlacementGroup (Core.Maybe Core.Text)
placementGroup_groupName = Lens.lens (\PlacementGroup' {groupName} -> groupName) (\s@PlacementGroup' {} a -> s {groupName = a} :: PlacementGroup)

-- | The ID of the placement group.
placementGroup_groupId :: Lens.Lens' PlacementGroup (Core.Maybe Core.Text)
placementGroup_groupId = Lens.lens (\PlacementGroup' {groupId} -> groupId) (\s@PlacementGroup' {} a -> s {groupId = a} :: PlacementGroup)

-- | The state of the placement group.
placementGroup_state :: Lens.Lens' PlacementGroup (Core.Maybe PlacementGroupState)
placementGroup_state = Lens.lens (\PlacementGroup' {state} -> state) (\s@PlacementGroup' {} a -> s {state = a} :: PlacementGroup)

-- | Any tags applied to the placement group.
placementGroup_tags :: Lens.Lens' PlacementGroup (Core.Maybe [Tag])
placementGroup_tags = Lens.lens (\PlacementGroup' {tags} -> tags) (\s@PlacementGroup' {} a -> s {tags = a} :: PlacementGroup) Core.. Lens.mapping Lens._Coerce

-- | The number of partitions. Valid only if __strategy__ is set to
-- @partition@.
placementGroup_partitionCount :: Lens.Lens' PlacementGroup (Core.Maybe Core.Int)
placementGroup_partitionCount = Lens.lens (\PlacementGroup' {partitionCount} -> partitionCount) (\s@PlacementGroup' {} a -> s {partitionCount = a} :: PlacementGroup)

instance Core.FromXML PlacementGroup where
  parseXML x =
    PlacementGroup'
      Core.<$> (x Core..@? "strategy")
      Core.<*> (x Core..@? "groupName")
      Core.<*> (x Core..@? "groupId")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "partitionCount")

instance Core.Hashable PlacementGroup

instance Core.NFData PlacementGroup
