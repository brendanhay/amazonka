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
-- Module      : Amazonka.EC2.Types.PlacementGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PlacementGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PlacementGroupState
import Amazonka.EC2.Types.PlacementStrategy
import Amazonka.EC2.Types.SpreadLevel
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a placement group.
--
-- /See:/ 'newPlacementGroup' smart constructor.
data PlacementGroup = PlacementGroup'
  { -- | The Amazon Resource Name (ARN) of the placement group.
    groupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the placement group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The number of partitions. Valid only if __strategy__ is set to
    -- @partition@.
    partitionCount :: Prelude.Maybe Prelude.Int,
    -- | The spread level for the placement group. /Only/ Outpost placement
    -- groups can be spread across hosts.
    spreadLevel :: Prelude.Maybe SpreadLevel,
    -- | The state of the placement group.
    state :: Prelude.Maybe PlacementGroupState,
    -- | The placement strategy.
    strategy :: Prelude.Maybe PlacementStrategy,
    -- | Any tags applied to the placement group.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupArn', 'placementGroup_groupArn' - The Amazon Resource Name (ARN) of the placement group.
--
-- 'groupId', 'placementGroup_groupId' - The ID of the placement group.
--
-- 'groupName', 'placementGroup_groupName' - The name of the placement group.
--
-- 'partitionCount', 'placementGroup_partitionCount' - The number of partitions. Valid only if __strategy__ is set to
-- @partition@.
--
-- 'spreadLevel', 'placementGroup_spreadLevel' - The spread level for the placement group. /Only/ Outpost placement
-- groups can be spread across hosts.
--
-- 'state', 'placementGroup_state' - The state of the placement group.
--
-- 'strategy', 'placementGroup_strategy' - The placement strategy.
--
-- 'tags', 'placementGroup_tags' - Any tags applied to the placement group.
newPlacementGroup ::
  PlacementGroup
newPlacementGroup =
  PlacementGroup'
    { groupArn = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      partitionCount = Prelude.Nothing,
      spreadLevel = Prelude.Nothing,
      state = Prelude.Nothing,
      strategy = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the placement group.
placementGroup_groupArn :: Lens.Lens' PlacementGroup (Prelude.Maybe Prelude.Text)
placementGroup_groupArn = Lens.lens (\PlacementGroup' {groupArn} -> groupArn) (\s@PlacementGroup' {} a -> s {groupArn = a} :: PlacementGroup)

-- | The ID of the placement group.
placementGroup_groupId :: Lens.Lens' PlacementGroup (Prelude.Maybe Prelude.Text)
placementGroup_groupId = Lens.lens (\PlacementGroup' {groupId} -> groupId) (\s@PlacementGroup' {} a -> s {groupId = a} :: PlacementGroup)

-- | The name of the placement group.
placementGroup_groupName :: Lens.Lens' PlacementGroup (Prelude.Maybe Prelude.Text)
placementGroup_groupName = Lens.lens (\PlacementGroup' {groupName} -> groupName) (\s@PlacementGroup' {} a -> s {groupName = a} :: PlacementGroup)

-- | The number of partitions. Valid only if __strategy__ is set to
-- @partition@.
placementGroup_partitionCount :: Lens.Lens' PlacementGroup (Prelude.Maybe Prelude.Int)
placementGroup_partitionCount = Lens.lens (\PlacementGroup' {partitionCount} -> partitionCount) (\s@PlacementGroup' {} a -> s {partitionCount = a} :: PlacementGroup)

-- | The spread level for the placement group. /Only/ Outpost placement
-- groups can be spread across hosts.
placementGroup_spreadLevel :: Lens.Lens' PlacementGroup (Prelude.Maybe SpreadLevel)
placementGroup_spreadLevel = Lens.lens (\PlacementGroup' {spreadLevel} -> spreadLevel) (\s@PlacementGroup' {} a -> s {spreadLevel = a} :: PlacementGroup)

-- | The state of the placement group.
placementGroup_state :: Lens.Lens' PlacementGroup (Prelude.Maybe PlacementGroupState)
placementGroup_state = Lens.lens (\PlacementGroup' {state} -> state) (\s@PlacementGroup' {} a -> s {state = a} :: PlacementGroup)

-- | The placement strategy.
placementGroup_strategy :: Lens.Lens' PlacementGroup (Prelude.Maybe PlacementStrategy)
placementGroup_strategy = Lens.lens (\PlacementGroup' {strategy} -> strategy) (\s@PlacementGroup' {} a -> s {strategy = a} :: PlacementGroup)

-- | Any tags applied to the placement group.
placementGroup_tags :: Lens.Lens' PlacementGroup (Prelude.Maybe [Tag])
placementGroup_tags = Lens.lens (\PlacementGroup' {tags} -> tags) (\s@PlacementGroup' {} a -> s {tags = a} :: PlacementGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML PlacementGroup where
  parseXML x =
    PlacementGroup'
      Prelude.<$> (x Data..@? "groupArn")
      Prelude.<*> (x Data..@? "groupId")
      Prelude.<*> (x Data..@? "groupName")
      Prelude.<*> (x Data..@? "partitionCount")
      Prelude.<*> (x Data..@? "spreadLevel")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "strategy")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable PlacementGroup where
  hashWithSalt _salt PlacementGroup' {..} =
    _salt `Prelude.hashWithSalt` groupArn
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` partitionCount
      `Prelude.hashWithSalt` spreadLevel
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` strategy
      `Prelude.hashWithSalt` tags

instance Prelude.NFData PlacementGroup where
  rnf PlacementGroup' {..} =
    Prelude.rnf groupArn
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf partitionCount
      `Prelude.seq` Prelude.rnf spreadLevel
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf strategy
      `Prelude.seq` Prelude.rnf tags
