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
-- Module      : Network.AWS.ECS.Types.PlacementStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementStrategy where

import Network.AWS.ECS.Types.PlacementStrategyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The task placement strategy for a task or service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html Task Placement Strategies>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newPlacementStrategy' smart constructor.
data PlacementStrategy = PlacementStrategy'
  { -- | The type of placement strategy. The @random@ placement strategy randomly
    -- places tasks on available candidates. The @spread@ placement strategy
    -- spreads placement across available candidates evenly based on the
    -- @field@ parameter. The @binpack@ strategy places tasks on available
    -- candidates that have the least available amount of the resource that is
    -- specified with the @field@ parameter. For example, if you binpack on
    -- memory, a task is placed on the instance with the least amount of
    -- remaining memory (but still enough to run the task).
    type' :: Prelude.Maybe PlacementStrategyType,
    -- | The field to apply the placement strategy against. For the @spread@
    -- placement strategy, valid values are @instanceId@ (or @host@, which has
    -- the same effect), or any platform or custom attribute that is applied to
    -- a container instance, such as @attribute:ecs.availability-zone@. For the
    -- @binpack@ placement strategy, valid values are @cpu@ and @memory@. For
    -- the @random@ placement strategy, this field is not used.
    field :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PlacementStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'placementStrategy_type' - The type of placement strategy. The @random@ placement strategy randomly
-- places tasks on available candidates. The @spread@ placement strategy
-- spreads placement across available candidates evenly based on the
-- @field@ parameter. The @binpack@ strategy places tasks on available
-- candidates that have the least available amount of the resource that is
-- specified with the @field@ parameter. For example, if you binpack on
-- memory, a task is placed on the instance with the least amount of
-- remaining memory (but still enough to run the task).
--
-- 'field', 'placementStrategy_field' - The field to apply the placement strategy against. For the @spread@
-- placement strategy, valid values are @instanceId@ (or @host@, which has
-- the same effect), or any platform or custom attribute that is applied to
-- a container instance, such as @attribute:ecs.availability-zone@. For the
-- @binpack@ placement strategy, valid values are @cpu@ and @memory@. For
-- the @random@ placement strategy, this field is not used.
newPlacementStrategy ::
  PlacementStrategy
newPlacementStrategy =
  PlacementStrategy'
    { type' = Prelude.Nothing,
      field = Prelude.Nothing
    }

-- | The type of placement strategy. The @random@ placement strategy randomly
-- places tasks on available candidates. The @spread@ placement strategy
-- spreads placement across available candidates evenly based on the
-- @field@ parameter. The @binpack@ strategy places tasks on available
-- candidates that have the least available amount of the resource that is
-- specified with the @field@ parameter. For example, if you binpack on
-- memory, a task is placed on the instance with the least amount of
-- remaining memory (but still enough to run the task).
placementStrategy_type :: Lens.Lens' PlacementStrategy (Prelude.Maybe PlacementStrategyType)
placementStrategy_type = Lens.lens (\PlacementStrategy' {type'} -> type') (\s@PlacementStrategy' {} a -> s {type' = a} :: PlacementStrategy)

-- | The field to apply the placement strategy against. For the @spread@
-- placement strategy, valid values are @instanceId@ (or @host@, which has
-- the same effect), or any platform or custom attribute that is applied to
-- a container instance, such as @attribute:ecs.availability-zone@. For the
-- @binpack@ placement strategy, valid values are @cpu@ and @memory@. For
-- the @random@ placement strategy, this field is not used.
placementStrategy_field :: Lens.Lens' PlacementStrategy (Prelude.Maybe Prelude.Text)
placementStrategy_field = Lens.lens (\PlacementStrategy' {field} -> field) (\s@PlacementStrategy' {} a -> s {field = a} :: PlacementStrategy)

instance Prelude.FromJSON PlacementStrategy where
  parseJSON =
    Prelude.withObject
      "PlacementStrategy"
      ( \x ->
          PlacementStrategy'
            Prelude.<$> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "field")
      )

instance Prelude.Hashable PlacementStrategy

instance Prelude.NFData PlacementStrategy

instance Prelude.ToJSON PlacementStrategy where
  toJSON PlacementStrategy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("type" Prelude..=) Prelude.<$> type',
            ("field" Prelude..=) Prelude.<$> field
          ]
      )
