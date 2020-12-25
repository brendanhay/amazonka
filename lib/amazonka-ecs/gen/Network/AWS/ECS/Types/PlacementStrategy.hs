{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementStrategy
  ( PlacementStrategy (..),

    -- * Smart constructor
    mkPlacementStrategy,

    -- * Lenses
    psField,
    psType,
  )
where

import qualified Network.AWS.ECS.Types.PlacementStrategyType as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The task placement strategy for a task or service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html Task Placement Strategies> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkPlacementStrategy' smart constructor.
data PlacementStrategy = PlacementStrategy'
  { -- | The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
    field :: Core.Maybe Types.String,
    -- | The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
    type' :: Core.Maybe Types.PlacementStrategyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementStrategy' value with any optional fields omitted.
mkPlacementStrategy ::
  PlacementStrategy
mkPlacementStrategy =
  PlacementStrategy' {field = Core.Nothing, type' = Core.Nothing}

-- | The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
--
-- /Note:/ Consider using 'field' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psField :: Lens.Lens' PlacementStrategy (Core.Maybe Types.String)
psField = Lens.field @"field"
{-# DEPRECATED psField "Use generic-lens or generic-optics with 'field' instead." #-}

-- | The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psType :: Lens.Lens' PlacementStrategy (Core.Maybe Types.PlacementStrategyType)
psType = Lens.field @"type'"
{-# DEPRECATED psType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON PlacementStrategy where
  toJSON PlacementStrategy {..} =
    Core.object
      ( Core.catMaybes
          [ ("field" Core..=) Core.<$> field,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON PlacementStrategy where
  parseJSON =
    Core.withObject "PlacementStrategy" Core.$
      \x ->
        PlacementStrategy'
          Core.<$> (x Core..:? "field") Core.<*> (x Core..:? "type")
