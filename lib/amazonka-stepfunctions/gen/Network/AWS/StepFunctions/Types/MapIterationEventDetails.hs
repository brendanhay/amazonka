{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.MapIterationEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapIterationEventDetails
  ( MapIterationEventDetails (..),

    -- * Smart constructor
    mkMapIterationEventDetails,

    -- * Lenses
    miedIndex,
    miedName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Name as Types

-- | Contains details about an iteration of a Map state.
--
-- /See:/ 'mkMapIterationEventDetails' smart constructor.
data MapIterationEventDetails = MapIterationEventDetails'
  { -- | The index of the array belonging to the Map state iteration.
    index :: Core.Maybe Core.Natural,
    -- | The name of the iteration’s parent Map state.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MapIterationEventDetails' value with any optional fields omitted.
mkMapIterationEventDetails ::
  MapIterationEventDetails
mkMapIterationEventDetails =
  MapIterationEventDetails'
    { index = Core.Nothing,
      name = Core.Nothing
    }

-- | The index of the array belonging to the Map state iteration.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miedIndex :: Lens.Lens' MapIterationEventDetails (Core.Maybe Core.Natural)
miedIndex = Lens.field @"index"
{-# DEPRECATED miedIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | The name of the iteration’s parent Map state.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miedName :: Lens.Lens' MapIterationEventDetails (Core.Maybe Types.Name)
miedName = Lens.field @"name"
{-# DEPRECATED miedName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON MapIterationEventDetails where
  parseJSON =
    Core.withObject "MapIterationEventDetails" Core.$
      \x ->
        MapIterationEventDetails'
          Core.<$> (x Core..:? "index") Core.<*> (x Core..:? "name")
