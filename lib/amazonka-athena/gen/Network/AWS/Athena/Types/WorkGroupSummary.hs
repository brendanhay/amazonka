{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupSummary
  ( WorkGroupSummary (..),

    -- * Smart constructor
    mkWorkGroupSummary,

    -- * Lenses
    wgsCreationTime,
    wgsDescription,
    wgsName,
    wgsState,
  )
where

import qualified Network.AWS.Athena.Types.Description as Types
import qualified Network.AWS.Athena.Types.Name as Types
import qualified Network.AWS.Athena.Types.WorkGroupState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary information for the workgroup, which includes its name, state, description, and the date and time it was created.
--
-- /See:/ 'mkWorkGroupSummary' smart constructor.
data WorkGroupSummary = WorkGroupSummary'
  { -- | The workgroup creation date and time.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The workgroup description.
    description :: Core.Maybe Types.Description,
    -- | The name of the workgroup.
    name :: Core.Maybe Types.Name,
    -- | The state of the workgroup.
    state :: Core.Maybe Types.WorkGroupState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'WorkGroupSummary' value with any optional fields omitted.
mkWorkGroupSummary ::
  WorkGroupSummary
mkWorkGroupSummary =
  WorkGroupSummary'
    { creationTime = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing
    }

-- | The workgroup creation date and time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsCreationTime :: Lens.Lens' WorkGroupSummary (Core.Maybe Core.NominalDiffTime)
wgsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED wgsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsDescription :: Lens.Lens' WorkGroupSummary (Core.Maybe Types.Description)
wgsDescription = Lens.field @"description"
{-# DEPRECATED wgsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the workgroup.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsName :: Lens.Lens' WorkGroupSummary (Core.Maybe Types.Name)
wgsName = Lens.field @"name"
{-# DEPRECATED wgsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of the workgroup.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsState :: Lens.Lens' WorkGroupSummary (Core.Maybe Types.WorkGroupState)
wgsState = Lens.field @"state"
{-# DEPRECATED wgsState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON WorkGroupSummary where
  parseJSON =
    Core.withObject "WorkGroupSummary" Core.$
      \x ->
        WorkGroupSummary'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "State")
