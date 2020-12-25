{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroupStatus
  ( ParameterGroupStatus (..),

    -- * Smart constructor
    mkParameterGroupStatus,

    -- * Lenses
    pgsNodeIdsToReboot,
    pgsParameterApplyStatus,
    pgsParameterGroupName,
  )
where

import qualified Network.AWS.DAX.Types.ParameterApplyStatus as Types
import qualified Network.AWS.DAX.Types.ParameterGroupName as Types
import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of a parameter group.
--
-- /See:/ 'mkParameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { -- | The node IDs of one or more nodes to be rebooted.
    nodeIdsToReboot :: Core.Maybe [Types.String],
    -- | The status of parameter updates.
    parameterApplyStatus :: Core.Maybe Types.ParameterApplyStatus,
    -- | The name of the parameter group.
    parameterGroupName :: Core.Maybe Types.ParameterGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterGroupStatus' value with any optional fields omitted.
mkParameterGroupStatus ::
  ParameterGroupStatus
mkParameterGroupStatus =
  ParameterGroupStatus'
    { nodeIdsToReboot = Core.Nothing,
      parameterApplyStatus = Core.Nothing,
      parameterGroupName = Core.Nothing
    }

-- | The node IDs of one or more nodes to be rebooted.
--
-- /Note:/ Consider using 'nodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsNodeIdsToReboot :: Lens.Lens' ParameterGroupStatus (Core.Maybe [Types.String])
pgsNodeIdsToReboot = Lens.field @"nodeIdsToReboot"
{-# DEPRECATED pgsNodeIdsToReboot "Use generic-lens or generic-optics with 'nodeIdsToReboot' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsParameterApplyStatus :: Lens.Lens' ParameterGroupStatus (Core.Maybe Types.ParameterApplyStatus)
pgsParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# DEPRECATED pgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsParameterGroupName :: Lens.Lens' ParameterGroupStatus (Core.Maybe Types.ParameterGroupName)
pgsParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED pgsParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Core.FromJSON ParameterGroupStatus where
  parseJSON =
    Core.withObject "ParameterGroupStatus" Core.$
      \x ->
        ParameterGroupStatus'
          Core.<$> (x Core..:? "NodeIdsToReboot")
          Core.<*> (x Core..:? "ParameterApplyStatus")
          Core.<*> (x Core..:? "ParameterGroupName")
