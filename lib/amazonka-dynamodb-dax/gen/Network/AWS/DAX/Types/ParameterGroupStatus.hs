{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.ParameterGroupStatus
  ( ParameterGroupStatus (..)
  -- * Smart constructor
  , mkParameterGroupStatus
  -- * Lenses
  , pgsNodeIdsToReboot
  , pgsParameterApplyStatus
  , pgsParameterGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of a parameter group.
--
-- /See:/ 'mkParameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { nodeIdsToReboot :: Core.Maybe [Core.Text]
    -- ^ The node IDs of one or more nodes to be rebooted.
  , parameterApplyStatus :: Core.Maybe Core.Text
    -- ^ The status of parameter updates. 
  , parameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterGroupStatus' value with any optional fields omitted.
mkParameterGroupStatus
    :: ParameterGroupStatus
mkParameterGroupStatus
  = ParameterGroupStatus'{nodeIdsToReboot = Core.Nothing,
                          parameterApplyStatus = Core.Nothing,
                          parameterGroupName = Core.Nothing}

-- | The node IDs of one or more nodes to be rebooted.
--
-- /Note:/ Consider using 'nodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsNodeIdsToReboot :: Lens.Lens' ParameterGroupStatus (Core.Maybe [Core.Text])
pgsNodeIdsToReboot = Lens.field @"nodeIdsToReboot"
{-# INLINEABLE pgsNodeIdsToReboot #-}
{-# DEPRECATED nodeIdsToReboot "Use generic-lens or generic-optics with 'nodeIdsToReboot' instead"  #-}

-- | The status of parameter updates. 
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsParameterApplyStatus :: Lens.Lens' ParameterGroupStatus (Core.Maybe Core.Text)
pgsParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# INLINEABLE pgsParameterApplyStatus #-}
{-# DEPRECATED parameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead"  #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsParameterGroupName :: Lens.Lens' ParameterGroupStatus (Core.Maybe Core.Text)
pgsParameterGroupName = Lens.field @"parameterGroupName"
{-# INLINEABLE pgsParameterGroupName #-}
{-# DEPRECATED parameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead"  #-}

instance Core.FromJSON ParameterGroupStatus where
        parseJSON
          = Core.withObject "ParameterGroupStatus" Core.$
              \ x ->
                ParameterGroupStatus' Core.<$>
                  (x Core..:? "NodeIdsToReboot") Core.<*>
                    x Core..:? "ParameterApplyStatus"
                    Core.<*> x Core..:? "ParameterGroupName"
