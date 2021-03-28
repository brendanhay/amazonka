{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ManagedAction
  ( ManagedAction (..)
  -- * Smart constructor
  , mkManagedAction
  -- * Lenses
  , maActionDescription
  , maActionId
  , maActionType
  , maStatus
  , maWindowStartTime
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ActionStatus as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ActionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The record of an upcoming or in-progress managed action.
--
-- /See:/ 'mkManagedAction' smart constructor.
data ManagedAction = ManagedAction'
  { actionDescription :: Core.Maybe Core.Text
    -- ^ A description of the managed action.
  , actionId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the managed action.
  , actionType :: Core.Maybe Types.ActionType
    -- ^ The type of managed action.
  , status :: Core.Maybe Types.ActionStatus
    -- ^ The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
  , windowStartTime :: Core.Maybe Core.UTCTime
    -- ^ The start time of the maintenance window in which the managed action will execute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ManagedAction' value with any optional fields omitted.
mkManagedAction
    :: ManagedAction
mkManagedAction
  = ManagedAction'{actionDescription = Core.Nothing,
                   actionId = Core.Nothing, actionType = Core.Nothing,
                   status = Core.Nothing, windowStartTime = Core.Nothing}

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionDescription :: Lens.Lens' ManagedAction (Core.Maybe Core.Text)
maActionDescription = Lens.field @"actionDescription"
{-# INLINEABLE maActionDescription #-}
{-# DEPRECATED actionDescription "Use generic-lens or generic-optics with 'actionDescription' instead"  #-}

-- | A unique identifier for the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionId :: Lens.Lens' ManagedAction (Core.Maybe Core.Text)
maActionId = Lens.field @"actionId"
{-# INLINEABLE maActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The type of managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionType :: Lens.Lens' ManagedAction (Core.Maybe Types.ActionType)
maActionType = Lens.field @"actionType"
{-# INLINEABLE maActionType #-}
{-# DEPRECATED actionType "Use generic-lens or generic-optics with 'actionType' instead"  #-}

-- | The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStatus :: Lens.Lens' ManagedAction (Core.Maybe Types.ActionStatus)
maStatus = Lens.field @"status"
{-# INLINEABLE maStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The start time of the maintenance window in which the managed action will execute.
--
-- /Note:/ Consider using 'windowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maWindowStartTime :: Lens.Lens' ManagedAction (Core.Maybe Core.UTCTime)
maWindowStartTime = Lens.field @"windowStartTime"
{-# INLINEABLE maWindowStartTime #-}
{-# DEPRECATED windowStartTime "Use generic-lens or generic-optics with 'windowStartTime' instead"  #-}

instance Core.FromXML ManagedAction where
        parseXML x
          = ManagedAction' Core.<$>
              (x Core..@? "ActionDescription") Core.<*> x Core..@? "ActionId"
                Core.<*> x Core..@? "ActionType"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "WindowStartTime"
