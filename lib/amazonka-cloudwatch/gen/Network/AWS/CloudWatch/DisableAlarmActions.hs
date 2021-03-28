{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the actions for the specified alarms. When an alarm's actions are disabled, the alarm actions do not execute when the alarm state changes.
module Network.AWS.CloudWatch.DisableAlarmActions
    (
    -- * Creating a request
      DisableAlarmActions (..)
    , mkDisableAlarmActions
    -- ** Request lenses
    , daaAlarmNames

    -- * Destructuring the response
    , DisableAlarmActionsResponse (..)
    , mkDisableAlarmActionsResponse
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableAlarmActions' smart constructor.
newtype DisableAlarmActions = DisableAlarmActions'
  { alarmNames :: [Types.AlarmName]
    -- ^ The names of the alarms.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAlarmActions' value with any optional fields omitted.
mkDisableAlarmActions
    :: DisableAlarmActions
mkDisableAlarmActions
  = DisableAlarmActions'{alarmNames = Core.mempty}

-- | The names of the alarms.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAlarmNames :: Lens.Lens' DisableAlarmActions [Types.AlarmName]
daaAlarmNames = Lens.field @"alarmNames"
{-# INLINEABLE daaAlarmNames #-}
{-# DEPRECATED alarmNames "Use generic-lens or generic-optics with 'alarmNames' instead"  #-}

instance Core.ToQuery DisableAlarmActions where
        toQuery DisableAlarmActions{..}
          = Core.toQueryPair "Action" ("DisableAlarmActions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AlarmNames"
                (Core.toQueryList "member" alarmNames)

instance Core.ToHeaders DisableAlarmActions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableAlarmActions where
        type Rs DisableAlarmActions = DisableAlarmActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisableAlarmActionsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableAlarmActionsResponse' smart constructor.
data DisableAlarmActionsResponse = DisableAlarmActionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAlarmActionsResponse' value with any optional fields omitted.
mkDisableAlarmActionsResponse
    :: DisableAlarmActionsResponse
mkDisableAlarmActionsResponse = DisableAlarmActionsResponse'
