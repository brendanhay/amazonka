{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the actions for the specified alarms.
module Network.AWS.CloudWatch.EnableAlarmActions
  ( -- * Creating a request
    EnableAlarmActions (..),
    mkEnableAlarmActions,

    -- ** Request lenses
    eaaAlarmNames,

    -- * Destructuring the response
    EnableAlarmActionsResponse (..),
    mkEnableAlarmActionsResponse,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableAlarmActions' smart constructor.
newtype EnableAlarmActions = EnableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Types.AlarmName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAlarmActions' value with any optional fields omitted.
mkEnableAlarmActions ::
  EnableAlarmActions
mkEnableAlarmActions =
  EnableAlarmActions' {alarmNames = Core.mempty}

-- | The names of the alarms.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaaAlarmNames :: Lens.Lens' EnableAlarmActions [Types.AlarmName]
eaaAlarmNames = Lens.field @"alarmNames"
{-# DEPRECATED eaaAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

instance Core.AWSRequest EnableAlarmActions where
  type Rs EnableAlarmActions = EnableAlarmActionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "EnableAlarmActions")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue
                            "AlarmNames"
                            (Core.toQueryList "member" alarmNames)
                        )
            )
      }
  response = Response.receiveNull EnableAlarmActionsResponse'

-- | /See:/ 'mkEnableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse = EnableAlarmActionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAlarmActionsResponse' value with any optional fields omitted.
mkEnableAlarmActionsResponse ::
  EnableAlarmActionsResponse
mkEnableAlarmActionsResponse = EnableAlarmActionsResponse'
