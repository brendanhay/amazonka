{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Temporarily sets the state of an alarm for testing purposes. When the updated state differs from the previous value, the action configured for the appropriate state is invoked. For example, if your alarm is configured to send an Amazon SNS message when an alarm is triggered, temporarily changing the alarm state to @ALARM@ sends an SNS message.
--
-- Metric alarms returns to their actual state quickly, often within seconds. Because the metric alarm state change happens quickly, it is typically only visible in the alarm's __History__ tab in the Amazon CloudWatch console or through <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html DescribeAlarmHistory> .
-- If you use @SetAlarmState@ on a composite alarm, the composite alarm is not guaranteed to return to its actual state. It returns to its actual state only once any of its children alarms change state. It is also reevaluated if you update its configuration.
-- If an alarm triggers EC2 Auto Scaling policies or application Auto Scaling policies, you must include information in the @StateReasonData@ parameter to enable the policy to take the correct action.
module Network.AWS.CloudWatch.SetAlarmState
    (
    -- * Creating a request
      SetAlarmState (..)
    , mkSetAlarmState
    -- ** Request lenses
    , sasAlarmName
    , sasStateValue
    , sasStateReason
    , sasStateReasonData

    -- * Destructuring the response
    , SetAlarmStateResponse (..)
    , mkSetAlarmStateResponse
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetAlarmState' smart constructor.
data SetAlarmState = SetAlarmState'
  { alarmName :: Types.AlarmName
    -- ^ The name of the alarm.
  , stateValue :: Types.StateValue
    -- ^ The value of the state.
  , stateReason :: Types.StateReason
    -- ^ The reason that this alarm is set to this specific state, in text format.
  , stateReasonData :: Core.Maybe Types.StateReasonData
    -- ^ The reason that this alarm is set to this specific state, in JSON format.
--
-- For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetAlarmState' value with any optional fields omitted.
mkSetAlarmState
    :: Types.AlarmName -- ^ 'alarmName'
    -> Types.StateValue -- ^ 'stateValue'
    -> Types.StateReason -- ^ 'stateReason'
    -> SetAlarmState
mkSetAlarmState alarmName stateValue stateReason
  = SetAlarmState'{alarmName, stateValue, stateReason,
                   stateReasonData = Core.Nothing}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasAlarmName :: Lens.Lens' SetAlarmState Types.AlarmName
sasAlarmName = Lens.field @"alarmName"
{-# INLINEABLE sasAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | The value of the state.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStateValue :: Lens.Lens' SetAlarmState Types.StateValue
sasStateValue = Lens.field @"stateValue"
{-# INLINEABLE sasStateValue #-}
{-# DEPRECATED stateValue "Use generic-lens or generic-optics with 'stateValue' instead"  #-}

-- | The reason that this alarm is set to this specific state, in text format.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStateReason :: Lens.Lens' SetAlarmState Types.StateReason
sasStateReason = Lens.field @"stateReason"
{-# INLINEABLE sasStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | The reason that this alarm is set to this specific state, in JSON format.
--
-- For SNS or EC2 alarm actions, this is just informational. But for EC2 Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling policy uses the information in this field to take the correct action.
--
-- /Note:/ Consider using 'stateReasonData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasStateReasonData :: Lens.Lens' SetAlarmState (Core.Maybe Types.StateReasonData)
sasStateReasonData = Lens.field @"stateReasonData"
{-# INLINEABLE sasStateReasonData #-}
{-# DEPRECATED stateReasonData "Use generic-lens or generic-optics with 'stateReasonData' instead"  #-}

instance Core.ToQuery SetAlarmState where
        toQuery SetAlarmState{..}
          = Core.toQueryPair "Action" ("SetAlarmState" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "AlarmName" alarmName
              Core.<> Core.toQueryPair "StateValue" stateValue
              Core.<> Core.toQueryPair "StateReason" stateReason
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StateReasonData")
                stateReasonData

instance Core.ToHeaders SetAlarmState where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetAlarmState where
        type Rs SetAlarmState = SetAlarmStateResponse
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
        parseResponse = Response.receiveNull SetAlarmStateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetAlarmStateResponse' smart constructor.
data SetAlarmStateResponse = SetAlarmStateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetAlarmStateResponse' value with any optional fields omitted.
mkSetAlarmStateResponse
    :: SetAlarmStateResponse
mkSetAlarmStateResponse = SetAlarmStateResponse'
