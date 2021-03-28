{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ConnectionNotification
  ( ConnectionNotification (..)
  -- * Smart constructor
  , mkConnectionNotification
  -- * Lenses
  , cnConnectionEvents
  , cnConnectionNotificationArn
  , cnConnectionNotificationId
  , cnConnectionNotificationState
  , cnConnectionNotificationType
  , cnServiceId
  , cnVpcEndpointId
  ) where

import qualified Network.AWS.EC2.Types.ConnectionNotificationState as Types
import qualified Network.AWS.EC2.Types.ConnectionNotificationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a connection notification for a VPC endpoint or VPC endpoint service.
--
-- /See:/ 'mkConnectionNotification' smart constructor.
data ConnectionNotification = ConnectionNotification'
  { connectionEvents :: Core.Maybe [Core.Text]
    -- ^ The events for the notification. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
  , connectionNotificationArn :: Core.Maybe Core.Text
    -- ^ The ARN of the SNS topic for the notification.
  , connectionNotificationId :: Core.Maybe Core.Text
    -- ^ The ID of the notification.
  , connectionNotificationState :: Core.Maybe Types.ConnectionNotificationState
    -- ^ The state of the notification.
  , connectionNotificationType :: Core.Maybe Types.ConnectionNotificationType
    -- ^ The type of notification.
  , serviceId :: Core.Maybe Core.Text
    -- ^ The ID of the endpoint service.
  , vpcEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionNotification' value with any optional fields omitted.
mkConnectionNotification
    :: ConnectionNotification
mkConnectionNotification
  = ConnectionNotification'{connectionEvents = Core.Nothing,
                            connectionNotificationArn = Core.Nothing,
                            connectionNotificationId = Core.Nothing,
                            connectionNotificationState = Core.Nothing,
                            connectionNotificationType = Core.Nothing,
                            serviceId = Core.Nothing, vpcEndpointId = Core.Nothing}

-- | The events for the notification. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- /Note:/ Consider using 'connectionEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionEvents :: Lens.Lens' ConnectionNotification (Core.Maybe [Core.Text])
cnConnectionEvents = Lens.field @"connectionEvents"
{-# INLINEABLE cnConnectionEvents #-}
{-# DEPRECATED connectionEvents "Use generic-lens or generic-optics with 'connectionEvents' instead"  #-}

-- | The ARN of the SNS topic for the notification.
--
-- /Note:/ Consider using 'connectionNotificationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationArn :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
cnConnectionNotificationArn = Lens.field @"connectionNotificationArn"
{-# INLINEABLE cnConnectionNotificationArn #-}
{-# DEPRECATED connectionNotificationArn "Use generic-lens or generic-optics with 'connectionNotificationArn' instead"  #-}

-- | The ID of the notification.
--
-- /Note:/ Consider using 'connectionNotificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationId :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
cnConnectionNotificationId = Lens.field @"connectionNotificationId"
{-# INLINEABLE cnConnectionNotificationId #-}
{-# DEPRECATED connectionNotificationId "Use generic-lens or generic-optics with 'connectionNotificationId' instead"  #-}

-- | The state of the notification.
--
-- /Note:/ Consider using 'connectionNotificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationState :: Lens.Lens' ConnectionNotification (Core.Maybe Types.ConnectionNotificationState)
cnConnectionNotificationState = Lens.field @"connectionNotificationState"
{-# INLINEABLE cnConnectionNotificationState #-}
{-# DEPRECATED connectionNotificationState "Use generic-lens or generic-optics with 'connectionNotificationState' instead"  #-}

-- | The type of notification.
--
-- /Note:/ Consider using 'connectionNotificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationType :: Lens.Lens' ConnectionNotification (Core.Maybe Types.ConnectionNotificationType)
cnConnectionNotificationType = Lens.field @"connectionNotificationType"
{-# INLINEABLE cnConnectionNotificationType #-}
{-# DEPRECATED connectionNotificationType "Use generic-lens or generic-optics with 'connectionNotificationType' instead"  #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnServiceId :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
cnServiceId = Lens.field @"serviceId"
{-# INLINEABLE cnServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The ID of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnVpcEndpointId :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
cnVpcEndpointId = Lens.field @"vpcEndpointId"
{-# INLINEABLE cnVpcEndpointId #-}
{-# DEPRECATED vpcEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead"  #-}

instance Core.FromXML ConnectionNotification where
        parseXML x
          = ConnectionNotification' Core.<$>
              (x Core..@? "connectionEvents" Core..<@> Core.parseXMLList "item")
                Core.<*> x Core..@? "connectionNotificationArn"
                Core.<*> x Core..@? "connectionNotificationId"
                Core.<*> x Core..@? "connectionNotificationState"
                Core.<*> x Core..@? "connectionNotificationType"
                Core.<*> x Core..@? "serviceId"
                Core.<*> x Core..@? "vpcEndpointId"
