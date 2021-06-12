{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionNotification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ConnectionNotificationState
import Network.AWS.EC2.Types.ConnectionNotificationType
import qualified Network.AWS.Lens as Lens

-- | Describes a connection notification for a VPC endpoint or VPC endpoint
-- service.
--
-- /See:/ 'newConnectionNotification' smart constructor.
data ConnectionNotification = ConnectionNotification'
  { -- | The events for the notification. Valid values are @Accept@, @Connect@,
    -- @Delete@, and @Reject@.
    connectionEvents :: Core.Maybe [Core.Text],
    -- | The ID of the notification.
    connectionNotificationId :: Core.Maybe Core.Text,
    -- | The type of notification.
    connectionNotificationType :: Core.Maybe ConnectionNotificationType,
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Core.Maybe Core.Text,
    -- | The ID of the endpoint service.
    serviceId :: Core.Maybe Core.Text,
    -- | The state of the notification.
    connectionNotificationState :: Core.Maybe ConnectionNotificationState,
    -- | The ARN of the SNS topic for the notification.
    connectionNotificationArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionEvents', 'connectionNotification_connectionEvents' - The events for the notification. Valid values are @Accept@, @Connect@,
-- @Delete@, and @Reject@.
--
-- 'connectionNotificationId', 'connectionNotification_connectionNotificationId' - The ID of the notification.
--
-- 'connectionNotificationType', 'connectionNotification_connectionNotificationType' - The type of notification.
--
-- 'vpcEndpointId', 'connectionNotification_vpcEndpointId' - The ID of the VPC endpoint.
--
-- 'serviceId', 'connectionNotification_serviceId' - The ID of the endpoint service.
--
-- 'connectionNotificationState', 'connectionNotification_connectionNotificationState' - The state of the notification.
--
-- 'connectionNotificationArn', 'connectionNotification_connectionNotificationArn' - The ARN of the SNS topic for the notification.
newConnectionNotification ::
  ConnectionNotification
newConnectionNotification =
  ConnectionNotification'
    { connectionEvents =
        Core.Nothing,
      connectionNotificationId = Core.Nothing,
      connectionNotificationType = Core.Nothing,
      vpcEndpointId = Core.Nothing,
      serviceId = Core.Nothing,
      connectionNotificationState = Core.Nothing,
      connectionNotificationArn = Core.Nothing
    }

-- | The events for the notification. Valid values are @Accept@, @Connect@,
-- @Delete@, and @Reject@.
connectionNotification_connectionEvents :: Lens.Lens' ConnectionNotification (Core.Maybe [Core.Text])
connectionNotification_connectionEvents = Lens.lens (\ConnectionNotification' {connectionEvents} -> connectionEvents) (\s@ConnectionNotification' {} a -> s {connectionEvents = a} :: ConnectionNotification) Core.. Lens.mapping Lens._Coerce

-- | The ID of the notification.
connectionNotification_connectionNotificationId :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
connectionNotification_connectionNotificationId = Lens.lens (\ConnectionNotification' {connectionNotificationId} -> connectionNotificationId) (\s@ConnectionNotification' {} a -> s {connectionNotificationId = a} :: ConnectionNotification)

-- | The type of notification.
connectionNotification_connectionNotificationType :: Lens.Lens' ConnectionNotification (Core.Maybe ConnectionNotificationType)
connectionNotification_connectionNotificationType = Lens.lens (\ConnectionNotification' {connectionNotificationType} -> connectionNotificationType) (\s@ConnectionNotification' {} a -> s {connectionNotificationType = a} :: ConnectionNotification)

-- | The ID of the VPC endpoint.
connectionNotification_vpcEndpointId :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
connectionNotification_vpcEndpointId = Lens.lens (\ConnectionNotification' {vpcEndpointId} -> vpcEndpointId) (\s@ConnectionNotification' {} a -> s {vpcEndpointId = a} :: ConnectionNotification)

-- | The ID of the endpoint service.
connectionNotification_serviceId :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
connectionNotification_serviceId = Lens.lens (\ConnectionNotification' {serviceId} -> serviceId) (\s@ConnectionNotification' {} a -> s {serviceId = a} :: ConnectionNotification)

-- | The state of the notification.
connectionNotification_connectionNotificationState :: Lens.Lens' ConnectionNotification (Core.Maybe ConnectionNotificationState)
connectionNotification_connectionNotificationState = Lens.lens (\ConnectionNotification' {connectionNotificationState} -> connectionNotificationState) (\s@ConnectionNotification' {} a -> s {connectionNotificationState = a} :: ConnectionNotification)

-- | The ARN of the SNS topic for the notification.
connectionNotification_connectionNotificationArn :: Lens.Lens' ConnectionNotification (Core.Maybe Core.Text)
connectionNotification_connectionNotificationArn = Lens.lens (\ConnectionNotification' {connectionNotificationArn} -> connectionNotificationArn) (\s@ConnectionNotification' {} a -> s {connectionNotificationArn = a} :: ConnectionNotification)

instance Core.FromXML ConnectionNotification where
  parseXML x =
    ConnectionNotification'
      Core.<$> ( x Core..@? "connectionEvents" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "connectionNotificationId")
      Core.<*> (x Core..@? "connectionNotificationType")
      Core.<*> (x Core..@? "vpcEndpointId")
      Core.<*> (x Core..@? "serviceId")
      Core.<*> (x Core..@? "connectionNotificationState")
      Core.<*> (x Core..@? "connectionNotificationArn")

instance Core.Hashable ConnectionNotification

instance Core.NFData ConnectionNotification
