{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ConnectionNotificationState
import Network.AWS.EC2.Types.ConnectionNotificationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a connection notification for a VPC endpoint or VPC endpoint
-- service.
--
-- /See:/ 'newConnectionNotification' smart constructor.
data ConnectionNotification = ConnectionNotification'
  { -- | The events for the notification. Valid values are @Accept@, @Connect@,
    -- @Delete@, and @Reject@.
    connectionEvents :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the notification.
    connectionNotificationId :: Prelude.Maybe Prelude.Text,
    -- | The type of notification.
    connectionNotificationType :: Prelude.Maybe ConnectionNotificationType,
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the endpoint service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The state of the notification.
    connectionNotificationState :: Prelude.Maybe ConnectionNotificationState,
    -- | The ARN of the SNS topic for the notification.
    connectionNotificationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      connectionNotificationId = Prelude.Nothing,
      connectionNotificationType = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      connectionNotificationState = Prelude.Nothing,
      connectionNotificationArn = Prelude.Nothing
    }

-- | The events for the notification. Valid values are @Accept@, @Connect@,
-- @Delete@, and @Reject@.
connectionNotification_connectionEvents :: Lens.Lens' ConnectionNotification (Prelude.Maybe [Prelude.Text])
connectionNotification_connectionEvents = Lens.lens (\ConnectionNotification' {connectionEvents} -> connectionEvents) (\s@ConnectionNotification' {} a -> s {connectionEvents = a} :: ConnectionNotification) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the notification.
connectionNotification_connectionNotificationId :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_connectionNotificationId = Lens.lens (\ConnectionNotification' {connectionNotificationId} -> connectionNotificationId) (\s@ConnectionNotification' {} a -> s {connectionNotificationId = a} :: ConnectionNotification)

-- | The type of notification.
connectionNotification_connectionNotificationType :: Lens.Lens' ConnectionNotification (Prelude.Maybe ConnectionNotificationType)
connectionNotification_connectionNotificationType = Lens.lens (\ConnectionNotification' {connectionNotificationType} -> connectionNotificationType) (\s@ConnectionNotification' {} a -> s {connectionNotificationType = a} :: ConnectionNotification)

-- | The ID of the VPC endpoint.
connectionNotification_vpcEndpointId :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_vpcEndpointId = Lens.lens (\ConnectionNotification' {vpcEndpointId} -> vpcEndpointId) (\s@ConnectionNotification' {} a -> s {vpcEndpointId = a} :: ConnectionNotification)

-- | The ID of the endpoint service.
connectionNotification_serviceId :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_serviceId = Lens.lens (\ConnectionNotification' {serviceId} -> serviceId) (\s@ConnectionNotification' {} a -> s {serviceId = a} :: ConnectionNotification)

-- | The state of the notification.
connectionNotification_connectionNotificationState :: Lens.Lens' ConnectionNotification (Prelude.Maybe ConnectionNotificationState)
connectionNotification_connectionNotificationState = Lens.lens (\ConnectionNotification' {connectionNotificationState} -> connectionNotificationState) (\s@ConnectionNotification' {} a -> s {connectionNotificationState = a} :: ConnectionNotification)

-- | The ARN of the SNS topic for the notification.
connectionNotification_connectionNotificationArn :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_connectionNotificationArn = Lens.lens (\ConnectionNotification' {connectionNotificationArn} -> connectionNotificationArn) (\s@ConnectionNotification' {} a -> s {connectionNotificationArn = a} :: ConnectionNotification)

instance Prelude.FromXML ConnectionNotification where
  parseXML x =
    ConnectionNotification'
      Prelude.<$> ( x Prelude..@? "connectionEvents"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "connectionNotificationId")
      Prelude.<*> (x Prelude..@? "connectionNotificationType")
      Prelude.<*> (x Prelude..@? "vpcEndpointId")
      Prelude.<*> (x Prelude..@? "serviceId")
      Prelude.<*> (x Prelude..@? "connectionNotificationState")
      Prelude.<*> (x Prelude..@? "connectionNotificationArn")

instance Prelude.Hashable ConnectionNotification

instance Prelude.NFData ConnectionNotification
