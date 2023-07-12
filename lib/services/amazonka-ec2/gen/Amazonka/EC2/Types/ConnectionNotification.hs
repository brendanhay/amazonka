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
-- Module      : Amazonka.EC2.Types.ConnectionNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ConnectionNotification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ConnectionNotificationState
import Amazonka.EC2.Types.ConnectionNotificationType
import qualified Amazonka.Prelude as Prelude

-- | Describes a connection notification for a VPC endpoint or VPC endpoint
-- service.
--
-- /See:/ 'newConnectionNotification' smart constructor.
data ConnectionNotification = ConnectionNotification'
  { -- | The events for the notification. Valid values are @Accept@, @Connect@,
    -- @Delete@, and @Reject@.
    connectionEvents :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the SNS topic for the notification.
    connectionNotificationArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the notification.
    connectionNotificationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the notification.
    connectionNotificationState :: Prelude.Maybe ConnectionNotificationState,
    -- | The type of notification.
    connectionNotificationType :: Prelude.Maybe ConnectionNotificationType,
    -- | The ID of the endpoint service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'connectionNotificationArn', 'connectionNotification_connectionNotificationArn' - The ARN of the SNS topic for the notification.
--
-- 'connectionNotificationId', 'connectionNotification_connectionNotificationId' - The ID of the notification.
--
-- 'connectionNotificationState', 'connectionNotification_connectionNotificationState' - The state of the notification.
--
-- 'connectionNotificationType', 'connectionNotification_connectionNotificationType' - The type of notification.
--
-- 'serviceId', 'connectionNotification_serviceId' - The ID of the endpoint service.
--
-- 'vpcEndpointId', 'connectionNotification_vpcEndpointId' - The ID of the VPC endpoint.
newConnectionNotification ::
  ConnectionNotification
newConnectionNotification =
  ConnectionNotification'
    { connectionEvents =
        Prelude.Nothing,
      connectionNotificationArn = Prelude.Nothing,
      connectionNotificationId = Prelude.Nothing,
      connectionNotificationState = Prelude.Nothing,
      connectionNotificationType = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing
    }

-- | The events for the notification. Valid values are @Accept@, @Connect@,
-- @Delete@, and @Reject@.
connectionNotification_connectionEvents :: Lens.Lens' ConnectionNotification (Prelude.Maybe [Prelude.Text])
connectionNotification_connectionEvents = Lens.lens (\ConnectionNotification' {connectionEvents} -> connectionEvents) (\s@ConnectionNotification' {} a -> s {connectionEvents = a} :: ConnectionNotification) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the SNS topic for the notification.
connectionNotification_connectionNotificationArn :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_connectionNotificationArn = Lens.lens (\ConnectionNotification' {connectionNotificationArn} -> connectionNotificationArn) (\s@ConnectionNotification' {} a -> s {connectionNotificationArn = a} :: ConnectionNotification)

-- | The ID of the notification.
connectionNotification_connectionNotificationId :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_connectionNotificationId = Lens.lens (\ConnectionNotification' {connectionNotificationId} -> connectionNotificationId) (\s@ConnectionNotification' {} a -> s {connectionNotificationId = a} :: ConnectionNotification)

-- | The state of the notification.
connectionNotification_connectionNotificationState :: Lens.Lens' ConnectionNotification (Prelude.Maybe ConnectionNotificationState)
connectionNotification_connectionNotificationState = Lens.lens (\ConnectionNotification' {connectionNotificationState} -> connectionNotificationState) (\s@ConnectionNotification' {} a -> s {connectionNotificationState = a} :: ConnectionNotification)

-- | The type of notification.
connectionNotification_connectionNotificationType :: Lens.Lens' ConnectionNotification (Prelude.Maybe ConnectionNotificationType)
connectionNotification_connectionNotificationType = Lens.lens (\ConnectionNotification' {connectionNotificationType} -> connectionNotificationType) (\s@ConnectionNotification' {} a -> s {connectionNotificationType = a} :: ConnectionNotification)

-- | The ID of the endpoint service.
connectionNotification_serviceId :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_serviceId = Lens.lens (\ConnectionNotification' {serviceId} -> serviceId) (\s@ConnectionNotification' {} a -> s {serviceId = a} :: ConnectionNotification)

-- | The ID of the VPC endpoint.
connectionNotification_vpcEndpointId :: Lens.Lens' ConnectionNotification (Prelude.Maybe Prelude.Text)
connectionNotification_vpcEndpointId = Lens.lens (\ConnectionNotification' {vpcEndpointId} -> vpcEndpointId) (\s@ConnectionNotification' {} a -> s {vpcEndpointId = a} :: ConnectionNotification)

instance Data.FromXML ConnectionNotification where
  parseXML x =
    ConnectionNotification'
      Prelude.<$> ( x
                      Data..@? "connectionEvents"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "connectionNotificationArn")
      Prelude.<*> (x Data..@? "connectionNotificationId")
      Prelude.<*> (x Data..@? "connectionNotificationState")
      Prelude.<*> (x Data..@? "connectionNotificationType")
      Prelude.<*> (x Data..@? "serviceId")
      Prelude.<*> (x Data..@? "vpcEndpointId")

instance Prelude.Hashable ConnectionNotification where
  hashWithSalt _salt ConnectionNotification' {..} =
    _salt
      `Prelude.hashWithSalt` connectionEvents
      `Prelude.hashWithSalt` connectionNotificationArn
      `Prelude.hashWithSalt` connectionNotificationId
      `Prelude.hashWithSalt` connectionNotificationState
      `Prelude.hashWithSalt` connectionNotificationType
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` vpcEndpointId

instance Prelude.NFData ConnectionNotification where
  rnf ConnectionNotification' {..} =
    Prelude.rnf connectionEvents
      `Prelude.seq` Prelude.rnf connectionNotificationArn
      `Prelude.seq` Prelude.rnf connectionNotificationId
      `Prelude.seq` Prelude.rnf connectionNotificationState
      `Prelude.seq` Prelude.rnf connectionNotificationType
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf vpcEndpointId
