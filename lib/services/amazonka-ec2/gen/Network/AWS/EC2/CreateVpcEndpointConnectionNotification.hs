{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpcEndpointConnectionNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection notification for a specified VPC endpoint or VPC
-- endpoint service. A connection notification notifies you of specific
-- endpoint events. You must create an SNS topic to receive notifications.
-- For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Create a Topic>
-- in the /Amazon Simple Notification Service Developer Guide/.
--
-- You can create a connection notification for interface endpoints only.
module Network.AWS.EC2.CreateVpcEndpointConnectionNotification
  ( -- * Creating a Request
    CreateVpcEndpointConnectionNotification (..),
    newCreateVpcEndpointConnectionNotification,

    -- * Request Lenses
    createVpcEndpointConnectionNotification_dryRun,
    createVpcEndpointConnectionNotification_vpcEndpointId,
    createVpcEndpointConnectionNotification_serviceId,
    createVpcEndpointConnectionNotification_clientToken,
    createVpcEndpointConnectionNotification_connectionNotificationArn,
    createVpcEndpointConnectionNotification_connectionEvents,

    -- * Destructuring the Response
    CreateVpcEndpointConnectionNotificationResponse (..),
    newCreateVpcEndpointConnectionNotificationResponse,

    -- * Response Lenses
    createVpcEndpointConnectionNotificationResponse_connectionNotification,
    createVpcEndpointConnectionNotificationResponse_clientToken,
    createVpcEndpointConnectionNotificationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateVpcEndpointConnectionNotification' smart constructor.
data CreateVpcEndpointConnectionNotification = CreateVpcEndpointConnectionNotification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the endpoint service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the SNS topic for the notifications.
    connectionNotificationArn :: Prelude.Text,
    -- | One or more endpoint events for which to receive notifications. Valid
    -- values are @Accept@, @Connect@, @Delete@, and @Reject@.
    connectionEvents :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointConnectionNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createVpcEndpointConnectionNotification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcEndpointId', 'createVpcEndpointConnectionNotification_vpcEndpointId' - The ID of the endpoint.
--
-- 'serviceId', 'createVpcEndpointConnectionNotification_serviceId' - The ID of the endpoint service.
--
-- 'clientToken', 'createVpcEndpointConnectionNotification_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'connectionNotificationArn', 'createVpcEndpointConnectionNotification_connectionNotificationArn' - The ARN of the SNS topic for the notifications.
--
-- 'connectionEvents', 'createVpcEndpointConnectionNotification_connectionEvents' - One or more endpoint events for which to receive notifications. Valid
-- values are @Accept@, @Connect@, @Delete@, and @Reject@.
newCreateVpcEndpointConnectionNotification ::
  -- | 'connectionNotificationArn'
  Prelude.Text ->
  CreateVpcEndpointConnectionNotification
newCreateVpcEndpointConnectionNotification
  pConnectionNotificationArn_ =
    CreateVpcEndpointConnectionNotification'
      { dryRun =
          Prelude.Nothing,
        vpcEndpointId = Prelude.Nothing,
        serviceId = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        connectionNotificationArn =
          pConnectionNotificationArn_,
        connectionEvents = Prelude.mempty
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpcEndpointConnectionNotification_dryRun :: Lens.Lens' CreateVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Bool)
createVpcEndpointConnectionNotification_dryRun = Lens.lens (\CreateVpcEndpointConnectionNotification' {dryRun} -> dryRun) (\s@CreateVpcEndpointConnectionNotification' {} a -> s {dryRun = a} :: CreateVpcEndpointConnectionNotification)

-- | The ID of the endpoint.
createVpcEndpointConnectionNotification_vpcEndpointId :: Lens.Lens' CreateVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Text)
createVpcEndpointConnectionNotification_vpcEndpointId = Lens.lens (\CreateVpcEndpointConnectionNotification' {vpcEndpointId} -> vpcEndpointId) (\s@CreateVpcEndpointConnectionNotification' {} a -> s {vpcEndpointId = a} :: CreateVpcEndpointConnectionNotification)

-- | The ID of the endpoint service.
createVpcEndpointConnectionNotification_serviceId :: Lens.Lens' CreateVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Text)
createVpcEndpointConnectionNotification_serviceId = Lens.lens (\CreateVpcEndpointConnectionNotification' {serviceId} -> serviceId) (\s@CreateVpcEndpointConnectionNotification' {} a -> s {serviceId = a} :: CreateVpcEndpointConnectionNotification)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createVpcEndpointConnectionNotification_clientToken :: Lens.Lens' CreateVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Text)
createVpcEndpointConnectionNotification_clientToken = Lens.lens (\CreateVpcEndpointConnectionNotification' {clientToken} -> clientToken) (\s@CreateVpcEndpointConnectionNotification' {} a -> s {clientToken = a} :: CreateVpcEndpointConnectionNotification)

-- | The ARN of the SNS topic for the notifications.
createVpcEndpointConnectionNotification_connectionNotificationArn :: Lens.Lens' CreateVpcEndpointConnectionNotification Prelude.Text
createVpcEndpointConnectionNotification_connectionNotificationArn = Lens.lens (\CreateVpcEndpointConnectionNotification' {connectionNotificationArn} -> connectionNotificationArn) (\s@CreateVpcEndpointConnectionNotification' {} a -> s {connectionNotificationArn = a} :: CreateVpcEndpointConnectionNotification)

-- | One or more endpoint events for which to receive notifications. Valid
-- values are @Accept@, @Connect@, @Delete@, and @Reject@.
createVpcEndpointConnectionNotification_connectionEvents :: Lens.Lens' CreateVpcEndpointConnectionNotification [Prelude.Text]
createVpcEndpointConnectionNotification_connectionEvents = Lens.lens (\CreateVpcEndpointConnectionNotification' {connectionEvents} -> connectionEvents) (\s@CreateVpcEndpointConnectionNotification' {} a -> s {connectionEvents = a} :: CreateVpcEndpointConnectionNotification) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    CreateVpcEndpointConnectionNotification
  where
  type
    AWSResponse
      CreateVpcEndpointConnectionNotification =
      CreateVpcEndpointConnectionNotificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpcEndpointConnectionNotificationResponse'
            Prelude.<$> (x Core..@? "connectionNotification")
              Prelude.<*> (x Core..@? "clientToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVpcEndpointConnectionNotification

instance
  Prelude.NFData
    CreateVpcEndpointConnectionNotification

instance
  Core.ToHeaders
    CreateVpcEndpointConnectionNotification
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    CreateVpcEndpointConnectionNotification
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateVpcEndpointConnectionNotification
  where
  toQuery CreateVpcEndpointConnectionNotification' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateVpcEndpointConnectionNotification" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "VpcEndpointId" Core.=: vpcEndpointId,
        "ServiceId" Core.=: serviceId,
        "ClientToken" Core.=: clientToken,
        "ConnectionNotificationArn"
          Core.=: connectionNotificationArn,
        Core.toQueryList "ConnectionEvents" connectionEvents
      ]

-- | /See:/ 'newCreateVpcEndpointConnectionNotificationResponse' smart constructor.
data CreateVpcEndpointConnectionNotificationResponse = CreateVpcEndpointConnectionNotificationResponse'
  { -- | Information about the notification.
    connectionNotification :: Prelude.Maybe ConnectionNotification,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointConnectionNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionNotification', 'createVpcEndpointConnectionNotificationResponse_connectionNotification' - Information about the notification.
--
-- 'clientToken', 'createVpcEndpointConnectionNotificationResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'httpStatus', 'createVpcEndpointConnectionNotificationResponse_httpStatus' - The response's http status code.
newCreateVpcEndpointConnectionNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcEndpointConnectionNotificationResponse
newCreateVpcEndpointConnectionNotificationResponse
  pHttpStatus_ =
    CreateVpcEndpointConnectionNotificationResponse'
      { connectionNotification =
          Prelude.Nothing,
        clientToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the notification.
createVpcEndpointConnectionNotificationResponse_connectionNotification :: Lens.Lens' CreateVpcEndpointConnectionNotificationResponse (Prelude.Maybe ConnectionNotification)
createVpcEndpointConnectionNotificationResponse_connectionNotification = Lens.lens (\CreateVpcEndpointConnectionNotificationResponse' {connectionNotification} -> connectionNotification) (\s@CreateVpcEndpointConnectionNotificationResponse' {} a -> s {connectionNotification = a} :: CreateVpcEndpointConnectionNotificationResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createVpcEndpointConnectionNotificationResponse_clientToken :: Lens.Lens' CreateVpcEndpointConnectionNotificationResponse (Prelude.Maybe Prelude.Text)
createVpcEndpointConnectionNotificationResponse_clientToken = Lens.lens (\CreateVpcEndpointConnectionNotificationResponse' {clientToken} -> clientToken) (\s@CreateVpcEndpointConnectionNotificationResponse' {} a -> s {clientToken = a} :: CreateVpcEndpointConnectionNotificationResponse)

-- | The response's http status code.
createVpcEndpointConnectionNotificationResponse_httpStatus :: Lens.Lens' CreateVpcEndpointConnectionNotificationResponse Prelude.Int
createVpcEndpointConnectionNotificationResponse_httpStatus = Lens.lens (\CreateVpcEndpointConnectionNotificationResponse' {httpStatus} -> httpStatus) (\s@CreateVpcEndpointConnectionNotificationResponse' {} a -> s {httpStatus = a} :: CreateVpcEndpointConnectionNotificationResponse)

instance
  Prelude.NFData
    CreateVpcEndpointConnectionNotificationResponse
