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
-- Module      : Network.AWS.EC2.ModifyVpcEndpointConnectionNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a connection notification for VPC endpoint or VPC endpoint
-- service. You can change the SNS topic for the notification, or the
-- events for which to be notified.
module Network.AWS.EC2.ModifyVpcEndpointConnectionNotification
  ( -- * Creating a Request
    ModifyVpcEndpointConnectionNotification (..),
    newModifyVpcEndpointConnectionNotification,

    -- * Request Lenses
    modifyVpcEndpointConnectionNotification_connectionEvents,
    modifyVpcEndpointConnectionNotification_connectionNotificationArn,
    modifyVpcEndpointConnectionNotification_dryRun,
    modifyVpcEndpointConnectionNotification_connectionNotificationId,

    -- * Destructuring the Response
    ModifyVpcEndpointConnectionNotificationResponse (..),
    newModifyVpcEndpointConnectionNotificationResponse,

    -- * Response Lenses
    modifyVpcEndpointConnectionNotificationResponse_returnValue,
    modifyVpcEndpointConnectionNotificationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcEndpointConnectionNotification' smart constructor.
data ModifyVpcEndpointConnectionNotification = ModifyVpcEndpointConnectionNotification'
  { -- | One or more events for the endpoint. Valid values are @Accept@,
    -- @Connect@, @Delete@, and @Reject@.
    connectionEvents :: Prelude.Maybe [Prelude.Text],
    -- | The ARN for the SNS topic for the notification.
    connectionNotificationArn :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the notification.
    connectionNotificationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointConnectionNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionEvents', 'modifyVpcEndpointConnectionNotification_connectionEvents' - One or more events for the endpoint. Valid values are @Accept@,
-- @Connect@, @Delete@, and @Reject@.
--
-- 'connectionNotificationArn', 'modifyVpcEndpointConnectionNotification_connectionNotificationArn' - The ARN for the SNS topic for the notification.
--
-- 'dryRun', 'modifyVpcEndpointConnectionNotification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'connectionNotificationId', 'modifyVpcEndpointConnectionNotification_connectionNotificationId' - The ID of the notification.
newModifyVpcEndpointConnectionNotification ::
  -- | 'connectionNotificationId'
  Prelude.Text ->
  ModifyVpcEndpointConnectionNotification
newModifyVpcEndpointConnectionNotification
  pConnectionNotificationId_ =
    ModifyVpcEndpointConnectionNotification'
      { connectionEvents =
          Prelude.Nothing,
        connectionNotificationArn =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        connectionNotificationId =
          pConnectionNotificationId_
      }

-- | One or more events for the endpoint. Valid values are @Accept@,
-- @Connect@, @Delete@, and @Reject@.
modifyVpcEndpointConnectionNotification_connectionEvents :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointConnectionNotification_connectionEvents = Lens.lens (\ModifyVpcEndpointConnectionNotification' {connectionEvents} -> connectionEvents) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {connectionEvents = a} :: ModifyVpcEndpointConnectionNotification) Prelude.. Lens.mapping Lens.coerced

-- | The ARN for the SNS topic for the notification.
modifyVpcEndpointConnectionNotification_connectionNotificationArn :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Text)
modifyVpcEndpointConnectionNotification_connectionNotificationArn = Lens.lens (\ModifyVpcEndpointConnectionNotification' {connectionNotificationArn} -> connectionNotificationArn) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {connectionNotificationArn = a} :: ModifyVpcEndpointConnectionNotification)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointConnectionNotification_dryRun :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointConnectionNotification_dryRun = Lens.lens (\ModifyVpcEndpointConnectionNotification' {dryRun} -> dryRun) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {dryRun = a} :: ModifyVpcEndpointConnectionNotification)

-- | The ID of the notification.
modifyVpcEndpointConnectionNotification_connectionNotificationId :: Lens.Lens' ModifyVpcEndpointConnectionNotification Prelude.Text
modifyVpcEndpointConnectionNotification_connectionNotificationId = Lens.lens (\ModifyVpcEndpointConnectionNotification' {connectionNotificationId} -> connectionNotificationId) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {connectionNotificationId = a} :: ModifyVpcEndpointConnectionNotification)

instance
  Core.AWSRequest
    ModifyVpcEndpointConnectionNotification
  where
  type
    AWSResponse
      ModifyVpcEndpointConnectionNotification =
      ModifyVpcEndpointConnectionNotificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointConnectionNotificationResponse'
            Prelude.<$> (x Core..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointConnectionNotification

instance
  Prelude.NFData
    ModifyVpcEndpointConnectionNotification

instance
  Core.ToHeaders
    ModifyVpcEndpointConnectionNotification
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ModifyVpcEndpointConnectionNotification
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyVpcEndpointConnectionNotification
  where
  toQuery ModifyVpcEndpointConnectionNotification' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyVpcEndpointConnectionNotification" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "ConnectionEvents"
              Prelude.<$> connectionEvents
          ),
        "ConnectionNotificationArn"
          Core.=: connectionNotificationArn,
        "DryRun" Core.=: dryRun,
        "ConnectionNotificationId"
          Core.=: connectionNotificationId
      ]

-- | /See:/ 'newModifyVpcEndpointConnectionNotificationResponse' smart constructor.
data ModifyVpcEndpointConnectionNotificationResponse = ModifyVpcEndpointConnectionNotificationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointConnectionNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'modifyVpcEndpointConnectionNotificationResponse_returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyVpcEndpointConnectionNotificationResponse_httpStatus' - The response's http status code.
newModifyVpcEndpointConnectionNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpcEndpointConnectionNotificationResponse
newModifyVpcEndpointConnectionNotificationResponse
  pHttpStatus_ =
    ModifyVpcEndpointConnectionNotificationResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointConnectionNotificationResponse_returnValue :: Lens.Lens' ModifyVpcEndpointConnectionNotificationResponse (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointConnectionNotificationResponse_returnValue = Lens.lens (\ModifyVpcEndpointConnectionNotificationResponse' {returnValue} -> returnValue) (\s@ModifyVpcEndpointConnectionNotificationResponse' {} a -> s {returnValue = a} :: ModifyVpcEndpointConnectionNotificationResponse)

-- | The response's http status code.
modifyVpcEndpointConnectionNotificationResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointConnectionNotificationResponse Prelude.Int
modifyVpcEndpointConnectionNotificationResponse_httpStatus = Lens.lens (\ModifyVpcEndpointConnectionNotificationResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointConnectionNotificationResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointConnectionNotificationResponse)

instance
  Prelude.NFData
    ModifyVpcEndpointConnectionNotificationResponse
