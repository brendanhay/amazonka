{-# LANGUAGE DeriveDataTypeable #-}
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
    modifyVpcEndpointConnectionNotification_dryRun,
    modifyVpcEndpointConnectionNotification_connectionNotificationArn,
    modifyVpcEndpointConnectionNotification_connectionNotificationId,

    -- * Destructuring the Response
    ModifyVpcEndpointConnectionNotificationResponse (..),
    newModifyVpcEndpointConnectionNotificationResponse,

    -- * Response Lenses
    modifyVpcEndpointConnectionNotificationResponse_returnValue,
    modifyVpcEndpointConnectionNotificationResponse_httpStatus,
  )
where

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
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ARN for the SNS topic for the notification.
    connectionNotificationArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the notification.
    connectionNotificationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'dryRun', 'modifyVpcEndpointConnectionNotification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'connectionNotificationArn', 'modifyVpcEndpointConnectionNotification_connectionNotificationArn' - The ARN for the SNS topic for the notification.
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
        dryRun = Prelude.Nothing,
        connectionNotificationArn =
          Prelude.Nothing,
        connectionNotificationId =
          pConnectionNotificationId_
      }

-- | One or more events for the endpoint. Valid values are @Accept@,
-- @Connect@, @Delete@, and @Reject@.
modifyVpcEndpointConnectionNotification_connectionEvents :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointConnectionNotification_connectionEvents = Lens.lens (\ModifyVpcEndpointConnectionNotification' {connectionEvents} -> connectionEvents) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {connectionEvents = a} :: ModifyVpcEndpointConnectionNotification) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointConnectionNotification_dryRun :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointConnectionNotification_dryRun = Lens.lens (\ModifyVpcEndpointConnectionNotification' {dryRun} -> dryRun) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {dryRun = a} :: ModifyVpcEndpointConnectionNotification)

-- | The ARN for the SNS topic for the notification.
modifyVpcEndpointConnectionNotification_connectionNotificationArn :: Lens.Lens' ModifyVpcEndpointConnectionNotification (Prelude.Maybe Prelude.Text)
modifyVpcEndpointConnectionNotification_connectionNotificationArn = Lens.lens (\ModifyVpcEndpointConnectionNotification' {connectionNotificationArn} -> connectionNotificationArn) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {connectionNotificationArn = a} :: ModifyVpcEndpointConnectionNotification)

-- | The ID of the notification.
modifyVpcEndpointConnectionNotification_connectionNotificationId :: Lens.Lens' ModifyVpcEndpointConnectionNotification Prelude.Text
modifyVpcEndpointConnectionNotification_connectionNotificationId = Lens.lens (\ModifyVpcEndpointConnectionNotification' {connectionNotificationId} -> connectionNotificationId) (\s@ModifyVpcEndpointConnectionNotification' {} a -> s {connectionNotificationId = a} :: ModifyVpcEndpointConnectionNotification)

instance
  Prelude.AWSRequest
    ModifyVpcEndpointConnectionNotification
  where
  type
    Rs ModifyVpcEndpointConnectionNotification =
      ModifyVpcEndpointConnectionNotificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointConnectionNotificationResponse'
            Prelude.<$> (x Prelude..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointConnectionNotification

instance
  Prelude.NFData
    ModifyVpcEndpointConnectionNotification

instance
  Prelude.ToHeaders
    ModifyVpcEndpointConnectionNotification
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ModifyVpcEndpointConnectionNotification
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyVpcEndpointConnectionNotification
  where
  toQuery ModifyVpcEndpointConnectionNotification' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyVpcEndpointConnectionNotification" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "ConnectionEvents"
              Prelude.<$> connectionEvents
          ),
        "DryRun" Prelude.=: dryRun,
        "ConnectionNotificationArn"
          Prelude.=: connectionNotificationArn,
        "ConnectionNotificationId"
          Prelude.=: connectionNotificationId
      ]

-- | /See:/ 'newModifyVpcEndpointConnectionNotificationResponse' smart constructor.
data ModifyVpcEndpointConnectionNotificationResponse = ModifyVpcEndpointConnectionNotificationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
