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
-- Module      : Amazonka.EC2.DeleteVpcEndpointConnectionNotifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint connection notifications.
module Amazonka.EC2.DeleteVpcEndpointConnectionNotifications
  ( -- * Creating a Request
    DeleteVpcEndpointConnectionNotifications (..),
    newDeleteVpcEndpointConnectionNotifications,

    -- * Request Lenses
    deleteVpcEndpointConnectionNotifications_dryRun,
    deleteVpcEndpointConnectionNotifications_connectionNotificationIds,

    -- * Destructuring the Response
    DeleteVpcEndpointConnectionNotificationsResponse (..),
    newDeleteVpcEndpointConnectionNotificationsResponse,

    -- * Response Lenses
    deleteVpcEndpointConnectionNotificationsResponse_unsuccessful,
    deleteVpcEndpointConnectionNotificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcEndpointConnectionNotifications' smart constructor.
data DeleteVpcEndpointConnectionNotifications = DeleteVpcEndpointConnectionNotifications'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more notification IDs.
    connectionNotificationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointConnectionNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpcEndpointConnectionNotifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'connectionNotificationIds', 'deleteVpcEndpointConnectionNotifications_connectionNotificationIds' - One or more notification IDs.
newDeleteVpcEndpointConnectionNotifications ::
  DeleteVpcEndpointConnectionNotifications
newDeleteVpcEndpointConnectionNotifications =
  DeleteVpcEndpointConnectionNotifications'
    { dryRun =
        Prelude.Nothing,
      connectionNotificationIds =
        Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpcEndpointConnectionNotifications_dryRun :: Lens.Lens' DeleteVpcEndpointConnectionNotifications (Prelude.Maybe Prelude.Bool)
deleteVpcEndpointConnectionNotifications_dryRun = Lens.lens (\DeleteVpcEndpointConnectionNotifications' {dryRun} -> dryRun) (\s@DeleteVpcEndpointConnectionNotifications' {} a -> s {dryRun = a} :: DeleteVpcEndpointConnectionNotifications)

-- | One or more notification IDs.
deleteVpcEndpointConnectionNotifications_connectionNotificationIds :: Lens.Lens' DeleteVpcEndpointConnectionNotifications [Prelude.Text]
deleteVpcEndpointConnectionNotifications_connectionNotificationIds = Lens.lens (\DeleteVpcEndpointConnectionNotifications' {connectionNotificationIds} -> connectionNotificationIds) (\s@DeleteVpcEndpointConnectionNotifications' {} a -> s {connectionNotificationIds = a} :: DeleteVpcEndpointConnectionNotifications) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DeleteVpcEndpointConnectionNotifications
  where
  type
    AWSResponse
      DeleteVpcEndpointConnectionNotifications =
      DeleteVpcEndpointConnectionNotificationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVpcEndpointConnectionNotificationsResponse'
            Prelude.<$> ( x
                            Data..@? "unsuccessful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVpcEndpointConnectionNotifications
  where
  hashWithSalt
    _salt
    DeleteVpcEndpointConnectionNotifications' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` connectionNotificationIds

instance
  Prelude.NFData
    DeleteVpcEndpointConnectionNotifications
  where
  rnf DeleteVpcEndpointConnectionNotifications' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf connectionNotificationIds

instance
  Data.ToHeaders
    DeleteVpcEndpointConnectionNotifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteVpcEndpointConnectionNotifications
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteVpcEndpointConnectionNotifications
  where
  toQuery DeleteVpcEndpointConnectionNotifications' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteVpcEndpointConnectionNotifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList
          "ConnectionNotificationId"
          connectionNotificationIds
      ]

-- | /See:/ 'newDeleteVpcEndpointConnectionNotificationsResponse' smart constructor.
data DeleteVpcEndpointConnectionNotificationsResponse = DeleteVpcEndpointConnectionNotificationsResponse'
  { -- | Information about the notifications that could not be deleted
    -- successfully.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointConnectionNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'deleteVpcEndpointConnectionNotificationsResponse_unsuccessful' - Information about the notifications that could not be deleted
-- successfully.
--
-- 'httpStatus', 'deleteVpcEndpointConnectionNotificationsResponse_httpStatus' - The response's http status code.
newDeleteVpcEndpointConnectionNotificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcEndpointConnectionNotificationsResponse
newDeleteVpcEndpointConnectionNotificationsResponse
  pHttpStatus_ =
    DeleteVpcEndpointConnectionNotificationsResponse'
      { unsuccessful =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the notifications that could not be deleted
-- successfully.
deleteVpcEndpointConnectionNotificationsResponse_unsuccessful :: Lens.Lens' DeleteVpcEndpointConnectionNotificationsResponse (Prelude.Maybe [UnsuccessfulItem])
deleteVpcEndpointConnectionNotificationsResponse_unsuccessful = Lens.lens (\DeleteVpcEndpointConnectionNotificationsResponse' {unsuccessful} -> unsuccessful) (\s@DeleteVpcEndpointConnectionNotificationsResponse' {} a -> s {unsuccessful = a} :: DeleteVpcEndpointConnectionNotificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteVpcEndpointConnectionNotificationsResponse_httpStatus :: Lens.Lens' DeleteVpcEndpointConnectionNotificationsResponse Prelude.Int
deleteVpcEndpointConnectionNotificationsResponse_httpStatus = Lens.lens (\DeleteVpcEndpointConnectionNotificationsResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcEndpointConnectionNotificationsResponse' {} a -> s {httpStatus = a} :: DeleteVpcEndpointConnectionNotificationsResponse)

instance
  Prelude.NFData
    DeleteVpcEndpointConnectionNotificationsResponse
  where
  rnf
    DeleteVpcEndpointConnectionNotificationsResponse' {..} =
      Prelude.rnf unsuccessful
        `Prelude.seq` Prelude.rnf httpStatus
