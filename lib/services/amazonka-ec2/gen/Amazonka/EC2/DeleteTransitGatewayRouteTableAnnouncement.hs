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
-- Module      : Amazonka.EC2.DeleteTransitGatewayRouteTableAnnouncement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Advertises to the transit gateway that a transit gateway route table is
-- deleted.
module Amazonka.EC2.DeleteTransitGatewayRouteTableAnnouncement
  ( -- * Creating a Request
    DeleteTransitGatewayRouteTableAnnouncement (..),
    newDeleteTransitGatewayRouteTableAnnouncement,

    -- * Request Lenses
    deleteTransitGatewayRouteTableAnnouncement_dryRun,
    deleteTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId,

    -- * Destructuring the Response
    DeleteTransitGatewayRouteTableAnnouncementResponse (..),
    newDeleteTransitGatewayRouteTableAnnouncementResponse,

    -- * Response Lenses
    deleteTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement,
    deleteTransitGatewayRouteTableAnnouncementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayRouteTableAnnouncement' smart constructor.
data DeleteTransitGatewayRouteTableAnnouncement = DeleteTransitGatewayRouteTableAnnouncement'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The transit gateway route table ID that\'s being deleted.
    transitGatewayRouteTableAnnouncementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayRouteTableAnnouncement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayRouteTableAnnouncement_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableAnnouncementId', 'deleteTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId' - The transit gateway route table ID that\'s being deleted.
newDeleteTransitGatewayRouteTableAnnouncement ::
  -- | 'transitGatewayRouteTableAnnouncementId'
  Prelude.Text ->
  DeleteTransitGatewayRouteTableAnnouncement
newDeleteTransitGatewayRouteTableAnnouncement
  pTransitGatewayRouteTableAnnouncementId_ =
    DeleteTransitGatewayRouteTableAnnouncement'
      { dryRun =
          Prelude.Nothing,
        transitGatewayRouteTableAnnouncementId =
          pTransitGatewayRouteTableAnnouncementId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayRouteTableAnnouncement_dryRun :: Lens.Lens' DeleteTransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayRouteTableAnnouncement_dryRun = Lens.lens (\DeleteTransitGatewayRouteTableAnnouncement' {dryRun} -> dryRun) (\s@DeleteTransitGatewayRouteTableAnnouncement' {} a -> s {dryRun = a} :: DeleteTransitGatewayRouteTableAnnouncement)

-- | The transit gateway route table ID that\'s being deleted.
deleteTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId :: Lens.Lens' DeleteTransitGatewayRouteTableAnnouncement Prelude.Text
deleteTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId = Lens.lens (\DeleteTransitGatewayRouteTableAnnouncement' {transitGatewayRouteTableAnnouncementId} -> transitGatewayRouteTableAnnouncementId) (\s@DeleteTransitGatewayRouteTableAnnouncement' {} a -> s {transitGatewayRouteTableAnnouncementId = a} :: DeleteTransitGatewayRouteTableAnnouncement)

instance
  Core.AWSRequest
    DeleteTransitGatewayRouteTableAnnouncement
  where
  type
    AWSResponse
      DeleteTransitGatewayRouteTableAnnouncement =
      DeleteTransitGatewayRouteTableAnnouncementResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteTableAnnouncementResponse'
            Prelude.<$> (x Data..@? "transitGatewayRouteTableAnnouncement")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayRouteTableAnnouncement
  where
  hashWithSalt
    _salt
    DeleteTransitGatewayRouteTableAnnouncement' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayRouteTableAnnouncementId

instance
  Prelude.NFData
    DeleteTransitGatewayRouteTableAnnouncement
  where
  rnf DeleteTransitGatewayRouteTableAnnouncement' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncementId

instance
  Data.ToHeaders
    DeleteTransitGatewayRouteTableAnnouncement
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteTransitGatewayRouteTableAnnouncement
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteTransitGatewayRouteTableAnnouncement
  where
  toQuery
    DeleteTransitGatewayRouteTableAnnouncement' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DeleteTransitGatewayRouteTableAnnouncement" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "TransitGatewayRouteTableAnnouncementId"
            Data.=: transitGatewayRouteTableAnnouncementId
        ]

-- | /See:/ 'newDeleteTransitGatewayRouteTableAnnouncementResponse' smart constructor.
data DeleteTransitGatewayRouteTableAnnouncementResponse = DeleteTransitGatewayRouteTableAnnouncementResponse'
  { -- | Provides details about a deleted transit gateway route table.
    transitGatewayRouteTableAnnouncement :: Prelude.Maybe TransitGatewayRouteTableAnnouncement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayRouteTableAnnouncementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTableAnnouncement', 'deleteTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement' - Provides details about a deleted transit gateway route table.
--
-- 'httpStatus', 'deleteTransitGatewayRouteTableAnnouncementResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayRouteTableAnnouncementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayRouteTableAnnouncementResponse
newDeleteTransitGatewayRouteTableAnnouncementResponse
  pHttpStatus_ =
    DeleteTransitGatewayRouteTableAnnouncementResponse'
      { transitGatewayRouteTableAnnouncement =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Provides details about a deleted transit gateway route table.
deleteTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement :: Lens.Lens' DeleteTransitGatewayRouteTableAnnouncementResponse (Prelude.Maybe TransitGatewayRouteTableAnnouncement)
deleteTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement = Lens.lens (\DeleteTransitGatewayRouteTableAnnouncementResponse' {transitGatewayRouteTableAnnouncement} -> transitGatewayRouteTableAnnouncement) (\s@DeleteTransitGatewayRouteTableAnnouncementResponse' {} a -> s {transitGatewayRouteTableAnnouncement = a} :: DeleteTransitGatewayRouteTableAnnouncementResponse)

-- | The response's http status code.
deleteTransitGatewayRouteTableAnnouncementResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayRouteTableAnnouncementResponse Prelude.Int
deleteTransitGatewayRouteTableAnnouncementResponse_httpStatus = Lens.lens (\DeleteTransitGatewayRouteTableAnnouncementResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayRouteTableAnnouncementResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayRouteTableAnnouncementResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayRouteTableAnnouncementResponse
  where
  rnf
    DeleteTransitGatewayRouteTableAnnouncementResponse' {..} =
      Prelude.rnf transitGatewayRouteTableAnnouncement
        `Prelude.seq` Prelude.rnf httpStatus
