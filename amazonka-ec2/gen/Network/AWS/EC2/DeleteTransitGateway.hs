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
-- Module      : Network.AWS.EC2.DeleteTransitGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway.
module Network.AWS.EC2.DeleteTransitGateway
  ( -- * Creating a Request
    DeleteTransitGateway (..),
    newDeleteTransitGateway,

    -- * Request Lenses
    deleteTransitGateway_dryRun,
    deleteTransitGateway_transitGatewayId,

    -- * Destructuring the Response
    DeleteTransitGatewayResponse (..),
    newDeleteTransitGatewayResponse,

    -- * Response Lenses
    deleteTransitGatewayResponse_transitGateway,
    deleteTransitGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGateway' smart constructor.
data DeleteTransitGateway = DeleteTransitGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayId', 'deleteTransitGateway_transitGatewayId' - The ID of the transit gateway.
newDeleteTransitGateway ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  DeleteTransitGateway
newDeleteTransitGateway pTransitGatewayId_ =
  DeleteTransitGateway'
    { dryRun = Prelude.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGateway_dryRun :: Lens.Lens' DeleteTransitGateway (Prelude.Maybe Prelude.Bool)
deleteTransitGateway_dryRun = Lens.lens (\DeleteTransitGateway' {dryRun} -> dryRun) (\s@DeleteTransitGateway' {} a -> s {dryRun = a} :: DeleteTransitGateway)

-- | The ID of the transit gateway.
deleteTransitGateway_transitGatewayId :: Lens.Lens' DeleteTransitGateway Prelude.Text
deleteTransitGateway_transitGatewayId = Lens.lens (\DeleteTransitGateway' {transitGatewayId} -> transitGatewayId) (\s@DeleteTransitGateway' {} a -> s {transitGatewayId = a} :: DeleteTransitGateway)

instance Core.AWSRequest DeleteTransitGateway where
  type
    AWSResponse DeleteTransitGateway =
      DeleteTransitGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayResponse'
            Prelude.<$> (x Core..@? "transitGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTransitGateway

instance Prelude.NFData DeleteTransitGateway

instance Core.ToHeaders DeleteTransitGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteTransitGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTransitGateway where
  toQuery DeleteTransitGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteTransitGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayId" Core.=: transitGatewayId
      ]

-- | /See:/ 'newDeleteTransitGatewayResponse' smart constructor.
data DeleteTransitGatewayResponse = DeleteTransitGatewayResponse'
  { -- | Information about the deleted transit gateway.
    transitGateway :: Prelude.Maybe TransitGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGateway', 'deleteTransitGatewayResponse_transitGateway' - Information about the deleted transit gateway.
--
-- 'httpStatus', 'deleteTransitGatewayResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayResponse
newDeleteTransitGatewayResponse pHttpStatus_ =
  DeleteTransitGatewayResponse'
    { transitGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted transit gateway.
deleteTransitGatewayResponse_transitGateway :: Lens.Lens' DeleteTransitGatewayResponse (Prelude.Maybe TransitGateway)
deleteTransitGatewayResponse_transitGateway = Lens.lens (\DeleteTransitGatewayResponse' {transitGateway} -> transitGateway) (\s@DeleteTransitGatewayResponse' {} a -> s {transitGateway = a} :: DeleteTransitGatewayResponse)

-- | The response's http status code.
deleteTransitGatewayResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayResponse Prelude.Int
deleteTransitGatewayResponse_httpStatus = Lens.lens (\DeleteTransitGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayResponse)

instance Prelude.NFData DeleteTransitGatewayResponse
