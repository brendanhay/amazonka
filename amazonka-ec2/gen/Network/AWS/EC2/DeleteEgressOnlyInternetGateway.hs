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
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
  ( -- * Creating a Request
    DeleteEgressOnlyInternetGateway (..),
    newDeleteEgressOnlyInternetGateway,

    -- * Request Lenses
    deleteEgressOnlyInternetGateway_dryRun,
    deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId,

    -- * Destructuring the Response
    DeleteEgressOnlyInternetGatewayResponse (..),
    newDeleteEgressOnlyInternetGatewayResponse,

    -- * Response Lenses
    deleteEgressOnlyInternetGatewayResponse_returnCode,
    deleteEgressOnlyInternetGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEgressOnlyInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteEgressOnlyInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'egressOnlyInternetGatewayId', 'deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
newDeleteEgressOnlyInternetGateway ::
  -- | 'egressOnlyInternetGatewayId'
  Core.Text ->
  DeleteEgressOnlyInternetGateway
newDeleteEgressOnlyInternetGateway
  pEgressOnlyInternetGatewayId_ =
    DeleteEgressOnlyInternetGateway'
      { dryRun =
          Core.Nothing,
        egressOnlyInternetGatewayId =
          pEgressOnlyInternetGatewayId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteEgressOnlyInternetGateway_dryRun :: Lens.Lens' DeleteEgressOnlyInternetGateway (Core.Maybe Core.Bool)
deleteEgressOnlyInternetGateway_dryRun = Lens.lens (\DeleteEgressOnlyInternetGateway' {dryRun} -> dryRun) (\s@DeleteEgressOnlyInternetGateway' {} a -> s {dryRun = a} :: DeleteEgressOnlyInternetGateway)

-- | The ID of the egress-only internet gateway.
deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId :: Lens.Lens' DeleteEgressOnlyInternetGateway Core.Text
deleteEgressOnlyInternetGateway_egressOnlyInternetGatewayId = Lens.lens (\DeleteEgressOnlyInternetGateway' {egressOnlyInternetGatewayId} -> egressOnlyInternetGatewayId) (\s@DeleteEgressOnlyInternetGateway' {} a -> s {egressOnlyInternetGatewayId = a} :: DeleteEgressOnlyInternetGateway)

instance
  Core.AWSRequest
    DeleteEgressOnlyInternetGateway
  where
  type
    AWSResponse DeleteEgressOnlyInternetGateway =
      DeleteEgressOnlyInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteEgressOnlyInternetGatewayResponse'
            Core.<$> (x Core..@? "returnCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteEgressOnlyInternetGateway

instance Core.NFData DeleteEgressOnlyInternetGateway

instance
  Core.ToHeaders
    DeleteEgressOnlyInternetGateway
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteEgressOnlyInternetGateway where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEgressOnlyInternetGateway where
  toQuery DeleteEgressOnlyInternetGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteEgressOnlyInternetGateway" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "EgressOnlyInternetGatewayId"
          Core.=: egressOnlyInternetGatewayId
      ]

-- | /See:/ 'newDeleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnCode :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEgressOnlyInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnCode', 'deleteEgressOnlyInternetGatewayResponse_returnCode' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'deleteEgressOnlyInternetGatewayResponse_httpStatus' - The response's http status code.
newDeleteEgressOnlyInternetGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteEgressOnlyInternetGatewayResponse
newDeleteEgressOnlyInternetGatewayResponse
  pHttpStatus_ =
    DeleteEgressOnlyInternetGatewayResponse'
      { returnCode =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
deleteEgressOnlyInternetGatewayResponse_returnCode :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse (Core.Maybe Core.Bool)
deleteEgressOnlyInternetGatewayResponse_returnCode = Lens.lens (\DeleteEgressOnlyInternetGatewayResponse' {returnCode} -> returnCode) (\s@DeleteEgressOnlyInternetGatewayResponse' {} a -> s {returnCode = a} :: DeleteEgressOnlyInternetGatewayResponse)

-- | The response's http status code.
deleteEgressOnlyInternetGatewayResponse_httpStatus :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse Core.Int
deleteEgressOnlyInternetGatewayResponse_httpStatus = Lens.lens (\DeleteEgressOnlyInternetGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteEgressOnlyInternetGatewayResponse' {} a -> s {httpStatus = a} :: DeleteEgressOnlyInternetGatewayResponse)

instance
  Core.NFData
    DeleteEgressOnlyInternetGatewayResponse
