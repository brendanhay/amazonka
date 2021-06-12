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
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified association between a VPC and local gateway route
-- table.
module Network.AWS.EC2.DeleteLocalGatewayRouteTableVpcAssociation
  ( -- * Creating a Request
    DeleteLocalGatewayRouteTableVpcAssociation (..),
    newDeleteLocalGatewayRouteTableVpcAssociation,

    -- * Request Lenses
    deleteLocalGatewayRouteTableVpcAssociation_dryRun,
    deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId,

    -- * Destructuring the Response
    DeleteLocalGatewayRouteTableVpcAssociationResponse (..),
    newDeleteLocalGatewayRouteTableVpcAssociationResponse,

    -- * Response Lenses
    deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLocalGatewayRouteTableVpcAssociation' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociation = DeleteLocalGatewayRouteTableVpcAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the association.
    localGatewayRouteTableVpcAssociationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLocalGatewayRouteTableVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteLocalGatewayRouteTableVpcAssociation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localGatewayRouteTableVpcAssociationId', 'deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId' - The ID of the association.
newDeleteLocalGatewayRouteTableVpcAssociation ::
  -- | 'localGatewayRouteTableVpcAssociationId'
  Core.Text ->
  DeleteLocalGatewayRouteTableVpcAssociation
newDeleteLocalGatewayRouteTableVpcAssociation
  pLocalGatewayRouteTableVpcAssociationId_ =
    DeleteLocalGatewayRouteTableVpcAssociation'
      { dryRun =
          Core.Nothing,
        localGatewayRouteTableVpcAssociationId =
          pLocalGatewayRouteTableVpcAssociationId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLocalGatewayRouteTableVpcAssociation_dryRun :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociation (Core.Maybe Core.Bool)
deleteLocalGatewayRouteTableVpcAssociation_dryRun = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociation' {dryRun} -> dryRun) (\s@DeleteLocalGatewayRouteTableVpcAssociation' {} a -> s {dryRun = a} :: DeleteLocalGatewayRouteTableVpcAssociation)

-- | The ID of the association.
deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociation Core.Text
deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociation' {localGatewayRouteTableVpcAssociationId} -> localGatewayRouteTableVpcAssociationId) (\s@DeleteLocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayRouteTableVpcAssociationId = a} :: DeleteLocalGatewayRouteTableVpcAssociation)

instance
  Core.AWSRequest
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  type
    AWSResponse
      DeleteLocalGatewayRouteTableVpcAssociation =
      DeleteLocalGatewayRouteTableVpcAssociationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteTableVpcAssociationResponse'
            Core.<$> (x Core..@? "localGatewayRouteTableVpcAssociation")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteLocalGatewayRouteTableVpcAssociation

instance
  Core.NFData
    DeleteLocalGatewayRouteTableVpcAssociation

instance
  Core.ToHeaders
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toQuery
    DeleteLocalGatewayRouteTableVpcAssociation' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DeleteLocalGatewayRouteTableVpcAssociation" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "LocalGatewayRouteTableVpcAssociationId"
            Core.=: localGatewayRouteTableVpcAssociationId
        ]

-- | /See:/ 'newDeleteLocalGatewayRouteTableVpcAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociationResponse = DeleteLocalGatewayRouteTableVpcAssociationResponse'
  { -- | Information about the association.
    localGatewayRouteTableVpcAssociation :: Core.Maybe LocalGatewayRouteTableVpcAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLocalGatewayRouteTableVpcAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableVpcAssociation', 'deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation' - Information about the association.
--
-- 'httpStatus', 'deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus' - The response's http status code.
newDeleteLocalGatewayRouteTableVpcAssociationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLocalGatewayRouteTableVpcAssociationResponse
newDeleteLocalGatewayRouteTableVpcAssociationResponse
  pHttpStatus_ =
    DeleteLocalGatewayRouteTableVpcAssociationResponse'
      { localGatewayRouteTableVpcAssociation =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the association.
deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociationResponse (Core.Maybe LocalGatewayRouteTableVpcAssociation)
deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociationResponse' {localGatewayRouteTableVpcAssociation} -> localGatewayRouteTableVpcAssociation) (\s@DeleteLocalGatewayRouteTableVpcAssociationResponse' {} a -> s {localGatewayRouteTableVpcAssociation = a} :: DeleteLocalGatewayRouteTableVpcAssociationResponse)

-- | The response's http status code.
deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociationResponse Core.Int
deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteLocalGatewayRouteTableVpcAssociationResponse' {} a -> s {httpStatus = a} :: DeleteLocalGatewayRouteTableVpcAssociationResponse)

instance
  Core.NFData
    DeleteLocalGatewayRouteTableVpcAssociationResponse
