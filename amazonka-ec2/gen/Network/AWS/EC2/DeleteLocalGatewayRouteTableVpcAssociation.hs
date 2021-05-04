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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLocalGatewayRouteTableVpcAssociation' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociation = DeleteLocalGatewayRouteTableVpcAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the association.
    localGatewayRouteTableVpcAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteLocalGatewayRouteTableVpcAssociation
newDeleteLocalGatewayRouteTableVpcAssociation
  pLocalGatewayRouteTableVpcAssociationId_ =
    DeleteLocalGatewayRouteTableVpcAssociation'
      { dryRun =
          Prelude.Nothing,
        localGatewayRouteTableVpcAssociationId =
          pLocalGatewayRouteTableVpcAssociationId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLocalGatewayRouteTableVpcAssociation_dryRun :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Bool)
deleteLocalGatewayRouteTableVpcAssociation_dryRun = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociation' {dryRun} -> dryRun) (\s@DeleteLocalGatewayRouteTableVpcAssociation' {} a -> s {dryRun = a} :: DeleteLocalGatewayRouteTableVpcAssociation)

-- | The ID of the association.
deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociation Prelude.Text
deleteLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociation' {localGatewayRouteTableVpcAssociationId} -> localGatewayRouteTableVpcAssociationId) (\s@DeleteLocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayRouteTableVpcAssociationId = a} :: DeleteLocalGatewayRouteTableVpcAssociation)

instance
  Prelude.AWSRequest
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  type
    Rs DeleteLocalGatewayRouteTableVpcAssociation =
      DeleteLocalGatewayRouteTableVpcAssociationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteTableVpcAssociationResponse'
            Prelude.<$> ( x
                            Prelude..@? "localGatewayRouteTableVpcAssociation"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteLocalGatewayRouteTableVpcAssociation

instance
  Prelude.NFData
    DeleteLocalGatewayRouteTableVpcAssociation

instance
  Prelude.ToHeaders
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toQuery
    DeleteLocalGatewayRouteTableVpcAssociation' {..} =
      Prelude.mconcat
        [ "Action"
            Prelude.=: ( "DeleteLocalGatewayRouteTableVpcAssociation" ::
                           Prelude.ByteString
                       ),
          "Version"
            Prelude.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Prelude.=: dryRun,
          "LocalGatewayRouteTableVpcAssociationId"
            Prelude.=: localGatewayRouteTableVpcAssociationId
        ]

-- | /See:/ 'newDeleteLocalGatewayRouteTableVpcAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociationResponse = DeleteLocalGatewayRouteTableVpcAssociationResponse'
  { -- | Information about the association.
    localGatewayRouteTableVpcAssociation :: Prelude.Maybe LocalGatewayRouteTableVpcAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteLocalGatewayRouteTableVpcAssociationResponse
newDeleteLocalGatewayRouteTableVpcAssociationResponse
  pHttpStatus_ =
    DeleteLocalGatewayRouteTableVpcAssociationResponse'
      { localGatewayRouteTableVpcAssociation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the association.
deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociationResponse (Prelude.Maybe LocalGatewayRouteTableVpcAssociation)
deleteLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociationResponse' {localGatewayRouteTableVpcAssociation} -> localGatewayRouteTableVpcAssociation) (\s@DeleteLocalGatewayRouteTableVpcAssociationResponse' {} a -> s {localGatewayRouteTableVpcAssociation = a} :: DeleteLocalGatewayRouteTableVpcAssociationResponse)

-- | The response's http status code.
deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus :: Lens.Lens' DeleteLocalGatewayRouteTableVpcAssociationResponse Prelude.Int
deleteLocalGatewayRouteTableVpcAssociationResponse_httpStatus = Lens.lens (\DeleteLocalGatewayRouteTableVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteLocalGatewayRouteTableVpcAssociationResponse' {} a -> s {httpStatus = a} :: DeleteLocalGatewayRouteTableVpcAssociationResponse)

instance
  Prelude.NFData
    DeleteLocalGatewayRouteTableVpcAssociationResponse
