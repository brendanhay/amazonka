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
-- Module      : Amazonka.EC2.DeleteLocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified association between a VPC and local gateway route
-- table.
module Amazonka.EC2.DeleteLocalGatewayRouteTableVpcAssociation
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  type
    AWSResponse
      DeleteLocalGatewayRouteTableVpcAssociation =
      DeleteLocalGatewayRouteTableVpcAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteTableVpcAssociationResponse'
            Prelude.<$> (x Core..@? "localGatewayRouteTableVpcAssociation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  hashWithSalt
    _salt
    DeleteLocalGatewayRouteTableVpcAssociation' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` localGatewayRouteTableVpcAssociationId

instance
  Prelude.NFData
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  rnf DeleteLocalGatewayRouteTableVpcAssociation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf localGatewayRouteTableVpcAssociationId

instance
  Core.ToHeaders
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteLocalGatewayRouteTableVpcAssociation
  where
  toQuery
    DeleteLocalGatewayRouteTableVpcAssociation' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "DeleteLocalGatewayRouteTableVpcAssociation" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Core.=: dryRun,
          "LocalGatewayRouteTableVpcAssociationId"
            Core.=: localGatewayRouteTableVpcAssociationId
        ]

-- | /See:/ 'newDeleteLocalGatewayRouteTableVpcAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVpcAssociationResponse = DeleteLocalGatewayRouteTableVpcAssociationResponse'
  { -- | Information about the association.
    localGatewayRouteTableVpcAssociation :: Prelude.Maybe LocalGatewayRouteTableVpcAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf
    DeleteLocalGatewayRouteTableVpcAssociationResponse' {..} =
      Prelude.rnf localGatewayRouteTableVpcAssociation
        `Prelude.seq` Prelude.rnf httpStatus
