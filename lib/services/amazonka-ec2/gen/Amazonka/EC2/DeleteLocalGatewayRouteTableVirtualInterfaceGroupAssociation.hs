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
-- Module      : Amazonka.EC2.DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a local gateway route table virtual interface group association.
module Amazonka.EC2.DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  ( -- * Creating a Request
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation,

    -- * Request Lenses
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun,
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId,

    -- * Destructuring the Response
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse (..),
    newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse,

    -- * Response Lenses
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation,
    deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation = DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the local gateway route table virtual interface group
    -- association.
    localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociationId', 'deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId' - The ID of the local gateway route table virtual interface group
-- association.
newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation ::
  -- | 'localGatewayRouteTableVirtualInterfaceGroupAssociationId'
  Prelude.Text ->
  DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  pLocalGatewayRouteTableVirtualInterfaceGroupAssociationId_ =
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation'
      { dryRun =
          Prelude.Nothing,
        localGatewayRouteTableVirtualInterfaceGroupAssociationId =
          pLocalGatewayRouteTableVirtualInterfaceGroupAssociationId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun :: Lens.Lens' DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Bool)
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun = Lens.lens (\DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {dryRun} -> dryRun) (\s@DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {dryRun = a} :: DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway route table virtual interface group
-- association.
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens.Lens' DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation Prelude.Text
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId = Lens.lens (\DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableVirtualInterfaceGroupAssociationId} -> localGatewayRouteTableVirtualInterfaceGroupAssociationId) (\s@DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationId = a} :: DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

instance
  Core.AWSRequest
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  type
    AWSResponse
      DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
      DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayRouteTableVirtualInterfaceGroupAssociation"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  hashWithSalt
    _salt
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` localGatewayRouteTableVirtualInterfaceGroupAssociationId

instance
  Prelude.NFData
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  rnf
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf
          localGatewayRouteTableVirtualInterfaceGroupAssociationId

instance
  Data.ToHeaders
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  toQuery
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociation" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
            Data.=: localGatewayRouteTableVirtualInterfaceGroupAssociationId
        ]

-- | /See:/ 'newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse = DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'
  { localGatewayRouteTableVirtualInterfaceGroupAssociation :: Prelude.Maybe LocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociation', 'deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation' - Undocumented member.
--
-- 'httpStatus', 'deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus' - The response's http status code.
newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
newDeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
  pHttpStatus_ =
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'
      { localGatewayRouteTableVirtualInterfaceGroupAssociation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation :: Lens.Lens' DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse (Prelude.Maybe LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation = Lens.lens (\DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {localGatewayRouteTableVirtualInterfaceGroupAssociation} -> localGatewayRouteTableVirtualInterfaceGroupAssociation) (\s@DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociation = a} :: DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse)

-- | The response's http status code.
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus :: Lens.Lens' DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse Prelude.Int
deleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus = Lens.lens (\DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {} a -> s {httpStatus = a} :: DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse)

instance
  Prelude.NFData
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
  where
  rnf
    DeleteLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {..} =
      Prelude.rnf
        localGatewayRouteTableVirtualInterfaceGroupAssociation
        `Prelude.seq` Prelude.rnf httpStatus
