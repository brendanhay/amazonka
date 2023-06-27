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
-- Module      : Amazonka.VPCLattice.DeleteServiceNetworkVpcAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the VPC from the service network. You can\'t disassociate
-- the VPC if there is a create or update association in progress.
module Amazonka.VPCLattice.DeleteServiceNetworkVpcAssociation
  ( -- * Creating a Request
    DeleteServiceNetworkVpcAssociation (..),
    newDeleteServiceNetworkVpcAssociation,

    -- * Request Lenses
    deleteServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier,

    -- * Destructuring the Response
    DeleteServiceNetworkVpcAssociationResponse (..),
    newDeleteServiceNetworkVpcAssociationResponse,

    -- * Response Lenses
    deleteServiceNetworkVpcAssociationResponse_arn,
    deleteServiceNetworkVpcAssociationResponse_id,
    deleteServiceNetworkVpcAssociationResponse_status,
    deleteServiceNetworkVpcAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteServiceNetworkVpcAssociation' smart constructor.
data DeleteServiceNetworkVpcAssociation = DeleteServiceNetworkVpcAssociation'
  { -- | The ID or Amazon Resource Name (ARN) of the association.
    serviceNetworkVpcAssociationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceNetworkVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNetworkVpcAssociationIdentifier', 'deleteServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier' - The ID or Amazon Resource Name (ARN) of the association.
newDeleteServiceNetworkVpcAssociation ::
  -- | 'serviceNetworkVpcAssociationIdentifier'
  Prelude.Text ->
  DeleteServiceNetworkVpcAssociation
newDeleteServiceNetworkVpcAssociation
  pServiceNetworkVpcAssociationIdentifier_ =
    DeleteServiceNetworkVpcAssociation'
      { serviceNetworkVpcAssociationIdentifier =
          pServiceNetworkVpcAssociationIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the association.
deleteServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier :: Lens.Lens' DeleteServiceNetworkVpcAssociation Prelude.Text
deleteServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier = Lens.lens (\DeleteServiceNetworkVpcAssociation' {serviceNetworkVpcAssociationIdentifier} -> serviceNetworkVpcAssociationIdentifier) (\s@DeleteServiceNetworkVpcAssociation' {} a -> s {serviceNetworkVpcAssociationIdentifier = a} :: DeleteServiceNetworkVpcAssociation)

instance
  Core.AWSRequest
    DeleteServiceNetworkVpcAssociation
  where
  type
    AWSResponse DeleteServiceNetworkVpcAssociation =
      DeleteServiceNetworkVpcAssociationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceNetworkVpcAssociationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteServiceNetworkVpcAssociation
  where
  hashWithSalt
    _salt
    DeleteServiceNetworkVpcAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` serviceNetworkVpcAssociationIdentifier

instance
  Prelude.NFData
    DeleteServiceNetworkVpcAssociation
  where
  rnf DeleteServiceNetworkVpcAssociation' {..} =
    Prelude.rnf serviceNetworkVpcAssociationIdentifier

instance
  Data.ToHeaders
    DeleteServiceNetworkVpcAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteServiceNetworkVpcAssociation
  where
  toPath DeleteServiceNetworkVpcAssociation' {..} =
    Prelude.mconcat
      [ "/servicenetworkvpcassociations/",
        Data.toBS serviceNetworkVpcAssociationIdentifier
      ]

instance
  Data.ToQuery
    DeleteServiceNetworkVpcAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceNetworkVpcAssociationResponse' smart constructor.
data DeleteServiceNetworkVpcAssociationResponse = DeleteServiceNetworkVpcAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status. You can retry the operation if the status is
    -- @DELETE_FAILED@. However, if you retry it when the status is
    -- @DELETE_IN_PROGRESS@, there is no change in the status.
    status :: Prelude.Maybe ServiceNetworkVpcAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceNetworkVpcAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteServiceNetworkVpcAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'id', 'deleteServiceNetworkVpcAssociationResponse_id' - The ID of the association.
--
-- 'status', 'deleteServiceNetworkVpcAssociationResponse_status' - The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it when the status is
-- @DELETE_IN_PROGRESS@, there is no change in the status.
--
-- 'httpStatus', 'deleteServiceNetworkVpcAssociationResponse_httpStatus' - The response's http status code.
newDeleteServiceNetworkVpcAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceNetworkVpcAssociationResponse
newDeleteServiceNetworkVpcAssociationResponse
  pHttpStatus_ =
    DeleteServiceNetworkVpcAssociationResponse'
      { arn =
          Prelude.Nothing,
        id = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the association.
deleteServiceNetworkVpcAssociationResponse_arn :: Lens.Lens' DeleteServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
deleteServiceNetworkVpcAssociationResponse_arn = Lens.lens (\DeleteServiceNetworkVpcAssociationResponse' {arn} -> arn) (\s@DeleteServiceNetworkVpcAssociationResponse' {} a -> s {arn = a} :: DeleteServiceNetworkVpcAssociationResponse)

-- | The ID of the association.
deleteServiceNetworkVpcAssociationResponse_id :: Lens.Lens' DeleteServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
deleteServiceNetworkVpcAssociationResponse_id = Lens.lens (\DeleteServiceNetworkVpcAssociationResponse' {id} -> id) (\s@DeleteServiceNetworkVpcAssociationResponse' {} a -> s {id = a} :: DeleteServiceNetworkVpcAssociationResponse)

-- | The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it when the status is
-- @DELETE_IN_PROGRESS@, there is no change in the status.
deleteServiceNetworkVpcAssociationResponse_status :: Lens.Lens' DeleteServiceNetworkVpcAssociationResponse (Prelude.Maybe ServiceNetworkVpcAssociationStatus)
deleteServiceNetworkVpcAssociationResponse_status = Lens.lens (\DeleteServiceNetworkVpcAssociationResponse' {status} -> status) (\s@DeleteServiceNetworkVpcAssociationResponse' {} a -> s {status = a} :: DeleteServiceNetworkVpcAssociationResponse)

-- | The response's http status code.
deleteServiceNetworkVpcAssociationResponse_httpStatus :: Lens.Lens' DeleteServiceNetworkVpcAssociationResponse Prelude.Int
deleteServiceNetworkVpcAssociationResponse_httpStatus = Lens.lens (\DeleteServiceNetworkVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceNetworkVpcAssociationResponse' {} a -> s {httpStatus = a} :: DeleteServiceNetworkVpcAssociationResponse)

instance
  Prelude.NFData
    DeleteServiceNetworkVpcAssociationResponse
  where
  rnf DeleteServiceNetworkVpcAssociationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
