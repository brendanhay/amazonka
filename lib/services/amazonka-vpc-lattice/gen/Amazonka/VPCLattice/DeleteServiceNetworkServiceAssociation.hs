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
-- Module      : Amazonka.VPCLattice.DeleteServiceNetworkServiceAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between a specified service and the specific
-- service network. This request will fail if an association is still in
-- progress.
module Amazonka.VPCLattice.DeleteServiceNetworkServiceAssociation
  ( -- * Creating a Request
    DeleteServiceNetworkServiceAssociation (..),
    newDeleteServiceNetworkServiceAssociation,

    -- * Request Lenses
    deleteServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier,

    -- * Destructuring the Response
    DeleteServiceNetworkServiceAssociationResponse (..),
    newDeleteServiceNetworkServiceAssociationResponse,

    -- * Response Lenses
    deleteServiceNetworkServiceAssociationResponse_arn,
    deleteServiceNetworkServiceAssociationResponse_id,
    deleteServiceNetworkServiceAssociationResponse_status,
    deleteServiceNetworkServiceAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteServiceNetworkServiceAssociation' smart constructor.
data DeleteServiceNetworkServiceAssociation = DeleteServiceNetworkServiceAssociation'
  { -- | The ID or Amazon Resource Name (ARN) of the association.
    serviceNetworkServiceAssociationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceNetworkServiceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNetworkServiceAssociationIdentifier', 'deleteServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier' - The ID or Amazon Resource Name (ARN) of the association.
newDeleteServiceNetworkServiceAssociation ::
  -- | 'serviceNetworkServiceAssociationIdentifier'
  Prelude.Text ->
  DeleteServiceNetworkServiceAssociation
newDeleteServiceNetworkServiceAssociation
  pServiceNetworkServiceAssociationIdentifier_ =
    DeleteServiceNetworkServiceAssociation'
      { serviceNetworkServiceAssociationIdentifier =
          pServiceNetworkServiceAssociationIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the association.
deleteServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier :: Lens.Lens' DeleteServiceNetworkServiceAssociation Prelude.Text
deleteServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier = Lens.lens (\DeleteServiceNetworkServiceAssociation' {serviceNetworkServiceAssociationIdentifier} -> serviceNetworkServiceAssociationIdentifier) (\s@DeleteServiceNetworkServiceAssociation' {} a -> s {serviceNetworkServiceAssociationIdentifier = a} :: DeleteServiceNetworkServiceAssociation)

instance
  Core.AWSRequest
    DeleteServiceNetworkServiceAssociation
  where
  type
    AWSResponse
      DeleteServiceNetworkServiceAssociation =
      DeleteServiceNetworkServiceAssociationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceNetworkServiceAssociationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteServiceNetworkServiceAssociation
  where
  hashWithSalt
    _salt
    DeleteServiceNetworkServiceAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` serviceNetworkServiceAssociationIdentifier

instance
  Prelude.NFData
    DeleteServiceNetworkServiceAssociation
  where
  rnf DeleteServiceNetworkServiceAssociation' {..} =
    Prelude.rnf
      serviceNetworkServiceAssociationIdentifier

instance
  Data.ToHeaders
    DeleteServiceNetworkServiceAssociation
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
    DeleteServiceNetworkServiceAssociation
  where
  toPath DeleteServiceNetworkServiceAssociation' {..} =
    Prelude.mconcat
      [ "/servicenetworkserviceassociations/",
        Data.toBS serviceNetworkServiceAssociationIdentifier
      ]

instance
  Data.ToQuery
    DeleteServiceNetworkServiceAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceNetworkServiceAssociationResponse' smart constructor.
data DeleteServiceNetworkServiceAssociationResponse = DeleteServiceNetworkServiceAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The operation\'s status. You can retry the operation if the status is
    -- @DELETE_FAILED@. However, if you retry it when the status is
    -- @DELETE_IN_PROGRESS@, there is no change in the status.
    status :: Prelude.Maybe ServiceNetworkServiceAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceNetworkServiceAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteServiceNetworkServiceAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'id', 'deleteServiceNetworkServiceAssociationResponse_id' - The ID of the association.
--
-- 'status', 'deleteServiceNetworkServiceAssociationResponse_status' - The operation\'s status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it when the status is
-- @DELETE_IN_PROGRESS@, there is no change in the status.
--
-- 'httpStatus', 'deleteServiceNetworkServiceAssociationResponse_httpStatus' - The response's http status code.
newDeleteServiceNetworkServiceAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceNetworkServiceAssociationResponse
newDeleteServiceNetworkServiceAssociationResponse
  pHttpStatus_ =
    DeleteServiceNetworkServiceAssociationResponse'
      { arn =
          Prelude.Nothing,
        id = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the association.
deleteServiceNetworkServiceAssociationResponse_arn :: Lens.Lens' DeleteServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
deleteServiceNetworkServiceAssociationResponse_arn = Lens.lens (\DeleteServiceNetworkServiceAssociationResponse' {arn} -> arn) (\s@DeleteServiceNetworkServiceAssociationResponse' {} a -> s {arn = a} :: DeleteServiceNetworkServiceAssociationResponse)

-- | The ID of the association.
deleteServiceNetworkServiceAssociationResponse_id :: Lens.Lens' DeleteServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
deleteServiceNetworkServiceAssociationResponse_id = Lens.lens (\DeleteServiceNetworkServiceAssociationResponse' {id} -> id) (\s@DeleteServiceNetworkServiceAssociationResponse' {} a -> s {id = a} :: DeleteServiceNetworkServiceAssociationResponse)

-- | The operation\'s status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it when the status is
-- @DELETE_IN_PROGRESS@, there is no change in the status.
deleteServiceNetworkServiceAssociationResponse_status :: Lens.Lens' DeleteServiceNetworkServiceAssociationResponse (Prelude.Maybe ServiceNetworkServiceAssociationStatus)
deleteServiceNetworkServiceAssociationResponse_status = Lens.lens (\DeleteServiceNetworkServiceAssociationResponse' {status} -> status) (\s@DeleteServiceNetworkServiceAssociationResponse' {} a -> s {status = a} :: DeleteServiceNetworkServiceAssociationResponse)

-- | The response's http status code.
deleteServiceNetworkServiceAssociationResponse_httpStatus :: Lens.Lens' DeleteServiceNetworkServiceAssociationResponse Prelude.Int
deleteServiceNetworkServiceAssociationResponse_httpStatus = Lens.lens (\DeleteServiceNetworkServiceAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceNetworkServiceAssociationResponse' {} a -> s {httpStatus = a} :: DeleteServiceNetworkServiceAssociationResponse)

instance
  Prelude.NFData
    DeleteServiceNetworkServiceAssociationResponse
  where
  rnf
    DeleteServiceNetworkServiceAssociationResponse' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf id
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf httpStatus
