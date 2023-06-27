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
-- Module      : Amazonka.VPCLattice.UpdateServiceNetworkVpcAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the service network and VPC association. If you add a security
-- group to the service network and VPC association, the association must
-- continue to always have at least one security group. You can add or edit
-- security groups at any time. However, to remove all security groups, you
-- must first delete the association and recreate it without security
-- groups.
module Amazonka.VPCLattice.UpdateServiceNetworkVpcAssociation
  ( -- * Creating a Request
    UpdateServiceNetworkVpcAssociation (..),
    newUpdateServiceNetworkVpcAssociation,

    -- * Request Lenses
    updateServiceNetworkVpcAssociation_securityGroupIds,
    updateServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier,

    -- * Destructuring the Response
    UpdateServiceNetworkVpcAssociationResponse (..),
    newUpdateServiceNetworkVpcAssociationResponse,

    -- * Response Lenses
    updateServiceNetworkVpcAssociationResponse_arn,
    updateServiceNetworkVpcAssociationResponse_createdBy,
    updateServiceNetworkVpcAssociationResponse_id,
    updateServiceNetworkVpcAssociationResponse_securityGroupIds,
    updateServiceNetworkVpcAssociationResponse_status,
    updateServiceNetworkVpcAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateServiceNetworkVpcAssociation' smart constructor.
data UpdateServiceNetworkVpcAssociation = UpdateServiceNetworkVpcAssociation'
  { -- | The IDs of the security groups.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the association.
    serviceNetworkVpcAssociationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceNetworkVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'updateServiceNetworkVpcAssociation_securityGroupIds' - The IDs of the security groups.
--
-- 'serviceNetworkVpcAssociationIdentifier', 'updateServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier' - The ID or Amazon Resource Name (ARN) of the association.
newUpdateServiceNetworkVpcAssociation ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'serviceNetworkVpcAssociationIdentifier'
  Prelude.Text ->
  UpdateServiceNetworkVpcAssociation
newUpdateServiceNetworkVpcAssociation
  pSecurityGroupIds_
  pServiceNetworkVpcAssociationIdentifier_ =
    UpdateServiceNetworkVpcAssociation'
      { securityGroupIds =
          Lens.coerced
            Lens.# pSecurityGroupIds_,
        serviceNetworkVpcAssociationIdentifier =
          pServiceNetworkVpcAssociationIdentifier_
      }

-- | The IDs of the security groups.
updateServiceNetworkVpcAssociation_securityGroupIds :: Lens.Lens' UpdateServiceNetworkVpcAssociation (Prelude.NonEmpty Prelude.Text)
updateServiceNetworkVpcAssociation_securityGroupIds = Lens.lens (\UpdateServiceNetworkVpcAssociation' {securityGroupIds} -> securityGroupIds) (\s@UpdateServiceNetworkVpcAssociation' {} a -> s {securityGroupIds = a} :: UpdateServiceNetworkVpcAssociation) Prelude.. Lens.coerced

-- | The ID or Amazon Resource Name (ARN) of the association.
updateServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier :: Lens.Lens' UpdateServiceNetworkVpcAssociation Prelude.Text
updateServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier = Lens.lens (\UpdateServiceNetworkVpcAssociation' {serviceNetworkVpcAssociationIdentifier} -> serviceNetworkVpcAssociationIdentifier) (\s@UpdateServiceNetworkVpcAssociation' {} a -> s {serviceNetworkVpcAssociationIdentifier = a} :: UpdateServiceNetworkVpcAssociation)

instance
  Core.AWSRequest
    UpdateServiceNetworkVpcAssociation
  where
  type
    AWSResponse UpdateServiceNetworkVpcAssociation =
      UpdateServiceNetworkVpcAssociationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceNetworkVpcAssociationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdBy")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> ( x
                            Data..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateServiceNetworkVpcAssociation
  where
  hashWithSalt
    _salt
    UpdateServiceNetworkVpcAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` serviceNetworkVpcAssociationIdentifier

instance
  Prelude.NFData
    UpdateServiceNetworkVpcAssociation
  where
  rnf UpdateServiceNetworkVpcAssociation' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf serviceNetworkVpcAssociationIdentifier

instance
  Data.ToHeaders
    UpdateServiceNetworkVpcAssociation
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
  Data.ToJSON
    UpdateServiceNetworkVpcAssociation
  where
  toJSON UpdateServiceNetworkVpcAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("securityGroupIds" Data..= securityGroupIds)
          ]
      )

instance
  Data.ToPath
    UpdateServiceNetworkVpcAssociation
  where
  toPath UpdateServiceNetworkVpcAssociation' {..} =
    Prelude.mconcat
      [ "/servicenetworkvpcassociations/",
        Data.toBS serviceNetworkVpcAssociationIdentifier
      ]

instance
  Data.ToQuery
    UpdateServiceNetworkVpcAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceNetworkVpcAssociationResponse' smart constructor.
data UpdateServiceNetworkVpcAssociationResponse = UpdateServiceNetworkVpcAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The status. You can retry the operation if the status is
    -- @DELETE_FAILED@. However, if you retry it while the status is
    -- @DELETE_IN_PROGRESS@, there is no change in the status.
    status :: Prelude.Maybe ServiceNetworkVpcAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceNetworkVpcAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateServiceNetworkVpcAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdBy', 'updateServiceNetworkVpcAssociationResponse_createdBy' - The account that created the association.
--
-- 'id', 'updateServiceNetworkVpcAssociationResponse_id' - The ID of the association.
--
-- 'securityGroupIds', 'updateServiceNetworkVpcAssociationResponse_securityGroupIds' - The IDs of the security groups.
--
-- 'status', 'updateServiceNetworkVpcAssociationResponse_status' - The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it while the status is
-- @DELETE_IN_PROGRESS@, there is no change in the status.
--
-- 'httpStatus', 'updateServiceNetworkVpcAssociationResponse_httpStatus' - The response's http status code.
newUpdateServiceNetworkVpcAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceNetworkVpcAssociationResponse
newUpdateServiceNetworkVpcAssociationResponse
  pHttpStatus_ =
    UpdateServiceNetworkVpcAssociationResponse'
      { arn =
          Prelude.Nothing,
        createdBy = Prelude.Nothing,
        id = Prelude.Nothing,
        securityGroupIds =
          Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the association.
updateServiceNetworkVpcAssociationResponse_arn :: Lens.Lens' UpdateServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
updateServiceNetworkVpcAssociationResponse_arn = Lens.lens (\UpdateServiceNetworkVpcAssociationResponse' {arn} -> arn) (\s@UpdateServiceNetworkVpcAssociationResponse' {} a -> s {arn = a} :: UpdateServiceNetworkVpcAssociationResponse)

-- | The account that created the association.
updateServiceNetworkVpcAssociationResponse_createdBy :: Lens.Lens' UpdateServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
updateServiceNetworkVpcAssociationResponse_createdBy = Lens.lens (\UpdateServiceNetworkVpcAssociationResponse' {createdBy} -> createdBy) (\s@UpdateServiceNetworkVpcAssociationResponse' {} a -> s {createdBy = a} :: UpdateServiceNetworkVpcAssociationResponse)

-- | The ID of the association.
updateServiceNetworkVpcAssociationResponse_id :: Lens.Lens' UpdateServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
updateServiceNetworkVpcAssociationResponse_id = Lens.lens (\UpdateServiceNetworkVpcAssociationResponse' {id} -> id) (\s@UpdateServiceNetworkVpcAssociationResponse' {} a -> s {id = a} :: UpdateServiceNetworkVpcAssociationResponse)

-- | The IDs of the security groups.
updateServiceNetworkVpcAssociationResponse_securityGroupIds :: Lens.Lens' UpdateServiceNetworkVpcAssociationResponse (Prelude.Maybe [Prelude.Text])
updateServiceNetworkVpcAssociationResponse_securityGroupIds = Lens.lens (\UpdateServiceNetworkVpcAssociationResponse' {securityGroupIds} -> securityGroupIds) (\s@UpdateServiceNetworkVpcAssociationResponse' {} a -> s {securityGroupIds = a} :: UpdateServiceNetworkVpcAssociationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it while the status is
-- @DELETE_IN_PROGRESS@, there is no change in the status.
updateServiceNetworkVpcAssociationResponse_status :: Lens.Lens' UpdateServiceNetworkVpcAssociationResponse (Prelude.Maybe ServiceNetworkVpcAssociationStatus)
updateServiceNetworkVpcAssociationResponse_status = Lens.lens (\UpdateServiceNetworkVpcAssociationResponse' {status} -> status) (\s@UpdateServiceNetworkVpcAssociationResponse' {} a -> s {status = a} :: UpdateServiceNetworkVpcAssociationResponse)

-- | The response's http status code.
updateServiceNetworkVpcAssociationResponse_httpStatus :: Lens.Lens' UpdateServiceNetworkVpcAssociationResponse Prelude.Int
updateServiceNetworkVpcAssociationResponse_httpStatus = Lens.lens (\UpdateServiceNetworkVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceNetworkVpcAssociationResponse' {} a -> s {httpStatus = a} :: UpdateServiceNetworkVpcAssociationResponse)

instance
  Prelude.NFData
    UpdateServiceNetworkVpcAssociationResponse
  where
  rnf UpdateServiceNetworkVpcAssociationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
