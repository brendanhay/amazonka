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
-- Module      : Amazonka.VPCLattice.CreateServiceNetworkVpcAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a VPC with a service network. When you associate a VPC with
-- the service network, it enables all the resources within that VPC to be
-- clients and communicate with other services in the service network. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-network-associations.html#service-network-vpc-associations Manage VPC associations>
-- in the /Amazon VPC Lattice User Guide/.
--
-- You can\'t use this operation if there is a disassociation in progress.
-- If the association fails, retry by deleting the association and
-- recreating it.
--
-- As a result of this operation, the association gets created in the
-- service network account and the VPC owner account.
--
-- If you add a security group to the service network and VPC association,
-- the association must continue to always have at least one security
-- group. You can add or edit security groups at any time. However, to
-- remove all security groups, you must first delete the association and
-- recreate it without security groups.
module Amazonka.VPCLattice.CreateServiceNetworkVpcAssociation
  ( -- * Creating a Request
    CreateServiceNetworkVpcAssociation (..),
    newCreateServiceNetworkVpcAssociation,

    -- * Request Lenses
    createServiceNetworkVpcAssociation_clientToken,
    createServiceNetworkVpcAssociation_securityGroupIds,
    createServiceNetworkVpcAssociation_tags,
    createServiceNetworkVpcAssociation_serviceNetworkIdentifier,
    createServiceNetworkVpcAssociation_vpcIdentifier,

    -- * Destructuring the Response
    CreateServiceNetworkVpcAssociationResponse (..),
    newCreateServiceNetworkVpcAssociationResponse,

    -- * Response Lenses
    createServiceNetworkVpcAssociationResponse_arn,
    createServiceNetworkVpcAssociationResponse_createdBy,
    createServiceNetworkVpcAssociationResponse_id,
    createServiceNetworkVpcAssociationResponse_securityGroupIds,
    createServiceNetworkVpcAssociationResponse_status,
    createServiceNetworkVpcAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateServiceNetworkVpcAssociation' smart constructor.
data CreateServiceNetworkVpcAssociation = CreateServiceNetworkVpcAssociation'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security groups. Security groups aren\'t added by
    -- default. You can add a security group to apply network level controls to
    -- control which resources in a VPC are allowed to access the service
    -- network and its services. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Control traffic to resources using security groups>
    -- in the /Amazon VPC User Guide/.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags for the association.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID or Amazon Resource Name (ARN) of the service network. You must
    -- use the ARN when the resources specified in the operation are in
    -- different accounts.
    serviceNetworkIdentifier :: Prelude.Text,
    -- | The ID of the VPC.
    vpcIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceNetworkVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createServiceNetworkVpcAssociation_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'securityGroupIds', 'createServiceNetworkVpcAssociation_securityGroupIds' - The IDs of the security groups. Security groups aren\'t added by
-- default. You can add a security group to apply network level controls to
-- control which resources in a VPC are allowed to access the service
-- network and its services. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Control traffic to resources using security groups>
-- in the /Amazon VPC User Guide/.
--
-- 'tags', 'createServiceNetworkVpcAssociation_tags' - The tags for the association.
--
-- 'serviceNetworkIdentifier', 'createServiceNetworkVpcAssociation_serviceNetworkIdentifier' - The ID or Amazon Resource Name (ARN) of the service network. You must
-- use the ARN when the resources specified in the operation are in
-- different accounts.
--
-- 'vpcIdentifier', 'createServiceNetworkVpcAssociation_vpcIdentifier' - The ID of the VPC.
newCreateServiceNetworkVpcAssociation ::
  -- | 'serviceNetworkIdentifier'
  Prelude.Text ->
  -- | 'vpcIdentifier'
  Prelude.Text ->
  CreateServiceNetworkVpcAssociation
newCreateServiceNetworkVpcAssociation
  pServiceNetworkIdentifier_
  pVpcIdentifier_ =
    CreateServiceNetworkVpcAssociation'
      { clientToken =
          Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        serviceNetworkIdentifier =
          pServiceNetworkIdentifier_,
        vpcIdentifier = pVpcIdentifier_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createServiceNetworkVpcAssociation_clientToken :: Lens.Lens' CreateServiceNetworkVpcAssociation (Prelude.Maybe Prelude.Text)
createServiceNetworkVpcAssociation_clientToken = Lens.lens (\CreateServiceNetworkVpcAssociation' {clientToken} -> clientToken) (\s@CreateServiceNetworkVpcAssociation' {} a -> s {clientToken = a} :: CreateServiceNetworkVpcAssociation)

-- | The IDs of the security groups. Security groups aren\'t added by
-- default. You can add a security group to apply network level controls to
-- control which resources in a VPC are allowed to access the service
-- network and its services. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Control traffic to resources using security groups>
-- in the /Amazon VPC User Guide/.
createServiceNetworkVpcAssociation_securityGroupIds :: Lens.Lens' CreateServiceNetworkVpcAssociation (Prelude.Maybe [Prelude.Text])
createServiceNetworkVpcAssociation_securityGroupIds = Lens.lens (\CreateServiceNetworkVpcAssociation' {securityGroupIds} -> securityGroupIds) (\s@CreateServiceNetworkVpcAssociation' {} a -> s {securityGroupIds = a} :: CreateServiceNetworkVpcAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the association.
createServiceNetworkVpcAssociation_tags :: Lens.Lens' CreateServiceNetworkVpcAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createServiceNetworkVpcAssociation_tags = Lens.lens (\CreateServiceNetworkVpcAssociation' {tags} -> tags) (\s@CreateServiceNetworkVpcAssociation' {} a -> s {tags = a} :: CreateServiceNetworkVpcAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID or Amazon Resource Name (ARN) of the service network. You must
-- use the ARN when the resources specified in the operation are in
-- different accounts.
createServiceNetworkVpcAssociation_serviceNetworkIdentifier :: Lens.Lens' CreateServiceNetworkVpcAssociation Prelude.Text
createServiceNetworkVpcAssociation_serviceNetworkIdentifier = Lens.lens (\CreateServiceNetworkVpcAssociation' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@CreateServiceNetworkVpcAssociation' {} a -> s {serviceNetworkIdentifier = a} :: CreateServiceNetworkVpcAssociation)

-- | The ID of the VPC.
createServiceNetworkVpcAssociation_vpcIdentifier :: Lens.Lens' CreateServiceNetworkVpcAssociation Prelude.Text
createServiceNetworkVpcAssociation_vpcIdentifier = Lens.lens (\CreateServiceNetworkVpcAssociation' {vpcIdentifier} -> vpcIdentifier) (\s@CreateServiceNetworkVpcAssociation' {} a -> s {vpcIdentifier = a} :: CreateServiceNetworkVpcAssociation)

instance
  Core.AWSRequest
    CreateServiceNetworkVpcAssociation
  where
  type
    AWSResponse CreateServiceNetworkVpcAssociation =
      CreateServiceNetworkVpcAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceNetworkVpcAssociationResponse'
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
    CreateServiceNetworkVpcAssociation
  where
  hashWithSalt
    _salt
    CreateServiceNetworkVpcAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` serviceNetworkIdentifier
        `Prelude.hashWithSalt` vpcIdentifier

instance
  Prelude.NFData
    CreateServiceNetworkVpcAssociation
  where
  rnf CreateServiceNetworkVpcAssociation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serviceNetworkIdentifier
      `Prelude.seq` Prelude.rnf vpcIdentifier

instance
  Data.ToHeaders
    CreateServiceNetworkVpcAssociation
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
    CreateServiceNetworkVpcAssociation
  where
  toJSON CreateServiceNetworkVpcAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "serviceNetworkIdentifier"
                  Data..= serviceNetworkIdentifier
              ),
            Prelude.Just
              ("vpcIdentifier" Data..= vpcIdentifier)
          ]
      )

instance
  Data.ToPath
    CreateServiceNetworkVpcAssociation
  where
  toPath =
    Prelude.const "/servicenetworkvpcassociations"

instance
  Data.ToQuery
    CreateServiceNetworkVpcAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceNetworkVpcAssociationResponse' smart constructor.
data CreateServiceNetworkVpcAssociationResponse = CreateServiceNetworkVpcAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The operation\'s status.
    status :: Prelude.Maybe ServiceNetworkVpcAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceNetworkVpcAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createServiceNetworkVpcAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdBy', 'createServiceNetworkVpcAssociationResponse_createdBy' - The account that created the association.
--
-- 'id', 'createServiceNetworkVpcAssociationResponse_id' - The ID of the association.
--
-- 'securityGroupIds', 'createServiceNetworkVpcAssociationResponse_securityGroupIds' - The IDs of the security groups.
--
-- 'status', 'createServiceNetworkVpcAssociationResponse_status' - The operation\'s status.
--
-- 'httpStatus', 'createServiceNetworkVpcAssociationResponse_httpStatus' - The response's http status code.
newCreateServiceNetworkVpcAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceNetworkVpcAssociationResponse
newCreateServiceNetworkVpcAssociationResponse
  pHttpStatus_ =
    CreateServiceNetworkVpcAssociationResponse'
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
createServiceNetworkVpcAssociationResponse_arn :: Lens.Lens' CreateServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkVpcAssociationResponse_arn = Lens.lens (\CreateServiceNetworkVpcAssociationResponse' {arn} -> arn) (\s@CreateServiceNetworkVpcAssociationResponse' {} a -> s {arn = a} :: CreateServiceNetworkVpcAssociationResponse)

-- | The account that created the association.
createServiceNetworkVpcAssociationResponse_createdBy :: Lens.Lens' CreateServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkVpcAssociationResponse_createdBy = Lens.lens (\CreateServiceNetworkVpcAssociationResponse' {createdBy} -> createdBy) (\s@CreateServiceNetworkVpcAssociationResponse' {} a -> s {createdBy = a} :: CreateServiceNetworkVpcAssociationResponse)

-- | The ID of the association.
createServiceNetworkVpcAssociationResponse_id :: Lens.Lens' CreateServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkVpcAssociationResponse_id = Lens.lens (\CreateServiceNetworkVpcAssociationResponse' {id} -> id) (\s@CreateServiceNetworkVpcAssociationResponse' {} a -> s {id = a} :: CreateServiceNetworkVpcAssociationResponse)

-- | The IDs of the security groups.
createServiceNetworkVpcAssociationResponse_securityGroupIds :: Lens.Lens' CreateServiceNetworkVpcAssociationResponse (Prelude.Maybe [Prelude.Text])
createServiceNetworkVpcAssociationResponse_securityGroupIds = Lens.lens (\CreateServiceNetworkVpcAssociationResponse' {securityGroupIds} -> securityGroupIds) (\s@CreateServiceNetworkVpcAssociationResponse' {} a -> s {securityGroupIds = a} :: CreateServiceNetworkVpcAssociationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The operation\'s status.
createServiceNetworkVpcAssociationResponse_status :: Lens.Lens' CreateServiceNetworkVpcAssociationResponse (Prelude.Maybe ServiceNetworkVpcAssociationStatus)
createServiceNetworkVpcAssociationResponse_status = Lens.lens (\CreateServiceNetworkVpcAssociationResponse' {status} -> status) (\s@CreateServiceNetworkVpcAssociationResponse' {} a -> s {status = a} :: CreateServiceNetworkVpcAssociationResponse)

-- | The response's http status code.
createServiceNetworkVpcAssociationResponse_httpStatus :: Lens.Lens' CreateServiceNetworkVpcAssociationResponse Prelude.Int
createServiceNetworkVpcAssociationResponse_httpStatus = Lens.lens (\CreateServiceNetworkVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateServiceNetworkVpcAssociationResponse' {} a -> s {httpStatus = a} :: CreateServiceNetworkVpcAssociationResponse)

instance
  Prelude.NFData
    CreateServiceNetworkVpcAssociationResponse
  where
  rnf CreateServiceNetworkVpcAssociationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
