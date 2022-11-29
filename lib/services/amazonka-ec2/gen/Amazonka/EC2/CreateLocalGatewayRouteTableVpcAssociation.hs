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
-- Module      : Amazonka.EC2.CreateLocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified VPC with the specified local gateway route
-- table.
module Amazonka.EC2.CreateLocalGatewayRouteTableVpcAssociation
  ( -- * Creating a Request
    CreateLocalGatewayRouteTableVpcAssociation (..),
    newCreateLocalGatewayRouteTableVpcAssociation,

    -- * Request Lenses
    createLocalGatewayRouteTableVpcAssociation_dryRun,
    createLocalGatewayRouteTableVpcAssociation_tagSpecifications,
    createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId,
    createLocalGatewayRouteTableVpcAssociation_vpcId,

    -- * Destructuring the Response
    CreateLocalGatewayRouteTableVpcAssociationResponse (..),
    newCreateLocalGatewayRouteTableVpcAssociationResponse,

    -- * Response Lenses
    createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation,
    createLocalGatewayRouteTableVpcAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocalGatewayRouteTableVpcAssociation' smart constructor.
data CreateLocalGatewayRouteTableVpcAssociation = CreateLocalGatewayRouteTableVpcAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the local gateway route table VPC association.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteTableVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createLocalGatewayRouteTableVpcAssociation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createLocalGatewayRouteTableVpcAssociation_tagSpecifications' - The tags to assign to the local gateway route table VPC association.
--
-- 'localGatewayRouteTableId', 'createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'vpcId', 'createLocalGatewayRouteTableVpcAssociation_vpcId' - The ID of the VPC.
newCreateLocalGatewayRouteTableVpcAssociation ::
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateLocalGatewayRouteTableVpcAssociation
newCreateLocalGatewayRouteTableVpcAssociation
  pLocalGatewayRouteTableId_
  pVpcId_ =
    CreateLocalGatewayRouteTableVpcAssociation'
      { dryRun =
          Prelude.Nothing,
        tagSpecifications =
          Prelude.Nothing,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_,
        vpcId = pVpcId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLocalGatewayRouteTableVpcAssociation_dryRun :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Bool)
createLocalGatewayRouteTableVpcAssociation_dryRun = Lens.lens (\CreateLocalGatewayRouteTableVpcAssociation' {dryRun} -> dryRun) (\s@CreateLocalGatewayRouteTableVpcAssociation' {} a -> s {dryRun = a} :: CreateLocalGatewayRouteTableVpcAssociation)

-- | The tags to assign to the local gateway route table VPC association.
createLocalGatewayRouteTableVpcAssociation_tagSpecifications :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation (Prelude.Maybe [TagSpecification])
createLocalGatewayRouteTableVpcAssociation_tagSpecifications = Lens.lens (\CreateLocalGatewayRouteTableVpcAssociation' {tagSpecifications} -> tagSpecifications) (\s@CreateLocalGatewayRouteTableVpcAssociation' {} a -> s {tagSpecifications = a} :: CreateLocalGatewayRouteTableVpcAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the local gateway route table.
createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation Prelude.Text
createLocalGatewayRouteTableVpcAssociation_localGatewayRouteTableId = Lens.lens (\CreateLocalGatewayRouteTableVpcAssociation' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CreateLocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayRouteTableId = a} :: CreateLocalGatewayRouteTableVpcAssociation)

-- | The ID of the VPC.
createLocalGatewayRouteTableVpcAssociation_vpcId :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociation Prelude.Text
createLocalGatewayRouteTableVpcAssociation_vpcId = Lens.lens (\CreateLocalGatewayRouteTableVpcAssociation' {vpcId} -> vpcId) (\s@CreateLocalGatewayRouteTableVpcAssociation' {} a -> s {vpcId = a} :: CreateLocalGatewayRouteTableVpcAssociation)

instance
  Core.AWSRequest
    CreateLocalGatewayRouteTableVpcAssociation
  where
  type
    AWSResponse
      CreateLocalGatewayRouteTableVpcAssociation =
      CreateLocalGatewayRouteTableVpcAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteTableVpcAssociationResponse'
            Prelude.<$> (x Core..@? "localGatewayRouteTableVpcAssociation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLocalGatewayRouteTableVpcAssociation
  where
  hashWithSalt
    _salt
    CreateLocalGatewayRouteTableVpcAssociation' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` localGatewayRouteTableId
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    CreateLocalGatewayRouteTableVpcAssociation
  where
  rnf CreateLocalGatewayRouteTableVpcAssociation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf vpcId

instance
  Core.ToHeaders
    CreateLocalGatewayRouteTableVpcAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    CreateLocalGatewayRouteTableVpcAssociation
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateLocalGatewayRouteTableVpcAssociation
  where
  toQuery
    CreateLocalGatewayRouteTableVpcAssociation' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "CreateLocalGatewayRouteTableVpcAssociation" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Core.=: dryRun,
          Core.toQuery
            ( Core.toQueryList "TagSpecification"
                Prelude.<$> tagSpecifications
            ),
          "LocalGatewayRouteTableId"
            Core.=: localGatewayRouteTableId,
          "VpcId" Core.=: vpcId
        ]

-- | /See:/ 'newCreateLocalGatewayRouteTableVpcAssociationResponse' smart constructor.
data CreateLocalGatewayRouteTableVpcAssociationResponse = CreateLocalGatewayRouteTableVpcAssociationResponse'
  { -- | Information about the association.
    localGatewayRouteTableVpcAssociation :: Prelude.Maybe LocalGatewayRouteTableVpcAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteTableVpcAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableVpcAssociation', 'createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation' - Information about the association.
--
-- 'httpStatus', 'createLocalGatewayRouteTableVpcAssociationResponse_httpStatus' - The response's http status code.
newCreateLocalGatewayRouteTableVpcAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocalGatewayRouteTableVpcAssociationResponse
newCreateLocalGatewayRouteTableVpcAssociationResponse
  pHttpStatus_ =
    CreateLocalGatewayRouteTableVpcAssociationResponse'
      { localGatewayRouteTableVpcAssociation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the association.
createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociationResponse (Prelude.Maybe LocalGatewayRouteTableVpcAssociation)
createLocalGatewayRouteTableVpcAssociationResponse_localGatewayRouteTableVpcAssociation = Lens.lens (\CreateLocalGatewayRouteTableVpcAssociationResponse' {localGatewayRouteTableVpcAssociation} -> localGatewayRouteTableVpcAssociation) (\s@CreateLocalGatewayRouteTableVpcAssociationResponse' {} a -> s {localGatewayRouteTableVpcAssociation = a} :: CreateLocalGatewayRouteTableVpcAssociationResponse)

-- | The response's http status code.
createLocalGatewayRouteTableVpcAssociationResponse_httpStatus :: Lens.Lens' CreateLocalGatewayRouteTableVpcAssociationResponse Prelude.Int
createLocalGatewayRouteTableVpcAssociationResponse_httpStatus = Lens.lens (\CreateLocalGatewayRouteTableVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateLocalGatewayRouteTableVpcAssociationResponse' {} a -> s {httpStatus = a} :: CreateLocalGatewayRouteTableVpcAssociationResponse)

instance
  Prelude.NFData
    CreateLocalGatewayRouteTableVpcAssociationResponse
  where
  rnf
    CreateLocalGatewayRouteTableVpcAssociationResponse' {..} =
      Prelude.rnf localGatewayRouteTableVpcAssociation
        `Prelude.seq` Prelude.rnf httpStatus
