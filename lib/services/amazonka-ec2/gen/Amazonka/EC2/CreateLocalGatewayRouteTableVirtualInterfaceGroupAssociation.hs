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
-- Module      : Amazonka.EC2.CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a local gateway route table virtual interface group association.
module Amazonka.EC2.CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  ( -- * Creating a Request
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),
    newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation,

    -- * Request Lenses
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_tagSpecifications,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId,

    -- * Destructuring the Response
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse (..),
    newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse,

    -- * Response Lenses
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation,
    createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation = CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags assigned to the local gateway route table virtual interface
    -- group association.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the local gateway route table virtual interface group
    -- association.
    localGatewayVirtualInterfaceGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_tagSpecifications' - The tags assigned to the local gateway route table virtual interface
-- group association.
--
-- 'localGatewayRouteTableId', 'createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'localGatewayVirtualInterfaceGroupId', 'createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId' - The ID of the local gateway route table virtual interface group
-- association.
newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation ::
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  -- | 'localGatewayVirtualInterfaceGroupId'
  Prelude.Text ->
  CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  pLocalGatewayRouteTableId_
  pLocalGatewayVirtualInterfaceGroupId_ =
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation'
      { dryRun =
          Prelude.Nothing,
        tagSpecifications =
          Prelude.Nothing,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_,
        localGatewayVirtualInterfaceGroupId =
          pLocalGatewayVirtualInterfaceGroupId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun :: Lens.Lens' CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Bool)
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_dryRun = Lens.lens (\CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {dryRun} -> dryRun) (\s@CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {dryRun = a} :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The tags assigned to the local gateway route table virtual interface
-- group association.
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_tagSpecifications :: Lens.Lens' CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe [TagSpecification])
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_tagSpecifications = Lens.lens (\CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {tagSpecifications} -> tagSpecifications) (\s@CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {tagSpecifications = a} :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the local gateway route table.
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation Prelude.Text
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId = Lens.lens (\CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableId = a} :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway route table virtual interface group
-- association.
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId :: Lens.Lens' CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation Prelude.Text
createLocalGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId = Lens.lens (\CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation)

instance
  Core.AWSRequest
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  type
    AWSResponse
      CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
      CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayRouteTableVirtualInterfaceGroupAssociation"
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  hashWithSalt
    _salt
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` localGatewayRouteTableId
        `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId

instance
  Prelude.NFData
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  rnf
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf tagSpecifications
        `Prelude.seq` Prelude.rnf localGatewayRouteTableId
        `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId

instance
  Data.ToHeaders
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  toQuery
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociation" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            ( Data.toQueryList "TagSpecification"
                Prelude.<$> tagSpecifications
            ),
          "LocalGatewayRouteTableId"
            Data.=: localGatewayRouteTableId,
          "LocalGatewayVirtualInterfaceGroupId"
            Data.=: localGatewayVirtualInterfaceGroupId
        ]

-- | /See:/ 'newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' smart constructor.
data CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse = CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'
  { localGatewayRouteTableVirtualInterfaceGroupAssociation :: Prelude.Maybe LocalGatewayRouteTableVirtualInterfaceGroupAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociation', 'createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation' - Undocumented member.
--
-- 'httpStatus', 'createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus' - The response's http status code.
newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
newCreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
  pHttpStatus_ =
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse'
      { localGatewayRouteTableVirtualInterfaceGroupAssociation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation :: Lens.Lens' CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse (Prelude.Maybe LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_localGatewayRouteTableVirtualInterfaceGroupAssociation = Lens.lens (\CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {localGatewayRouteTableVirtualInterfaceGroupAssociation} -> localGatewayRouteTableVirtualInterfaceGroupAssociation) (\s@CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociation = a} :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse)

-- | The response's http status code.
createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus :: Lens.Lens' CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse Prelude.Int
createLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse_httpStatus = Lens.lens (\CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {} a -> s {httpStatus = a} :: CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse)

instance
  Prelude.NFData
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse
  where
  rnf
    CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResponse' {..} =
      Prelude.rnf
        localGatewayRouteTableVirtualInterfaceGroupAssociation
        `Prelude.seq` Prelude.rnf httpStatus
