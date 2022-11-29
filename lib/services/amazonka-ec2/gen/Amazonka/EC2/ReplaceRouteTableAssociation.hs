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
-- Module      : Amazonka.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the route table associated with a given subnet, internet
-- gateway, or virtual private gateway in a VPC. After the operation
-- completes, the subnet or gateway uses the routes in the new route table.
-- For more information about route tables, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can also use this operation to change which table is the main route
-- table in the VPC. Specify the main route table\'s association ID and the
-- route table ID of the new main route table.
module Amazonka.EC2.ReplaceRouteTableAssociation
  ( -- * Creating a Request
    ReplaceRouteTableAssociation (..),
    newReplaceRouteTableAssociation,

    -- * Request Lenses
    replaceRouteTableAssociation_dryRun,
    replaceRouteTableAssociation_associationId,
    replaceRouteTableAssociation_routeTableId,

    -- * Destructuring the Response
    ReplaceRouteTableAssociationResponse (..),
    newReplaceRouteTableAssociationResponse,

    -- * Response Lenses
    replaceRouteTableAssociationResponse_newAssociationId,
    replaceRouteTableAssociationResponse_associationState,
    replaceRouteTableAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplaceRouteTableAssociation' smart constructor.
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The association ID.
    associationId :: Prelude.Text,
    -- | The ID of the new route table to associate with the subnet.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceRouteTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'replaceRouteTableAssociation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'associationId', 'replaceRouteTableAssociation_associationId' - The association ID.
--
-- 'routeTableId', 'replaceRouteTableAssociation_routeTableId' - The ID of the new route table to associate with the subnet.
newReplaceRouteTableAssociation ::
  -- | 'associationId'
  Prelude.Text ->
  -- | 'routeTableId'
  Prelude.Text ->
  ReplaceRouteTableAssociation
newReplaceRouteTableAssociation
  pAssociationId_
  pRouteTableId_ =
    ReplaceRouteTableAssociation'
      { dryRun =
          Prelude.Nothing,
        associationId = pAssociationId_,
        routeTableId = pRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
replaceRouteTableAssociation_dryRun :: Lens.Lens' ReplaceRouteTableAssociation (Prelude.Maybe Prelude.Bool)
replaceRouteTableAssociation_dryRun = Lens.lens (\ReplaceRouteTableAssociation' {dryRun} -> dryRun) (\s@ReplaceRouteTableAssociation' {} a -> s {dryRun = a} :: ReplaceRouteTableAssociation)

-- | The association ID.
replaceRouteTableAssociation_associationId :: Lens.Lens' ReplaceRouteTableAssociation Prelude.Text
replaceRouteTableAssociation_associationId = Lens.lens (\ReplaceRouteTableAssociation' {associationId} -> associationId) (\s@ReplaceRouteTableAssociation' {} a -> s {associationId = a} :: ReplaceRouteTableAssociation)

-- | The ID of the new route table to associate with the subnet.
replaceRouteTableAssociation_routeTableId :: Lens.Lens' ReplaceRouteTableAssociation Prelude.Text
replaceRouteTableAssociation_routeTableId = Lens.lens (\ReplaceRouteTableAssociation' {routeTableId} -> routeTableId) (\s@ReplaceRouteTableAssociation' {} a -> s {routeTableId = a} :: ReplaceRouteTableAssociation)

instance Core.AWSRequest ReplaceRouteTableAssociation where
  type
    AWSResponse ReplaceRouteTableAssociation =
      ReplaceRouteTableAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ReplaceRouteTableAssociationResponse'
            Prelude.<$> (x Core..@? "newAssociationId")
            Prelude.<*> (x Core..@? "associationState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ReplaceRouteTableAssociation
  where
  hashWithSalt _salt ReplaceRouteTableAssociation' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData ReplaceRouteTableAssociation where
  rnf ReplaceRouteTableAssociation' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf routeTableId

instance Core.ToHeaders ReplaceRouteTableAssociation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ReplaceRouteTableAssociation where
  toPath = Prelude.const "/"

instance Core.ToQuery ReplaceRouteTableAssociation where
  toQuery ReplaceRouteTableAssociation' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ReplaceRouteTableAssociation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "AssociationId" Core.=: associationId,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newReplaceRouteTableAssociationResponse' smart constructor.
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
  { -- | The ID of the new association.
    newAssociationId' :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    associationState :: Prelude.Maybe RouteTableAssociationState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceRouteTableAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newAssociationId'', 'replaceRouteTableAssociationResponse_newAssociationId' - The ID of the new association.
--
-- 'associationState', 'replaceRouteTableAssociationResponse_associationState' - The state of the association.
--
-- 'httpStatus', 'replaceRouteTableAssociationResponse_httpStatus' - The response's http status code.
newReplaceRouteTableAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplaceRouteTableAssociationResponse
newReplaceRouteTableAssociationResponse pHttpStatus_ =
  ReplaceRouteTableAssociationResponse'
    { newAssociationId' =
        Prelude.Nothing,
      associationState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new association.
replaceRouteTableAssociationResponse_newAssociationId :: Lens.Lens' ReplaceRouteTableAssociationResponse (Prelude.Maybe Prelude.Text)
replaceRouteTableAssociationResponse_newAssociationId = Lens.lens (\ReplaceRouteTableAssociationResponse' {newAssociationId'} -> newAssociationId') (\s@ReplaceRouteTableAssociationResponse' {} a -> s {newAssociationId' = a} :: ReplaceRouteTableAssociationResponse)

-- | The state of the association.
replaceRouteTableAssociationResponse_associationState :: Lens.Lens' ReplaceRouteTableAssociationResponse (Prelude.Maybe RouteTableAssociationState)
replaceRouteTableAssociationResponse_associationState = Lens.lens (\ReplaceRouteTableAssociationResponse' {associationState} -> associationState) (\s@ReplaceRouteTableAssociationResponse' {} a -> s {associationState = a} :: ReplaceRouteTableAssociationResponse)

-- | The response's http status code.
replaceRouteTableAssociationResponse_httpStatus :: Lens.Lens' ReplaceRouteTableAssociationResponse Prelude.Int
replaceRouteTableAssociationResponse_httpStatus = Lens.lens (\ReplaceRouteTableAssociationResponse' {httpStatus} -> httpStatus) (\s@ReplaceRouteTableAssociationResponse' {} a -> s {httpStatus = a} :: ReplaceRouteTableAssociationResponse)

instance
  Prelude.NFData
    ReplaceRouteTableAssociationResponse
  where
  rnf ReplaceRouteTableAssociationResponse' {..} =
    Prelude.rnf newAssociationId'
      `Prelude.seq` Prelude.rnf associationState
      `Prelude.seq` Prelude.rnf httpStatus
