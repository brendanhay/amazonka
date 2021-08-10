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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference (route) to a prefix list in a specified transit
-- gateway route table.
module Network.AWS.EC2.DeleteTransitGatewayPrefixListReference
  ( -- * Creating a Request
    DeleteTransitGatewayPrefixListReference (..),
    newDeleteTransitGatewayPrefixListReference,

    -- * Request Lenses
    deleteTransitGatewayPrefixListReference_dryRun,
    deleteTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    deleteTransitGatewayPrefixListReference_prefixListId,

    -- * Destructuring the Response
    DeleteTransitGatewayPrefixListReferenceResponse (..),
    newDeleteTransitGatewayPrefixListReferenceResponse,

    -- * Response Lenses
    deleteTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    deleteTransitGatewayPrefixListReferenceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayPrefixListReference' smart constructor.
data DeleteTransitGatewayPrefixListReference = DeleteTransitGatewayPrefixListReference'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayPrefixListReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayPrefixListReference_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'deleteTransitGatewayPrefixListReference_transitGatewayRouteTableId' - The ID of the route table.
--
-- 'prefixListId', 'deleteTransitGatewayPrefixListReference_prefixListId' - The ID of the prefix list.
newDeleteTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'prefixListId'
  Prelude.Text ->
  DeleteTransitGatewayPrefixListReference
newDeleteTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    DeleteTransitGatewayPrefixListReference'
      { dryRun =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        prefixListId = pPrefixListId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayPrefixListReference_dryRun :: Lens.Lens' DeleteTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayPrefixListReference_dryRun = Lens.lens (\DeleteTransitGatewayPrefixListReference' {dryRun} -> dryRun) (\s@DeleteTransitGatewayPrefixListReference' {} a -> s {dryRun = a} :: DeleteTransitGatewayPrefixListReference)

-- | The ID of the route table.
deleteTransitGatewayPrefixListReference_transitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayPrefixListReference Prelude.Text
deleteTransitGatewayPrefixListReference_transitGatewayRouteTableId = Lens.lens (\DeleteTransitGatewayPrefixListReference' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DeleteTransitGatewayPrefixListReference' {} a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayPrefixListReference)

-- | The ID of the prefix list.
deleteTransitGatewayPrefixListReference_prefixListId :: Lens.Lens' DeleteTransitGatewayPrefixListReference Prelude.Text
deleteTransitGatewayPrefixListReference_prefixListId = Lens.lens (\DeleteTransitGatewayPrefixListReference' {prefixListId} -> prefixListId) (\s@DeleteTransitGatewayPrefixListReference' {} a -> s {prefixListId = a} :: DeleteTransitGatewayPrefixListReference)

instance
  Core.AWSRequest
    DeleteTransitGatewayPrefixListReference
  where
  type
    AWSResponse
      DeleteTransitGatewayPrefixListReference =
      DeleteTransitGatewayPrefixListReferenceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayPrefixListReferenceResponse'
            Prelude.<$> (x Core..@? "transitGatewayPrefixListReference")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayPrefixListReference

instance
  Prelude.NFData
    DeleteTransitGatewayPrefixListReference

instance
  Core.ToHeaders
    DeleteTransitGatewayPrefixListReference
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteTransitGatewayPrefixListReference
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteTransitGatewayPrefixListReference
  where
  toQuery DeleteTransitGatewayPrefixListReference' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayPrefixListReference" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId,
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newDeleteTransitGatewayPrefixListReferenceResponse' smart constructor.
data DeleteTransitGatewayPrefixListReferenceResponse = DeleteTransitGatewayPrefixListReferenceResponse'
  { -- | Information about the deleted prefix list reference.
    transitGatewayPrefixListReference :: Prelude.Maybe TransitGatewayPrefixListReference,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayPrefixListReferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPrefixListReference', 'deleteTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference' - Information about the deleted prefix list reference.
--
-- 'httpStatus', 'deleteTransitGatewayPrefixListReferenceResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayPrefixListReferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayPrefixListReferenceResponse
newDeleteTransitGatewayPrefixListReferenceResponse
  pHttpStatus_ =
    DeleteTransitGatewayPrefixListReferenceResponse'
      { transitGatewayPrefixListReference =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted prefix list reference.
deleteTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference :: Lens.Lens' DeleteTransitGatewayPrefixListReferenceResponse (Prelude.Maybe TransitGatewayPrefixListReference)
deleteTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference = Lens.lens (\DeleteTransitGatewayPrefixListReferenceResponse' {transitGatewayPrefixListReference} -> transitGatewayPrefixListReference) (\s@DeleteTransitGatewayPrefixListReferenceResponse' {} a -> s {transitGatewayPrefixListReference = a} :: DeleteTransitGatewayPrefixListReferenceResponse)

-- | The response's http status code.
deleteTransitGatewayPrefixListReferenceResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayPrefixListReferenceResponse Prelude.Int
deleteTransitGatewayPrefixListReferenceResponse_httpStatus = Lens.lens (\DeleteTransitGatewayPrefixListReferenceResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayPrefixListReferenceResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayPrefixListReferenceResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayPrefixListReferenceResponse
