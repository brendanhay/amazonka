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
-- Module      : Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a reference (route) to a prefix list in a specified transit
-- gateway route table.
module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
  ( -- * Creating a Request
    ModifyTransitGatewayPrefixListReference (..),
    newModifyTransitGatewayPrefixListReference,

    -- * Request Lenses
    modifyTransitGatewayPrefixListReference_dryRun,
    modifyTransitGatewayPrefixListReference_blackhole,
    modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    modifyTransitGatewayPrefixListReference_prefixListId,

    -- * Destructuring the Response
    ModifyTransitGatewayPrefixListReferenceResponse (..),
    newModifyTransitGatewayPrefixListReferenceResponse,

    -- * Response Lenses
    modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    modifyTransitGatewayPrefixListReferenceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTransitGatewayPrefixListReference' smart constructor.
data ModifyTransitGatewayPrefixListReference = ModifyTransitGatewayPrefixListReference'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the attachment to which traffic is routed.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayPrefixListReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyTransitGatewayPrefixListReference_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'blackhole', 'modifyTransitGatewayPrefixListReference_blackhole' - Indicates whether to drop traffic that matches this route.
--
-- 'transitGatewayAttachmentId', 'modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId' - The ID of the attachment to which traffic is routed.
--
-- 'transitGatewayRouteTableId', 'modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'prefixListId', 'modifyTransitGatewayPrefixListReference_prefixListId' - The ID of the prefix list.
newModifyTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'prefixListId'
  Prelude.Text ->
  ModifyTransitGatewayPrefixListReference
newModifyTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    ModifyTransitGatewayPrefixListReference'
      { dryRun =
          Prelude.Nothing,
        blackhole = Prelude.Nothing,
        transitGatewayAttachmentId =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        prefixListId = pPrefixListId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTransitGatewayPrefixListReference_dryRun :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
modifyTransitGatewayPrefixListReference_dryRun = Lens.lens (\ModifyTransitGatewayPrefixListReference' {dryRun} -> dryRun) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {dryRun = a} :: ModifyTransitGatewayPrefixListReference)

-- | Indicates whether to drop traffic that matches this route.
modifyTransitGatewayPrefixListReference_blackhole :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
modifyTransitGatewayPrefixListReference_blackhole = Lens.lens (\ModifyTransitGatewayPrefixListReference' {blackhole} -> blackhole) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {blackhole = a} :: ModifyTransitGatewayPrefixListReference)

-- | The ID of the attachment to which traffic is routed.
modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Text)
modifyTransitGatewayPrefixListReference_transitGatewayAttachmentId = Lens.lens (\ModifyTransitGatewayPrefixListReference' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {transitGatewayAttachmentId = a} :: ModifyTransitGatewayPrefixListReference)

-- | The ID of the transit gateway route table.
modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Prelude.Text
modifyTransitGatewayPrefixListReference_transitGatewayRouteTableId = Lens.lens (\ModifyTransitGatewayPrefixListReference' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {transitGatewayRouteTableId = a} :: ModifyTransitGatewayPrefixListReference)

-- | The ID of the prefix list.
modifyTransitGatewayPrefixListReference_prefixListId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Prelude.Text
modifyTransitGatewayPrefixListReference_prefixListId = Lens.lens (\ModifyTransitGatewayPrefixListReference' {prefixListId} -> prefixListId) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {prefixListId = a} :: ModifyTransitGatewayPrefixListReference)

instance
  Core.AWSRequest
    ModifyTransitGatewayPrefixListReference
  where
  type
    AWSResponse
      ModifyTransitGatewayPrefixListReference =
      ModifyTransitGatewayPrefixListReferenceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayPrefixListReferenceResponse'
            Prelude.<$> (x Core..@? "transitGatewayPrefixListReference")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyTransitGatewayPrefixListReference

instance
  Prelude.NFData
    ModifyTransitGatewayPrefixListReference

instance
  Core.ToHeaders
    ModifyTransitGatewayPrefixListReference
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ModifyTransitGatewayPrefixListReference
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyTransitGatewayPrefixListReference
  where
  toQuery ModifyTransitGatewayPrefixListReference' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyTransitGatewayPrefixListReference" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Blackhole" Core.=: blackhole,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId,
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newModifyTransitGatewayPrefixListReferenceResponse' smart constructor.
data ModifyTransitGatewayPrefixListReferenceResponse = ModifyTransitGatewayPrefixListReferenceResponse'
  { -- | Information about the prefix list reference.
    transitGatewayPrefixListReference :: Prelude.Maybe TransitGatewayPrefixListReference,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayPrefixListReferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPrefixListReference', 'modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference' - Information about the prefix list reference.
--
-- 'httpStatus', 'modifyTransitGatewayPrefixListReferenceResponse_httpStatus' - The response's http status code.
newModifyTransitGatewayPrefixListReferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyTransitGatewayPrefixListReferenceResponse
newModifyTransitGatewayPrefixListReferenceResponse
  pHttpStatus_ =
    ModifyTransitGatewayPrefixListReferenceResponse'
      { transitGatewayPrefixListReference =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the prefix list reference.
modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse (Prelude.Maybe TransitGatewayPrefixListReference)
modifyTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference = Lens.lens (\ModifyTransitGatewayPrefixListReferenceResponse' {transitGatewayPrefixListReference} -> transitGatewayPrefixListReference) (\s@ModifyTransitGatewayPrefixListReferenceResponse' {} a -> s {transitGatewayPrefixListReference = a} :: ModifyTransitGatewayPrefixListReferenceResponse)

-- | The response's http status code.
modifyTransitGatewayPrefixListReferenceResponse_httpStatus :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse Prelude.Int
modifyTransitGatewayPrefixListReferenceResponse_httpStatus = Lens.lens (\ModifyTransitGatewayPrefixListReferenceResponse' {httpStatus} -> httpStatus) (\s@ModifyTransitGatewayPrefixListReferenceResponse' {} a -> s {httpStatus = a} :: ModifyTransitGatewayPrefixListReferenceResponse)

instance
  Prelude.NFData
    ModifyTransitGatewayPrefixListReferenceResponse
