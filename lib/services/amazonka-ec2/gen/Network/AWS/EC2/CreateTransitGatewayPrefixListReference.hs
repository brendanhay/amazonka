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
-- Module      : Network.AWS.EC2.CreateTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference (route) to a prefix list in a specified transit
-- gateway route table.
module Network.AWS.EC2.CreateTransitGatewayPrefixListReference
  ( -- * Creating a Request
    CreateTransitGatewayPrefixListReference (..),
    newCreateTransitGatewayPrefixListReference,

    -- * Request Lenses
    createTransitGatewayPrefixListReference_dryRun,
    createTransitGatewayPrefixListReference_blackhole,
    createTransitGatewayPrefixListReference_transitGatewayAttachmentId,
    createTransitGatewayPrefixListReference_transitGatewayRouteTableId,
    createTransitGatewayPrefixListReference_prefixListId,

    -- * Destructuring the Response
    CreateTransitGatewayPrefixListReferenceResponse (..),
    newCreateTransitGatewayPrefixListReferenceResponse,

    -- * Response Lenses
    createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference,
    createTransitGatewayPrefixListReferenceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGatewayPrefixListReference' smart constructor.
data CreateTransitGatewayPrefixListReference = CreateTransitGatewayPrefixListReference'
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
    -- | The ID of the prefix list that is used for destination matches.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPrefixListReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayPrefixListReference_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'blackhole', 'createTransitGatewayPrefixListReference_blackhole' - Indicates whether to drop traffic that matches this route.
--
-- 'transitGatewayAttachmentId', 'createTransitGatewayPrefixListReference_transitGatewayAttachmentId' - The ID of the attachment to which traffic is routed.
--
-- 'transitGatewayRouteTableId', 'createTransitGatewayPrefixListReference_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'prefixListId', 'createTransitGatewayPrefixListReference_prefixListId' - The ID of the prefix list that is used for destination matches.
newCreateTransitGatewayPrefixListReference ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'prefixListId'
  Prelude.Text ->
  CreateTransitGatewayPrefixListReference
newCreateTransitGatewayPrefixListReference
  pTransitGatewayRouteTableId_
  pPrefixListId_ =
    CreateTransitGatewayPrefixListReference'
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
createTransitGatewayPrefixListReference_dryRun :: Lens.Lens' CreateTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
createTransitGatewayPrefixListReference_dryRun = Lens.lens (\CreateTransitGatewayPrefixListReference' {dryRun} -> dryRun) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {dryRun = a} :: CreateTransitGatewayPrefixListReference)

-- | Indicates whether to drop traffic that matches this route.
createTransitGatewayPrefixListReference_blackhole :: Lens.Lens' CreateTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
createTransitGatewayPrefixListReference_blackhole = Lens.lens (\CreateTransitGatewayPrefixListReference' {blackhole} -> blackhole) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {blackhole = a} :: CreateTransitGatewayPrefixListReference)

-- | The ID of the attachment to which traffic is routed.
createTransitGatewayPrefixListReference_transitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Text)
createTransitGatewayPrefixListReference_transitGatewayAttachmentId = Lens.lens (\CreateTransitGatewayPrefixListReference' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {transitGatewayAttachmentId = a} :: CreateTransitGatewayPrefixListReference)

-- | The ID of the transit gateway route table.
createTransitGatewayPrefixListReference_transitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayPrefixListReference Prelude.Text
createTransitGatewayPrefixListReference_transitGatewayRouteTableId = Lens.lens (\CreateTransitGatewayPrefixListReference' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {transitGatewayRouteTableId = a} :: CreateTransitGatewayPrefixListReference)

-- | The ID of the prefix list that is used for destination matches.
createTransitGatewayPrefixListReference_prefixListId :: Lens.Lens' CreateTransitGatewayPrefixListReference Prelude.Text
createTransitGatewayPrefixListReference_prefixListId = Lens.lens (\CreateTransitGatewayPrefixListReference' {prefixListId} -> prefixListId) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {prefixListId = a} :: CreateTransitGatewayPrefixListReference)

instance
  Core.AWSRequest
    CreateTransitGatewayPrefixListReference
  where
  type
    AWSResponse
      CreateTransitGatewayPrefixListReference =
      CreateTransitGatewayPrefixListReferenceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayPrefixListReferenceResponse'
            Prelude.<$> (x Core..@? "transitGatewayPrefixListReference")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayPrefixListReference

instance
  Prelude.NFData
    CreateTransitGatewayPrefixListReference

instance
  Core.ToHeaders
    CreateTransitGatewayPrefixListReference
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    CreateTransitGatewayPrefixListReference
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateTransitGatewayPrefixListReference
  where
  toQuery CreateTransitGatewayPrefixListReference' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateTransitGatewayPrefixListReference" ::
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

-- | /See:/ 'newCreateTransitGatewayPrefixListReferenceResponse' smart constructor.
data CreateTransitGatewayPrefixListReferenceResponse = CreateTransitGatewayPrefixListReferenceResponse'
  { -- | Information about the prefix list reference.
    transitGatewayPrefixListReference :: Prelude.Maybe TransitGatewayPrefixListReference,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPrefixListReferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPrefixListReference', 'createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference' - Information about the prefix list reference.
--
-- 'httpStatus', 'createTransitGatewayPrefixListReferenceResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayPrefixListReferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayPrefixListReferenceResponse
newCreateTransitGatewayPrefixListReferenceResponse
  pHttpStatus_ =
    CreateTransitGatewayPrefixListReferenceResponse'
      { transitGatewayPrefixListReference =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the prefix list reference.
createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference :: Lens.Lens' CreateTransitGatewayPrefixListReferenceResponse (Prelude.Maybe TransitGatewayPrefixListReference)
createTransitGatewayPrefixListReferenceResponse_transitGatewayPrefixListReference = Lens.lens (\CreateTransitGatewayPrefixListReferenceResponse' {transitGatewayPrefixListReference} -> transitGatewayPrefixListReference) (\s@CreateTransitGatewayPrefixListReferenceResponse' {} a -> s {transitGatewayPrefixListReference = a} :: CreateTransitGatewayPrefixListReferenceResponse)

-- | The response's http status code.
createTransitGatewayPrefixListReferenceResponse_httpStatus :: Lens.Lens' CreateTransitGatewayPrefixListReferenceResponse Prelude.Int
createTransitGatewayPrefixListReferenceResponse_httpStatus = Lens.lens (\CreateTransitGatewayPrefixListReferenceResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayPrefixListReferenceResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayPrefixListReferenceResponse)

instance
  Prelude.NFData
    CreateTransitGatewayPrefixListReferenceResponse
