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
-- Module      : Amazonka.EC2.ModifyTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a reference (route) to a prefix list in a specified transit
-- gateway route table.
module Amazonka.EC2.ModifyTransitGatewayPrefixListReference
  ( -- * Creating a Request
    ModifyTransitGatewayPrefixListReference (..),
    newModifyTransitGatewayPrefixListReference,

    -- * Request Lenses
    modifyTransitGatewayPrefixListReference_blackhole,
    modifyTransitGatewayPrefixListReference_dryRun,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyTransitGatewayPrefixListReference' smart constructor.
data ModifyTransitGatewayPrefixListReference = ModifyTransitGatewayPrefixListReference'
  { -- | Indicates whether to drop traffic that matches this route.
    blackhole :: Prelude.Maybe Prelude.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
-- 'blackhole', 'modifyTransitGatewayPrefixListReference_blackhole' - Indicates whether to drop traffic that matches this route.
--
-- 'dryRun', 'modifyTransitGatewayPrefixListReference_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
      { blackhole =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        transitGatewayAttachmentId =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        prefixListId = pPrefixListId_
      }

-- | Indicates whether to drop traffic that matches this route.
modifyTransitGatewayPrefixListReference_blackhole :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
modifyTransitGatewayPrefixListReference_blackhole = Lens.lens (\ModifyTransitGatewayPrefixListReference' {blackhole} -> blackhole) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {blackhole = a} :: ModifyTransitGatewayPrefixListReference)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTransitGatewayPrefixListReference_dryRun :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
modifyTransitGatewayPrefixListReference_dryRun = Lens.lens (\ModifyTransitGatewayPrefixListReference' {dryRun} -> dryRun) (\s@ModifyTransitGatewayPrefixListReference' {} a -> s {dryRun = a} :: ModifyTransitGatewayPrefixListReference)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayPrefixListReferenceResponse'
            Prelude.<$> (x Data..@? "transitGatewayPrefixListReference")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyTransitGatewayPrefixListReference
  where
  hashWithSalt
    _salt
    ModifyTransitGatewayPrefixListReference' {..} =
      _salt
        `Prelude.hashWithSalt` blackhole
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayRouteTableId
        `Prelude.hashWithSalt` prefixListId

instance
  Prelude.NFData
    ModifyTransitGatewayPrefixListReference
  where
  rnf ModifyTransitGatewayPrefixListReference' {..} =
    Prelude.rnf blackhole
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf prefixListId

instance
  Data.ToHeaders
    ModifyTransitGatewayPrefixListReference
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyTransitGatewayPrefixListReference
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyTransitGatewayPrefixListReference
  where
  toQuery ModifyTransitGatewayPrefixListReference' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyTransitGatewayPrefixListReference" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Blackhole" Data.=: blackhole,
        "DryRun" Data.=: dryRun,
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId,
        "TransitGatewayRouteTableId"
          Data.=: transitGatewayRouteTableId,
        "PrefixListId" Data.=: prefixListId
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
  where
  rnf
    ModifyTransitGatewayPrefixListReferenceResponse' {..} =
      Prelude.rnf transitGatewayPrefixListReference
        `Prelude.seq` Prelude.rnf httpStatus
