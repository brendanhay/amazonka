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
-- Module      : Amazonka.EC2.CreateTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference (route) to a prefix list in a specified transit
-- gateway route table.
module Amazonka.EC2.CreateTransitGatewayPrefixListReference
  ( -- * Creating a Request
    CreateTransitGatewayPrefixListReference (..),
    newCreateTransitGatewayPrefixListReference,

    -- * Request Lenses
    createTransitGatewayPrefixListReference_blackhole,
    createTransitGatewayPrefixListReference_dryRun,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayPrefixListReference' smart constructor.
data CreateTransitGatewayPrefixListReference = CreateTransitGatewayPrefixListReference'
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
-- 'blackhole', 'createTransitGatewayPrefixListReference_blackhole' - Indicates whether to drop traffic that matches this route.
--
-- 'dryRun', 'createTransitGatewayPrefixListReference_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
createTransitGatewayPrefixListReference_blackhole :: Lens.Lens' CreateTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
createTransitGatewayPrefixListReference_blackhole = Lens.lens (\CreateTransitGatewayPrefixListReference' {blackhole} -> blackhole) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {blackhole = a} :: CreateTransitGatewayPrefixListReference)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayPrefixListReference_dryRun :: Lens.Lens' CreateTransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
createTransitGatewayPrefixListReference_dryRun = Lens.lens (\CreateTransitGatewayPrefixListReference' {dryRun} -> dryRun) (\s@CreateTransitGatewayPrefixListReference' {} a -> s {dryRun = a} :: CreateTransitGatewayPrefixListReference)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayPrefixListReferenceResponse'
            Prelude.<$> (x Data..@? "transitGatewayPrefixListReference")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayPrefixListReference
  where
  hashWithSalt
    _salt
    CreateTransitGatewayPrefixListReference' {..} =
      _salt
        `Prelude.hashWithSalt` blackhole
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` transitGatewayRouteTableId
        `Prelude.hashWithSalt` prefixListId

instance
  Prelude.NFData
    CreateTransitGatewayPrefixListReference
  where
  rnf CreateTransitGatewayPrefixListReference' {..} =
    Prelude.rnf blackhole `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf transitGatewayAttachmentId `Prelude.seq`
          Prelude.rnf transitGatewayRouteTableId `Prelude.seq`
            Prelude.rnf prefixListId

instance
  Data.ToHeaders
    CreateTransitGatewayPrefixListReference
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateTransitGatewayPrefixListReference
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateTransitGatewayPrefixListReference
  where
  toQuery CreateTransitGatewayPrefixListReference' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateTransitGatewayPrefixListReference" ::
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
  where
  rnf
    CreateTransitGatewayPrefixListReferenceResponse' {..} =
      Prelude.rnf transitGatewayPrefixListReference `Prelude.seq`
        Prelude.rnf httpStatus
