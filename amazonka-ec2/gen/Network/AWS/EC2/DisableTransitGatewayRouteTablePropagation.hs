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
-- Module      : Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified resource attachment from propagating routes to
-- the specified propagation route table.
module Network.AWS.EC2.DisableTransitGatewayRouteTablePropagation
  ( -- * Creating a Request
    DisableTransitGatewayRouteTablePropagation (..),
    newDisableTransitGatewayRouteTablePropagation,

    -- * Request Lenses
    disableTransitGatewayRouteTablePropagation_dryRun,
    disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DisableTransitGatewayRouteTablePropagationResponse (..),
    newDisableTransitGatewayRouteTablePropagationResponse,

    -- * Response Lenses
    disableTransitGatewayRouteTablePropagationResponse_propagation,
    disableTransitGatewayRouteTablePropagationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableTransitGatewayRouteTablePropagation' smart constructor.
data DisableTransitGatewayRouteTablePropagation = DisableTransitGatewayRouteTablePropagation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableTransitGatewayRouteTablePropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableTransitGatewayRouteTablePropagation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId' - The ID of the propagation route table.
--
-- 'transitGatewayAttachmentId', 'disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId' - The ID of the attachment.
newDisableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  DisableTransitGatewayRouteTablePropagation
newDisableTransitGatewayRouteTablePropagation
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    DisableTransitGatewayRouteTablePropagation'
      { dryRun =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableTransitGatewayRouteTablePropagation_dryRun :: Lens.Lens' DisableTransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Bool)
disableTransitGatewayRouteTablePropagation_dryRun = Lens.lens (\DisableTransitGatewayRouteTablePropagation' {dryRun} -> dryRun) (\s@DisableTransitGatewayRouteTablePropagation' {} a -> s {dryRun = a} :: DisableTransitGatewayRouteTablePropagation)

-- | The ID of the propagation route table.
disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Prelude.Text
disableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId = Lens.lens (\DisableTransitGatewayRouteTablePropagation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DisableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayRouteTableId = a} :: DisableTransitGatewayRouteTablePropagation)

-- | The ID of the attachment.
disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId :: Lens.Lens' DisableTransitGatewayRouteTablePropagation Prelude.Text
disableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId = Lens.lens (\DisableTransitGatewayRouteTablePropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DisableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayAttachmentId = a} :: DisableTransitGatewayRouteTablePropagation)

instance
  Core.AWSRequest
    DisableTransitGatewayRouteTablePropagation
  where
  type
    AWSResponse
      DisableTransitGatewayRouteTablePropagation =
      DisableTransitGatewayRouteTablePropagationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisableTransitGatewayRouteTablePropagationResponse'
            Prelude.<$> (x Core..@? "propagation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableTransitGatewayRouteTablePropagation

instance
  Prelude.NFData
    DisableTransitGatewayRouteTablePropagation

instance
  Core.ToHeaders
    DisableTransitGatewayRouteTablePropagation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DisableTransitGatewayRouteTablePropagation
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DisableTransitGatewayRouteTablePropagation
  where
  toQuery
    DisableTransitGatewayRouteTablePropagation' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "DisableTransitGatewayRouteTablePropagation" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Core.=: dryRun,
          "TransitGatewayRouteTableId"
            Core.=: transitGatewayRouteTableId,
          "TransitGatewayAttachmentId"
            Core.=: transitGatewayAttachmentId
        ]

-- | /See:/ 'newDisableTransitGatewayRouteTablePropagationResponse' smart constructor.
data DisableTransitGatewayRouteTablePropagationResponse = DisableTransitGatewayRouteTablePropagationResponse'
  { -- | Information about route propagation.
    propagation :: Prelude.Maybe TransitGatewayPropagation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableTransitGatewayRouteTablePropagationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propagation', 'disableTransitGatewayRouteTablePropagationResponse_propagation' - Information about route propagation.
--
-- 'httpStatus', 'disableTransitGatewayRouteTablePropagationResponse_httpStatus' - The response's http status code.
newDisableTransitGatewayRouteTablePropagationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableTransitGatewayRouteTablePropagationResponse
newDisableTransitGatewayRouteTablePropagationResponse
  pHttpStatus_ =
    DisableTransitGatewayRouteTablePropagationResponse'
      { propagation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about route propagation.
disableTransitGatewayRouteTablePropagationResponse_propagation :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse (Prelude.Maybe TransitGatewayPropagation)
disableTransitGatewayRouteTablePropagationResponse_propagation = Lens.lens (\DisableTransitGatewayRouteTablePropagationResponse' {propagation} -> propagation) (\s@DisableTransitGatewayRouteTablePropagationResponse' {} a -> s {propagation = a} :: DisableTransitGatewayRouteTablePropagationResponse)

-- | The response's http status code.
disableTransitGatewayRouteTablePropagationResponse_httpStatus :: Lens.Lens' DisableTransitGatewayRouteTablePropagationResponse Prelude.Int
disableTransitGatewayRouteTablePropagationResponse_httpStatus = Lens.lens (\DisableTransitGatewayRouteTablePropagationResponse' {httpStatus} -> httpStatus) (\s@DisableTransitGatewayRouteTablePropagationResponse' {} a -> s {httpStatus = a} :: DisableTransitGatewayRouteTablePropagationResponse)

instance
  Prelude.NFData
    DisableTransitGatewayRouteTablePropagationResponse
