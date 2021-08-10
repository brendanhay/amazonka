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
-- Module      : Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified attachment to propagate routes to the specified
-- propagation route table.
module Network.AWS.EC2.EnableTransitGatewayRouteTablePropagation
  ( -- * Creating a Request
    EnableTransitGatewayRouteTablePropagation (..),
    newEnableTransitGatewayRouteTablePropagation,

    -- * Request Lenses
    enableTransitGatewayRouteTablePropagation_dryRun,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,
    enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,

    -- * Destructuring the Response
    EnableTransitGatewayRouteTablePropagationResponse (..),
    newEnableTransitGatewayRouteTablePropagationResponse,

    -- * Response Lenses
    enableTransitGatewayRouteTablePropagationResponse_propagation,
    enableTransitGatewayRouteTablePropagationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableTransitGatewayRouteTablePropagation' smart constructor.
data EnableTransitGatewayRouteTablePropagation = EnableTransitGatewayRouteTablePropagation'
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
-- Create a value of 'EnableTransitGatewayRouteTablePropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableTransitGatewayRouteTablePropagation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId' - The ID of the propagation route table.
--
-- 'transitGatewayAttachmentId', 'enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId' - The ID of the attachment.
newEnableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  EnableTransitGatewayRouteTablePropagation
newEnableTransitGatewayRouteTablePropagation
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_ =
    EnableTransitGatewayRouteTablePropagation'
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
enableTransitGatewayRouteTablePropagation_dryRun :: Lens.Lens' EnableTransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Bool)
enableTransitGatewayRouteTablePropagation_dryRun = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {dryRun} -> dryRun) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {dryRun = a} :: EnableTransitGatewayRouteTablePropagation)

-- | The ID of the propagation route table.
enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Prelude.Text
enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayRouteTableId = a} :: EnableTransitGatewayRouteTablePropagation)

-- | The ID of the attachment.
enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Prelude.Text
enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayAttachmentId = a} :: EnableTransitGatewayRouteTablePropagation)

instance
  Core.AWSRequest
    EnableTransitGatewayRouteTablePropagation
  where
  type
    AWSResponse
      EnableTransitGatewayRouteTablePropagation =
      EnableTransitGatewayRouteTablePropagationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          EnableTransitGatewayRouteTablePropagationResponse'
            Prelude.<$> (x Core..@? "propagation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableTransitGatewayRouteTablePropagation

instance
  Prelude.NFData
    EnableTransitGatewayRouteTablePropagation

instance
  Core.ToHeaders
    EnableTransitGatewayRouteTablePropagation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    EnableTransitGatewayRouteTablePropagation
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    EnableTransitGatewayRouteTablePropagation
  where
  toQuery
    EnableTransitGatewayRouteTablePropagation' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "EnableTransitGatewayRouteTablePropagation" ::
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

-- | /See:/ 'newEnableTransitGatewayRouteTablePropagationResponse' smart constructor.
data EnableTransitGatewayRouteTablePropagationResponse = EnableTransitGatewayRouteTablePropagationResponse'
  { -- | Information about route propagation.
    propagation :: Prelude.Maybe TransitGatewayPropagation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableTransitGatewayRouteTablePropagationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propagation', 'enableTransitGatewayRouteTablePropagationResponse_propagation' - Information about route propagation.
--
-- 'httpStatus', 'enableTransitGatewayRouteTablePropagationResponse_httpStatus' - The response's http status code.
newEnableTransitGatewayRouteTablePropagationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableTransitGatewayRouteTablePropagationResponse
newEnableTransitGatewayRouteTablePropagationResponse
  pHttpStatus_ =
    EnableTransitGatewayRouteTablePropagationResponse'
      { propagation =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about route propagation.
enableTransitGatewayRouteTablePropagationResponse_propagation :: Lens.Lens' EnableTransitGatewayRouteTablePropagationResponse (Prelude.Maybe TransitGatewayPropagation)
enableTransitGatewayRouteTablePropagationResponse_propagation = Lens.lens (\EnableTransitGatewayRouteTablePropagationResponse' {propagation} -> propagation) (\s@EnableTransitGatewayRouteTablePropagationResponse' {} a -> s {propagation = a} :: EnableTransitGatewayRouteTablePropagationResponse)

-- | The response's http status code.
enableTransitGatewayRouteTablePropagationResponse_httpStatus :: Lens.Lens' EnableTransitGatewayRouteTablePropagationResponse Prelude.Int
enableTransitGatewayRouteTablePropagationResponse_httpStatus = Lens.lens (\EnableTransitGatewayRouteTablePropagationResponse' {httpStatus} -> httpStatus) (\s@EnableTransitGatewayRouteTablePropagationResponse' {} a -> s {httpStatus = a} :: EnableTransitGatewayRouteTablePropagationResponse)

instance
  Prelude.NFData
    EnableTransitGatewayRouteTablePropagationResponse
