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
-- Module      : Amazonka.EC2.EnableTransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified attachment to propagate routes to the specified
-- propagation route table.
module Amazonka.EC2.EnableTransitGatewayRouteTablePropagation
  ( -- * Creating a Request
    EnableTransitGatewayRouteTablePropagation (..),
    newEnableTransitGatewayRouteTablePropagation,

    -- * Request Lenses
    enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId,
    enableTransitGatewayRouteTablePropagation_dryRun,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId,
    enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId,

    -- * Destructuring the Response
    EnableTransitGatewayRouteTablePropagationResponse (..),
    newEnableTransitGatewayRouteTablePropagationResponse,

    -- * Response Lenses
    enableTransitGatewayRouteTablePropagationResponse_propagation,
    enableTransitGatewayRouteTablePropagationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableTransitGatewayRouteTablePropagation' smart constructor.
data EnableTransitGatewayRouteTablePropagation = EnableTransitGatewayRouteTablePropagation'
  { -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway route table announcement.
    transitGatewayRouteTableAnnouncementId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Prelude.Text
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
-- 'transitGatewayAttachmentId', 'enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'dryRun', 'enableTransitGatewayRouteTablePropagation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableAnnouncementId', 'enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId' - The ID of the transit gateway route table announcement.
--
-- 'transitGatewayRouteTableId', 'enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId' - The ID of the propagation route table.
newEnableTransitGatewayRouteTablePropagation ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  EnableTransitGatewayRouteTablePropagation
newEnableTransitGatewayRouteTablePropagation
  pTransitGatewayRouteTableId_ =
    EnableTransitGatewayRouteTablePropagation'
      { transitGatewayAttachmentId =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        transitGatewayRouteTableAnnouncementId =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | The ID of the attachment.
enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
enableTransitGatewayRouteTablePropagation_transitGatewayAttachmentId = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayAttachmentId = a} :: EnableTransitGatewayRouteTablePropagation)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableTransitGatewayRouteTablePropagation_dryRun :: Lens.Lens' EnableTransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Bool)
enableTransitGatewayRouteTablePropagation_dryRun = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {dryRun} -> dryRun) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {dryRun = a} :: EnableTransitGatewayRouteTablePropagation)

-- | The ID of the transit gateway route table announcement.
enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableAnnouncementId = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {transitGatewayRouteTableAnnouncementId} -> transitGatewayRouteTableAnnouncementId) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayRouteTableAnnouncementId = a} :: EnableTransitGatewayRouteTablePropagation)

-- | The ID of the propagation route table.
enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId :: Lens.Lens' EnableTransitGatewayRouteTablePropagation Prelude.Text
enableTransitGatewayRouteTablePropagation_transitGatewayRouteTableId = Lens.lens (\EnableTransitGatewayRouteTablePropagation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@EnableTransitGatewayRouteTablePropagation' {} a -> s {transitGatewayRouteTableId = a} :: EnableTransitGatewayRouteTablePropagation)

instance
  Core.AWSRequest
    EnableTransitGatewayRouteTablePropagation
  where
  type
    AWSResponse
      EnableTransitGatewayRouteTablePropagation =
      EnableTransitGatewayRouteTablePropagationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableTransitGatewayRouteTablePropagationResponse'
            Prelude.<$> (x Data..@? "propagation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableTransitGatewayRouteTablePropagation
  where
  hashWithSalt
    _salt
    EnableTransitGatewayRouteTablePropagation' {..} =
      _salt
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayRouteTableAnnouncementId
        `Prelude.hashWithSalt` transitGatewayRouteTableId

instance
  Prelude.NFData
    EnableTransitGatewayRouteTablePropagation
  where
  rnf EnableTransitGatewayRouteTablePropagation' {..} =
    Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncementId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId

instance
  Data.ToHeaders
    EnableTransitGatewayRouteTablePropagation
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    EnableTransitGatewayRouteTablePropagation
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    EnableTransitGatewayRouteTablePropagation
  where
  toQuery
    EnableTransitGatewayRouteTablePropagation' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "EnableTransitGatewayRouteTablePropagation" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "TransitGatewayAttachmentId"
            Data.=: transitGatewayAttachmentId,
          "DryRun" Data.=: dryRun,
          "TransitGatewayRouteTableAnnouncementId"
            Data.=: transitGatewayRouteTableAnnouncementId,
          "TransitGatewayRouteTableId"
            Data.=: transitGatewayRouteTableId
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
  where
  rnf
    EnableTransitGatewayRouteTablePropagationResponse' {..} =
      Prelude.rnf propagation
        `Prelude.seq` Prelude.rnf httpStatus
