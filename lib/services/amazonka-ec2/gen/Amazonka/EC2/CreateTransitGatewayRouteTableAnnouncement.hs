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
-- Module      : Amazonka.EC2.CreateTransitGatewayRouteTableAnnouncement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Advertises a new transit gateway route table.
module Amazonka.EC2.CreateTransitGatewayRouteTableAnnouncement
  ( -- * Creating a Request
    CreateTransitGatewayRouteTableAnnouncement (..),
    newCreateTransitGatewayRouteTableAnnouncement,

    -- * Request Lenses
    createTransitGatewayRouteTableAnnouncement_dryRun,
    createTransitGatewayRouteTableAnnouncement_tagSpecifications,
    createTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableId,
    createTransitGatewayRouteTableAnnouncement_peeringAttachmentId,

    -- * Destructuring the Response
    CreateTransitGatewayRouteTableAnnouncementResponse (..),
    newCreateTransitGatewayRouteTableAnnouncementResponse,

    -- * Response Lenses
    createTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement,
    createTransitGatewayRouteTableAnnouncementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayRouteTableAnnouncement' smart constructor.
data CreateTransitGatewayRouteTableAnnouncement = CreateTransitGatewayRouteTableAnnouncement'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags specifications applied to the transit gateway route table
    -- announcement.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The ID of the peering attachment.
    peeringAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteTableAnnouncement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayRouteTableAnnouncement_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createTransitGatewayRouteTableAnnouncement_tagSpecifications' - The tags specifications applied to the transit gateway route table
-- announcement.
--
-- 'transitGatewayRouteTableId', 'createTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'peeringAttachmentId', 'createTransitGatewayRouteTableAnnouncement_peeringAttachmentId' - The ID of the peering attachment.
newCreateTransitGatewayRouteTableAnnouncement ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'peeringAttachmentId'
  Prelude.Text ->
  CreateTransitGatewayRouteTableAnnouncement
newCreateTransitGatewayRouteTableAnnouncement
  pTransitGatewayRouteTableId_
  pPeeringAttachmentId_ =
    CreateTransitGatewayRouteTableAnnouncement'
      { dryRun =
          Prelude.Nothing,
        tagSpecifications =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        peeringAttachmentId =
          pPeeringAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayRouteTableAnnouncement_dryRun :: Lens.Lens' CreateTransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Bool)
createTransitGatewayRouteTableAnnouncement_dryRun = Lens.lens (\CreateTransitGatewayRouteTableAnnouncement' {dryRun} -> dryRun) (\s@CreateTransitGatewayRouteTableAnnouncement' {} a -> s {dryRun = a} :: CreateTransitGatewayRouteTableAnnouncement)

-- | The tags specifications applied to the transit gateway route table
-- announcement.
createTransitGatewayRouteTableAnnouncement_tagSpecifications :: Lens.Lens' CreateTransitGatewayRouteTableAnnouncement (Prelude.Maybe [TagSpecification])
createTransitGatewayRouteTableAnnouncement_tagSpecifications = Lens.lens (\CreateTransitGatewayRouteTableAnnouncement' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayRouteTableAnnouncement' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayRouteTableAnnouncement) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway route table.
createTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayRouteTableAnnouncement Prelude.Text
createTransitGatewayRouteTableAnnouncement_transitGatewayRouteTableId = Lens.lens (\CreateTransitGatewayRouteTableAnnouncement' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@CreateTransitGatewayRouteTableAnnouncement' {} a -> s {transitGatewayRouteTableId = a} :: CreateTransitGatewayRouteTableAnnouncement)

-- | The ID of the peering attachment.
createTransitGatewayRouteTableAnnouncement_peeringAttachmentId :: Lens.Lens' CreateTransitGatewayRouteTableAnnouncement Prelude.Text
createTransitGatewayRouteTableAnnouncement_peeringAttachmentId = Lens.lens (\CreateTransitGatewayRouteTableAnnouncement' {peeringAttachmentId} -> peeringAttachmentId) (\s@CreateTransitGatewayRouteTableAnnouncement' {} a -> s {peeringAttachmentId = a} :: CreateTransitGatewayRouteTableAnnouncement)

instance
  Core.AWSRequest
    CreateTransitGatewayRouteTableAnnouncement
  where
  type
    AWSResponse
      CreateTransitGatewayRouteTableAnnouncement =
      CreateTransitGatewayRouteTableAnnouncementResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteTableAnnouncementResponse'
            Prelude.<$> (x Data..@? "transitGatewayRouteTableAnnouncement")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayRouteTableAnnouncement
  where
  hashWithSalt
    _salt
    CreateTransitGatewayRouteTableAnnouncement' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` transitGatewayRouteTableId
        `Prelude.hashWithSalt` peeringAttachmentId

instance
  Prelude.NFData
    CreateTransitGatewayRouteTableAnnouncement
  where
  rnf CreateTransitGatewayRouteTableAnnouncement' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf peeringAttachmentId

instance
  Data.ToHeaders
    CreateTransitGatewayRouteTableAnnouncement
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateTransitGatewayRouteTableAnnouncement
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateTransitGatewayRouteTableAnnouncement
  where
  toQuery
    CreateTransitGatewayRouteTableAnnouncement' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "CreateTransitGatewayRouteTableAnnouncement" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            ( Data.toQueryList "TagSpecification"
                Prelude.<$> tagSpecifications
            ),
          "TransitGatewayRouteTableId"
            Data.=: transitGatewayRouteTableId,
          "PeeringAttachmentId" Data.=: peeringAttachmentId
        ]

-- | /See:/ 'newCreateTransitGatewayRouteTableAnnouncementResponse' smart constructor.
data CreateTransitGatewayRouteTableAnnouncementResponse = CreateTransitGatewayRouteTableAnnouncementResponse'
  { -- | Provides details about the transit gateway route table announcement.
    transitGatewayRouteTableAnnouncement :: Prelude.Maybe TransitGatewayRouteTableAnnouncement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteTableAnnouncementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTableAnnouncement', 'createTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement' - Provides details about the transit gateway route table announcement.
--
-- 'httpStatus', 'createTransitGatewayRouteTableAnnouncementResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayRouteTableAnnouncementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayRouteTableAnnouncementResponse
newCreateTransitGatewayRouteTableAnnouncementResponse
  pHttpStatus_ =
    CreateTransitGatewayRouteTableAnnouncementResponse'
      { transitGatewayRouteTableAnnouncement =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Provides details about the transit gateway route table announcement.
createTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement :: Lens.Lens' CreateTransitGatewayRouteTableAnnouncementResponse (Prelude.Maybe TransitGatewayRouteTableAnnouncement)
createTransitGatewayRouteTableAnnouncementResponse_transitGatewayRouteTableAnnouncement = Lens.lens (\CreateTransitGatewayRouteTableAnnouncementResponse' {transitGatewayRouteTableAnnouncement} -> transitGatewayRouteTableAnnouncement) (\s@CreateTransitGatewayRouteTableAnnouncementResponse' {} a -> s {transitGatewayRouteTableAnnouncement = a} :: CreateTransitGatewayRouteTableAnnouncementResponse)

-- | The response's http status code.
createTransitGatewayRouteTableAnnouncementResponse_httpStatus :: Lens.Lens' CreateTransitGatewayRouteTableAnnouncementResponse Prelude.Int
createTransitGatewayRouteTableAnnouncementResponse_httpStatus = Lens.lens (\CreateTransitGatewayRouteTableAnnouncementResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayRouteTableAnnouncementResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayRouteTableAnnouncementResponse)

instance
  Prelude.NFData
    CreateTransitGatewayRouteTableAnnouncementResponse
  where
  rnf
    CreateTransitGatewayRouteTableAnnouncementResponse' {..} =
      Prelude.rnf transitGatewayRouteTableAnnouncement
        `Prelude.seq` Prelude.rnf httpStatus
