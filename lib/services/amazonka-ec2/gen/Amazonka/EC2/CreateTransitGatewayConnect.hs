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
-- Module      : Amazonka.EC2.CreateTransitGatewayConnect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Connect attachment from a specified transit gateway
-- attachment. A Connect attachment is a GRE-based tunnel attachment that
-- you can use to establish a connection between a transit gateway and an
-- appliance.
--
-- A Connect attachment uses an existing VPC or Amazon Web Services Direct
-- Connect attachment as the underlying transport mechanism.
module Amazonka.EC2.CreateTransitGatewayConnect
  ( -- * Creating a Request
    CreateTransitGatewayConnect (..),
    newCreateTransitGatewayConnect,

    -- * Request Lenses
    createTransitGatewayConnect_dryRun,
    createTransitGatewayConnect_tagSpecifications,
    createTransitGatewayConnect_transportTransitGatewayAttachmentId,
    createTransitGatewayConnect_options,

    -- * Destructuring the Response
    CreateTransitGatewayConnectResponse (..),
    newCreateTransitGatewayConnectResponse,

    -- * Response Lenses
    createTransitGatewayConnectResponse_transitGatewayConnect,
    createTransitGatewayConnectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayConnect' smart constructor.
data CreateTransitGatewayConnect = CreateTransitGatewayConnect'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the Connect attachment.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the transit gateway attachment. You can specify a VPC
    -- attachment or Amazon Web Services Direct Connect attachment.
    transportTransitGatewayAttachmentId :: Prelude.Text,
    -- | The Connect attachment options.
    options :: CreateTransitGatewayConnectRequestOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayConnect_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createTransitGatewayConnect_tagSpecifications' - The tags to apply to the Connect attachment.
--
-- 'transportTransitGatewayAttachmentId', 'createTransitGatewayConnect_transportTransitGatewayAttachmentId' - The ID of the transit gateway attachment. You can specify a VPC
-- attachment or Amazon Web Services Direct Connect attachment.
--
-- 'options', 'createTransitGatewayConnect_options' - The Connect attachment options.
newCreateTransitGatewayConnect ::
  -- | 'transportTransitGatewayAttachmentId'
  Prelude.Text ->
  -- | 'options'
  CreateTransitGatewayConnectRequestOptions ->
  CreateTransitGatewayConnect
newCreateTransitGatewayConnect
  pTransportTransitGatewayAttachmentId_
  pOptions_ =
    CreateTransitGatewayConnect'
      { dryRun =
          Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        transportTransitGatewayAttachmentId =
          pTransportTransitGatewayAttachmentId_,
        options = pOptions_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayConnect_dryRun :: Lens.Lens' CreateTransitGatewayConnect (Prelude.Maybe Prelude.Bool)
createTransitGatewayConnect_dryRun = Lens.lens (\CreateTransitGatewayConnect' {dryRun} -> dryRun) (\s@CreateTransitGatewayConnect' {} a -> s {dryRun = a} :: CreateTransitGatewayConnect)

-- | The tags to apply to the Connect attachment.
createTransitGatewayConnect_tagSpecifications :: Lens.Lens' CreateTransitGatewayConnect (Prelude.Maybe [TagSpecification])
createTransitGatewayConnect_tagSpecifications = Lens.lens (\CreateTransitGatewayConnect' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayConnect' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayConnect) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway attachment. You can specify a VPC
-- attachment or Amazon Web Services Direct Connect attachment.
createTransitGatewayConnect_transportTransitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayConnect Prelude.Text
createTransitGatewayConnect_transportTransitGatewayAttachmentId = Lens.lens (\CreateTransitGatewayConnect' {transportTransitGatewayAttachmentId} -> transportTransitGatewayAttachmentId) (\s@CreateTransitGatewayConnect' {} a -> s {transportTransitGatewayAttachmentId = a} :: CreateTransitGatewayConnect)

-- | The Connect attachment options.
createTransitGatewayConnect_options :: Lens.Lens' CreateTransitGatewayConnect CreateTransitGatewayConnectRequestOptions
createTransitGatewayConnect_options = Lens.lens (\CreateTransitGatewayConnect' {options} -> options) (\s@CreateTransitGatewayConnect' {} a -> s {options = a} :: CreateTransitGatewayConnect)

instance Core.AWSRequest CreateTransitGatewayConnect where
  type
    AWSResponse CreateTransitGatewayConnect =
      CreateTransitGatewayConnectResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayConnectResponse'
            Prelude.<$> (x Data..@? "transitGatewayConnect")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTransitGatewayConnect where
  hashWithSalt _salt CreateTransitGatewayConnect' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` transportTransitGatewayAttachmentId
      `Prelude.hashWithSalt` options

instance Prelude.NFData CreateTransitGatewayConnect where
  rnf CreateTransitGatewayConnect' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf transportTransitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf options

instance Data.ToHeaders CreateTransitGatewayConnect where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTransitGatewayConnect where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTransitGatewayConnect where
  toQuery CreateTransitGatewayConnect' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateTransitGatewayConnect" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "TransportTransitGatewayAttachmentId"
          Data.=: transportTransitGatewayAttachmentId,
        "Options" Data.=: options
      ]

-- | /See:/ 'newCreateTransitGatewayConnectResponse' smart constructor.
data CreateTransitGatewayConnectResponse = CreateTransitGatewayConnectResponse'
  { -- | Information about the Connect attachment.
    transitGatewayConnect :: Prelude.Maybe TransitGatewayConnect,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayConnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnect', 'createTransitGatewayConnectResponse_transitGatewayConnect' - Information about the Connect attachment.
--
-- 'httpStatus', 'createTransitGatewayConnectResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayConnectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayConnectResponse
newCreateTransitGatewayConnectResponse pHttpStatus_ =
  CreateTransitGatewayConnectResponse'
    { transitGatewayConnect =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Connect attachment.
createTransitGatewayConnectResponse_transitGatewayConnect :: Lens.Lens' CreateTransitGatewayConnectResponse (Prelude.Maybe TransitGatewayConnect)
createTransitGatewayConnectResponse_transitGatewayConnect = Lens.lens (\CreateTransitGatewayConnectResponse' {transitGatewayConnect} -> transitGatewayConnect) (\s@CreateTransitGatewayConnectResponse' {} a -> s {transitGatewayConnect = a} :: CreateTransitGatewayConnectResponse)

-- | The response's http status code.
createTransitGatewayConnectResponse_httpStatus :: Lens.Lens' CreateTransitGatewayConnectResponse Prelude.Int
createTransitGatewayConnectResponse_httpStatus = Lens.lens (\CreateTransitGatewayConnectResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayConnectResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayConnectResponse)

instance
  Prelude.NFData
    CreateTransitGatewayConnectResponse
  where
  rnf CreateTransitGatewayConnectResponse' {..} =
    Prelude.rnf transitGatewayConnect
      `Prelude.seq` Prelude.rnf httpStatus
