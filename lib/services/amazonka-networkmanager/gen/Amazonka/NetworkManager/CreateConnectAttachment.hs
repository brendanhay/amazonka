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
-- Module      : Amazonka.NetworkManager.CreateConnectAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core network Connect attachment from a specified core network
-- attachment.
--
-- A core network Connect attachment is a GRE-based tunnel attachment that
-- you can use to establish a connection between a core network and an
-- appliance. A core network Connect attachment uses an existing VPC
-- attachment as the underlying transport mechanism.
module Amazonka.NetworkManager.CreateConnectAttachment
  ( -- * Creating a Request
    CreateConnectAttachment (..),
    newCreateConnectAttachment,

    -- * Request Lenses
    createConnectAttachment_clientToken,
    createConnectAttachment_tags,
    createConnectAttachment_coreNetworkId,
    createConnectAttachment_edgeLocation,
    createConnectAttachment_transportAttachmentId,
    createConnectAttachment_options,

    -- * Destructuring the Response
    CreateConnectAttachmentResponse (..),
    newCreateConnectAttachmentResponse,

    -- * Response Lenses
    createConnectAttachmentResponse_connectAttachment,
    createConnectAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnectAttachment' smart constructor.
data CreateConnectAttachment = CreateConnectAttachment'
  { -- | The client token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The list of key-value tags associated with the request.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of a core network where you want to create the attachment.
    coreNetworkId :: Prelude.Text,
    -- | The Region where the edge is located.
    edgeLocation :: Prelude.Text,
    -- | The ID of the attachment between the two connections.
    transportAttachmentId :: Prelude.Text,
    -- | Options for creating an attachment.
    options :: ConnectAttachmentOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createConnectAttachment_clientToken' - The client token associated with the request.
--
-- 'tags', 'createConnectAttachment_tags' - The list of key-value tags associated with the request.
--
-- 'coreNetworkId', 'createConnectAttachment_coreNetworkId' - The ID of a core network where you want to create the attachment.
--
-- 'edgeLocation', 'createConnectAttachment_edgeLocation' - The Region where the edge is located.
--
-- 'transportAttachmentId', 'createConnectAttachment_transportAttachmentId' - The ID of the attachment between the two connections.
--
-- 'options', 'createConnectAttachment_options' - Options for creating an attachment.
newCreateConnectAttachment ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'edgeLocation'
  Prelude.Text ->
  -- | 'transportAttachmentId'
  Prelude.Text ->
  -- | 'options'
  ConnectAttachmentOptions ->
  CreateConnectAttachment
newCreateConnectAttachment
  pCoreNetworkId_
  pEdgeLocation_
  pTransportAttachmentId_
  pOptions_ =
    CreateConnectAttachment'
      { clientToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        coreNetworkId = pCoreNetworkId_,
        edgeLocation = pEdgeLocation_,
        transportAttachmentId = pTransportAttachmentId_,
        options = pOptions_
      }

-- | The client token associated with the request.
createConnectAttachment_clientToken :: Lens.Lens' CreateConnectAttachment (Prelude.Maybe Prelude.Text)
createConnectAttachment_clientToken = Lens.lens (\CreateConnectAttachment' {clientToken} -> clientToken) (\s@CreateConnectAttachment' {} a -> s {clientToken = a} :: CreateConnectAttachment)

-- | The list of key-value tags associated with the request.
createConnectAttachment_tags :: Lens.Lens' CreateConnectAttachment (Prelude.Maybe [Tag])
createConnectAttachment_tags = Lens.lens (\CreateConnectAttachment' {tags} -> tags) (\s@CreateConnectAttachment' {} a -> s {tags = a} :: CreateConnectAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a core network where you want to create the attachment.
createConnectAttachment_coreNetworkId :: Lens.Lens' CreateConnectAttachment Prelude.Text
createConnectAttachment_coreNetworkId = Lens.lens (\CreateConnectAttachment' {coreNetworkId} -> coreNetworkId) (\s@CreateConnectAttachment' {} a -> s {coreNetworkId = a} :: CreateConnectAttachment)

-- | The Region where the edge is located.
createConnectAttachment_edgeLocation :: Lens.Lens' CreateConnectAttachment Prelude.Text
createConnectAttachment_edgeLocation = Lens.lens (\CreateConnectAttachment' {edgeLocation} -> edgeLocation) (\s@CreateConnectAttachment' {} a -> s {edgeLocation = a} :: CreateConnectAttachment)

-- | The ID of the attachment between the two connections.
createConnectAttachment_transportAttachmentId :: Lens.Lens' CreateConnectAttachment Prelude.Text
createConnectAttachment_transportAttachmentId = Lens.lens (\CreateConnectAttachment' {transportAttachmentId} -> transportAttachmentId) (\s@CreateConnectAttachment' {} a -> s {transportAttachmentId = a} :: CreateConnectAttachment)

-- | Options for creating an attachment.
createConnectAttachment_options :: Lens.Lens' CreateConnectAttachment ConnectAttachmentOptions
createConnectAttachment_options = Lens.lens (\CreateConnectAttachment' {options} -> options) (\s@CreateConnectAttachment' {} a -> s {options = a} :: CreateConnectAttachment)

instance Core.AWSRequest CreateConnectAttachment where
  type
    AWSResponse CreateConnectAttachment =
      CreateConnectAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectAttachmentResponse'
            Prelude.<$> (x Data..?> "ConnectAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnectAttachment where
  hashWithSalt _salt CreateConnectAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` transportAttachmentId
      `Prelude.hashWithSalt` options

instance Prelude.NFData CreateConnectAttachment where
  rnf CreateConnectAttachment' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf coreNetworkId `Prelude.seq`
          Prelude.rnf edgeLocation `Prelude.seq`
            Prelude.rnf transportAttachmentId `Prelude.seq`
              Prelude.rnf options

instance Data.ToHeaders CreateConnectAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnectAttachment where
  toJSON CreateConnectAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("CoreNetworkId" Data..= coreNetworkId),
            Prelude.Just ("EdgeLocation" Data..= edgeLocation),
            Prelude.Just
              ( "TransportAttachmentId"
                  Data..= transportAttachmentId
              ),
            Prelude.Just ("Options" Data..= options)
          ]
      )

instance Data.ToPath CreateConnectAttachment where
  toPath = Prelude.const "/connect-attachments"

instance Data.ToQuery CreateConnectAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectAttachmentResponse' smart constructor.
data CreateConnectAttachmentResponse = CreateConnectAttachmentResponse'
  { -- | The response to a Connect attachment request.
    connectAttachment :: Prelude.Maybe ConnectAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectAttachment', 'createConnectAttachmentResponse_connectAttachment' - The response to a Connect attachment request.
--
-- 'httpStatus', 'createConnectAttachmentResponse_httpStatus' - The response's http status code.
newCreateConnectAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectAttachmentResponse
newCreateConnectAttachmentResponse pHttpStatus_ =
  CreateConnectAttachmentResponse'
    { connectAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to a Connect attachment request.
createConnectAttachmentResponse_connectAttachment :: Lens.Lens' CreateConnectAttachmentResponse (Prelude.Maybe ConnectAttachment)
createConnectAttachmentResponse_connectAttachment = Lens.lens (\CreateConnectAttachmentResponse' {connectAttachment} -> connectAttachment) (\s@CreateConnectAttachmentResponse' {} a -> s {connectAttachment = a} :: CreateConnectAttachmentResponse)

-- | The response's http status code.
createConnectAttachmentResponse_httpStatus :: Lens.Lens' CreateConnectAttachmentResponse Prelude.Int
createConnectAttachmentResponse_httpStatus = Lens.lens (\CreateConnectAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateConnectAttachmentResponse' {} a -> s {httpStatus = a} :: CreateConnectAttachmentResponse)

instance
  Prelude.NFData
    CreateConnectAttachmentResponse
  where
  rnf CreateConnectAttachmentResponse' {..} =
    Prelude.rnf connectAttachment `Prelude.seq`
      Prelude.rnf httpStatus
