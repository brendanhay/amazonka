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
-- Module      : Amazonka.NetworkManager.CreateSiteToSiteVpnAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services site-to-site VPN attachment on an edge
-- location of a core network.
module Amazonka.NetworkManager.CreateSiteToSiteVpnAttachment
  ( -- * Creating a Request
    CreateSiteToSiteVpnAttachment (..),
    newCreateSiteToSiteVpnAttachment,

    -- * Request Lenses
    createSiteToSiteVpnAttachment_tags,
    createSiteToSiteVpnAttachment_clientToken,
    createSiteToSiteVpnAttachment_coreNetworkId,
    createSiteToSiteVpnAttachment_vpnConnectionArn,

    -- * Destructuring the Response
    CreateSiteToSiteVpnAttachmentResponse (..),
    newCreateSiteToSiteVpnAttachmentResponse,

    -- * Response Lenses
    createSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment,
    createSiteToSiteVpnAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSiteToSiteVpnAttachment' smart constructor.
data CreateSiteToSiteVpnAttachment = CreateSiteToSiteVpnAttachment'
  { -- | The tags associated with the request.
    tags :: Prelude.Maybe [Tag],
    -- | The client token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network where you\'re creating a site-to-site VPN
    -- attachment.
    coreNetworkId :: Prelude.Text,
    -- | The ARN identifying the VPN attachment.
    vpnConnectionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSiteToSiteVpnAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSiteToSiteVpnAttachment_tags' - The tags associated with the request.
--
-- 'clientToken', 'createSiteToSiteVpnAttachment_clientToken' - The client token associated with the request.
--
-- 'coreNetworkId', 'createSiteToSiteVpnAttachment_coreNetworkId' - The ID of a core network where you\'re creating a site-to-site VPN
-- attachment.
--
-- 'vpnConnectionArn', 'createSiteToSiteVpnAttachment_vpnConnectionArn' - The ARN identifying the VPN attachment.
newCreateSiteToSiteVpnAttachment ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'vpnConnectionArn'
  Prelude.Text ->
  CreateSiteToSiteVpnAttachment
newCreateSiteToSiteVpnAttachment
  pCoreNetworkId_
  pVpnConnectionArn_ =
    CreateSiteToSiteVpnAttachment'
      { tags =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        coreNetworkId = pCoreNetworkId_,
        vpnConnectionArn = pVpnConnectionArn_
      }

-- | The tags associated with the request.
createSiteToSiteVpnAttachment_tags :: Lens.Lens' CreateSiteToSiteVpnAttachment (Prelude.Maybe [Tag])
createSiteToSiteVpnAttachment_tags = Lens.lens (\CreateSiteToSiteVpnAttachment' {tags} -> tags) (\s@CreateSiteToSiteVpnAttachment' {} a -> s {tags = a} :: CreateSiteToSiteVpnAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The client token associated with the request.
createSiteToSiteVpnAttachment_clientToken :: Lens.Lens' CreateSiteToSiteVpnAttachment (Prelude.Maybe Prelude.Text)
createSiteToSiteVpnAttachment_clientToken = Lens.lens (\CreateSiteToSiteVpnAttachment' {clientToken} -> clientToken) (\s@CreateSiteToSiteVpnAttachment' {} a -> s {clientToken = a} :: CreateSiteToSiteVpnAttachment)

-- | The ID of a core network where you\'re creating a site-to-site VPN
-- attachment.
createSiteToSiteVpnAttachment_coreNetworkId :: Lens.Lens' CreateSiteToSiteVpnAttachment Prelude.Text
createSiteToSiteVpnAttachment_coreNetworkId = Lens.lens (\CreateSiteToSiteVpnAttachment' {coreNetworkId} -> coreNetworkId) (\s@CreateSiteToSiteVpnAttachment' {} a -> s {coreNetworkId = a} :: CreateSiteToSiteVpnAttachment)

-- | The ARN identifying the VPN attachment.
createSiteToSiteVpnAttachment_vpnConnectionArn :: Lens.Lens' CreateSiteToSiteVpnAttachment Prelude.Text
createSiteToSiteVpnAttachment_vpnConnectionArn = Lens.lens (\CreateSiteToSiteVpnAttachment' {vpnConnectionArn} -> vpnConnectionArn) (\s@CreateSiteToSiteVpnAttachment' {} a -> s {vpnConnectionArn = a} :: CreateSiteToSiteVpnAttachment)

instance
  Core.AWSRequest
    CreateSiteToSiteVpnAttachment
  where
  type
    AWSResponse CreateSiteToSiteVpnAttachment =
      CreateSiteToSiteVpnAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSiteToSiteVpnAttachmentResponse'
            Prelude.<$> (x Data..?> "SiteToSiteVpnAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSiteToSiteVpnAttachment
  where
  hashWithSalt _salt CreateSiteToSiteVpnAttachment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` vpnConnectionArn

instance Prelude.NFData CreateSiteToSiteVpnAttachment where
  rnf CreateSiteToSiteVpnAttachment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf vpnConnectionArn

instance Data.ToHeaders CreateSiteToSiteVpnAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSiteToSiteVpnAttachment where
  toJSON CreateSiteToSiteVpnAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("CoreNetworkId" Data..= coreNetworkId),
            Prelude.Just
              ("VpnConnectionArn" Data..= vpnConnectionArn)
          ]
      )

instance Data.ToPath CreateSiteToSiteVpnAttachment where
  toPath =
    Prelude.const "/site-to-site-vpn-attachments"

instance Data.ToQuery CreateSiteToSiteVpnAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSiteToSiteVpnAttachmentResponse' smart constructor.
data CreateSiteToSiteVpnAttachmentResponse = CreateSiteToSiteVpnAttachmentResponse'
  { -- | Details about a site-to-site VPN attachment.
    siteToSiteVpnAttachment :: Prelude.Maybe SiteToSiteVpnAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSiteToSiteVpnAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'siteToSiteVpnAttachment', 'createSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment' - Details about a site-to-site VPN attachment.
--
-- 'httpStatus', 'createSiteToSiteVpnAttachmentResponse_httpStatus' - The response's http status code.
newCreateSiteToSiteVpnAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSiteToSiteVpnAttachmentResponse
newCreateSiteToSiteVpnAttachmentResponse pHttpStatus_ =
  CreateSiteToSiteVpnAttachmentResponse'
    { siteToSiteVpnAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about a site-to-site VPN attachment.
createSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment :: Lens.Lens' CreateSiteToSiteVpnAttachmentResponse (Prelude.Maybe SiteToSiteVpnAttachment)
createSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment = Lens.lens (\CreateSiteToSiteVpnAttachmentResponse' {siteToSiteVpnAttachment} -> siteToSiteVpnAttachment) (\s@CreateSiteToSiteVpnAttachmentResponse' {} a -> s {siteToSiteVpnAttachment = a} :: CreateSiteToSiteVpnAttachmentResponse)

-- | The response's http status code.
createSiteToSiteVpnAttachmentResponse_httpStatus :: Lens.Lens' CreateSiteToSiteVpnAttachmentResponse Prelude.Int
createSiteToSiteVpnAttachmentResponse_httpStatus = Lens.lens (\CreateSiteToSiteVpnAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateSiteToSiteVpnAttachmentResponse' {} a -> s {httpStatus = a} :: CreateSiteToSiteVpnAttachmentResponse)

instance
  Prelude.NFData
    CreateSiteToSiteVpnAttachmentResponse
  where
  rnf CreateSiteToSiteVpnAttachmentResponse' {..} =
    Prelude.rnf siteToSiteVpnAttachment
      `Prelude.seq` Prelude.rnf httpStatus
