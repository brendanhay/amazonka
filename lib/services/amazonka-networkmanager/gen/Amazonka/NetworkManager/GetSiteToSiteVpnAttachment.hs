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
-- Module      : Amazonka.NetworkManager.GetSiteToSiteVpnAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a site-to-site VPN attachment.
module Amazonka.NetworkManager.GetSiteToSiteVpnAttachment
  ( -- * Creating a Request
    GetSiteToSiteVpnAttachment (..),
    newGetSiteToSiteVpnAttachment,

    -- * Request Lenses
    getSiteToSiteVpnAttachment_attachmentId,

    -- * Destructuring the Response
    GetSiteToSiteVpnAttachmentResponse (..),
    newGetSiteToSiteVpnAttachmentResponse,

    -- * Response Lenses
    getSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment,
    getSiteToSiteVpnAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSiteToSiteVpnAttachment' smart constructor.
data GetSiteToSiteVpnAttachment = GetSiteToSiteVpnAttachment'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSiteToSiteVpnAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'getSiteToSiteVpnAttachment_attachmentId' - The ID of the attachment.
newGetSiteToSiteVpnAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  GetSiteToSiteVpnAttachment
newGetSiteToSiteVpnAttachment pAttachmentId_ =
  GetSiteToSiteVpnAttachment'
    { attachmentId =
        pAttachmentId_
    }

-- | The ID of the attachment.
getSiteToSiteVpnAttachment_attachmentId :: Lens.Lens' GetSiteToSiteVpnAttachment Prelude.Text
getSiteToSiteVpnAttachment_attachmentId = Lens.lens (\GetSiteToSiteVpnAttachment' {attachmentId} -> attachmentId) (\s@GetSiteToSiteVpnAttachment' {} a -> s {attachmentId = a} :: GetSiteToSiteVpnAttachment)

instance Core.AWSRequest GetSiteToSiteVpnAttachment where
  type
    AWSResponse GetSiteToSiteVpnAttachment =
      GetSiteToSiteVpnAttachmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSiteToSiteVpnAttachmentResponse'
            Prelude.<$> (x Data..?> "SiteToSiteVpnAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSiteToSiteVpnAttachment where
  hashWithSalt _salt GetSiteToSiteVpnAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData GetSiteToSiteVpnAttachment where
  rnf GetSiteToSiteVpnAttachment' {..} =
    Prelude.rnf attachmentId

instance Data.ToHeaders GetSiteToSiteVpnAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSiteToSiteVpnAttachment where
  toPath GetSiteToSiteVpnAttachment' {..} =
    Prelude.mconcat
      [ "/site-to-site-vpn-attachments/",
        Data.toBS attachmentId
      ]

instance Data.ToQuery GetSiteToSiteVpnAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSiteToSiteVpnAttachmentResponse' smart constructor.
data GetSiteToSiteVpnAttachmentResponse = GetSiteToSiteVpnAttachmentResponse'
  { -- | Describes the site-to-site attachment.
    siteToSiteVpnAttachment :: Prelude.Maybe SiteToSiteVpnAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSiteToSiteVpnAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'siteToSiteVpnAttachment', 'getSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment' - Describes the site-to-site attachment.
--
-- 'httpStatus', 'getSiteToSiteVpnAttachmentResponse_httpStatus' - The response's http status code.
newGetSiteToSiteVpnAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSiteToSiteVpnAttachmentResponse
newGetSiteToSiteVpnAttachmentResponse pHttpStatus_ =
  GetSiteToSiteVpnAttachmentResponse'
    { siteToSiteVpnAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the site-to-site attachment.
getSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment :: Lens.Lens' GetSiteToSiteVpnAttachmentResponse (Prelude.Maybe SiteToSiteVpnAttachment)
getSiteToSiteVpnAttachmentResponse_siteToSiteVpnAttachment = Lens.lens (\GetSiteToSiteVpnAttachmentResponse' {siteToSiteVpnAttachment} -> siteToSiteVpnAttachment) (\s@GetSiteToSiteVpnAttachmentResponse' {} a -> s {siteToSiteVpnAttachment = a} :: GetSiteToSiteVpnAttachmentResponse)

-- | The response's http status code.
getSiteToSiteVpnAttachmentResponse_httpStatus :: Lens.Lens' GetSiteToSiteVpnAttachmentResponse Prelude.Int
getSiteToSiteVpnAttachmentResponse_httpStatus = Lens.lens (\GetSiteToSiteVpnAttachmentResponse' {httpStatus} -> httpStatus) (\s@GetSiteToSiteVpnAttachmentResponse' {} a -> s {httpStatus = a} :: GetSiteToSiteVpnAttachmentResponse)

instance
  Prelude.NFData
    GetSiteToSiteVpnAttachmentResponse
  where
  rnf GetSiteToSiteVpnAttachmentResponse' {..} =
    Prelude.rnf siteToSiteVpnAttachment
      `Prelude.seq` Prelude.rnf httpStatus
