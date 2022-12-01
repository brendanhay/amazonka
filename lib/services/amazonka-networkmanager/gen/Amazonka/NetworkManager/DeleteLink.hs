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
-- Module      : Amazonka.NetworkManager.DeleteLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing link. You must first disassociate the link from any
-- devices and customer gateways.
module Amazonka.NetworkManager.DeleteLink
  ( -- * Creating a Request
    DeleteLink (..),
    newDeleteLink,

    -- * Request Lenses
    deleteLink_globalNetworkId,
    deleteLink_linkId,

    -- * Destructuring the Response
    DeleteLinkResponse (..),
    newDeleteLinkResponse,

    -- * Response Lenses
    deleteLinkResponse_link,
    deleteLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLink' smart constructor.
data DeleteLink = DeleteLink'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'deleteLink_globalNetworkId' - The ID of the global network.
--
-- 'linkId', 'deleteLink_linkId' - The ID of the link.
newDeleteLink ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'linkId'
  Prelude.Text ->
  DeleteLink
newDeleteLink pGlobalNetworkId_ pLinkId_ =
  DeleteLink'
    { globalNetworkId = pGlobalNetworkId_,
      linkId = pLinkId_
    }

-- | The ID of the global network.
deleteLink_globalNetworkId :: Lens.Lens' DeleteLink Prelude.Text
deleteLink_globalNetworkId = Lens.lens (\DeleteLink' {globalNetworkId} -> globalNetworkId) (\s@DeleteLink' {} a -> s {globalNetworkId = a} :: DeleteLink)

-- | The ID of the link.
deleteLink_linkId :: Lens.Lens' DeleteLink Prelude.Text
deleteLink_linkId = Lens.lens (\DeleteLink' {linkId} -> linkId) (\s@DeleteLink' {} a -> s {linkId = a} :: DeleteLink)

instance Core.AWSRequest DeleteLink where
  type AWSResponse DeleteLink = DeleteLinkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLinkResponse'
            Prelude.<$> (x Core..?> "Link")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLink where
  hashWithSalt _salt DeleteLink' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` linkId

instance Prelude.NFData DeleteLink where
  rnf DeleteLink' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf linkId

instance Core.ToHeaders DeleteLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteLink where
  toPath DeleteLink' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/links/",
        Core.toBS linkId
      ]

instance Core.ToQuery DeleteLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLinkResponse' smart constructor.
data DeleteLinkResponse = DeleteLinkResponse'
  { -- | Information about the link.
    link :: Prelude.Maybe Link,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'link', 'deleteLinkResponse_link' - Information about the link.
--
-- 'httpStatus', 'deleteLinkResponse_httpStatus' - The response's http status code.
newDeleteLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLinkResponse
newDeleteLinkResponse pHttpStatus_ =
  DeleteLinkResponse'
    { link = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the link.
deleteLinkResponse_link :: Lens.Lens' DeleteLinkResponse (Prelude.Maybe Link)
deleteLinkResponse_link = Lens.lens (\DeleteLinkResponse' {link} -> link) (\s@DeleteLinkResponse' {} a -> s {link = a} :: DeleteLinkResponse)

-- | The response's http status code.
deleteLinkResponse_httpStatus :: Lens.Lens' DeleteLinkResponse Prelude.Int
deleteLinkResponse_httpStatus = Lens.lens (\DeleteLinkResponse' {httpStatus} -> httpStatus) (\s@DeleteLinkResponse' {} a -> s {httpStatus = a} :: DeleteLinkResponse)

instance Prelude.NFData DeleteLinkResponse where
  rnf DeleteLinkResponse' {..} =
    Prelude.rnf link
      `Prelude.seq` Prelude.rnf httpStatus
