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
-- Module      : Amazonka.NetworkManager.DeleteSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing site. The site cannot be associated with any device
-- or link.
module Amazonka.NetworkManager.DeleteSite
  ( -- * Creating a Request
    DeleteSite (..),
    newDeleteSite,

    -- * Request Lenses
    deleteSite_globalNetworkId,
    deleteSite_siteId,

    -- * Destructuring the Response
    DeleteSiteResponse (..),
    newDeleteSiteResponse,

    -- * Response Lenses
    deleteSiteResponse_site,
    deleteSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSite' smart constructor.
data DeleteSite = DeleteSite'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'deleteSite_globalNetworkId' - The ID of the global network.
--
-- 'siteId', 'deleteSite_siteId' - The ID of the site.
newDeleteSite ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'siteId'
  Prelude.Text ->
  DeleteSite
newDeleteSite pGlobalNetworkId_ pSiteId_ =
  DeleteSite'
    { globalNetworkId = pGlobalNetworkId_,
      siteId = pSiteId_
    }

-- | The ID of the global network.
deleteSite_globalNetworkId :: Lens.Lens' DeleteSite Prelude.Text
deleteSite_globalNetworkId = Lens.lens (\DeleteSite' {globalNetworkId} -> globalNetworkId) (\s@DeleteSite' {} a -> s {globalNetworkId = a} :: DeleteSite)

-- | The ID of the site.
deleteSite_siteId :: Lens.Lens' DeleteSite Prelude.Text
deleteSite_siteId = Lens.lens (\DeleteSite' {siteId} -> siteId) (\s@DeleteSite' {} a -> s {siteId = a} :: DeleteSite)

instance Core.AWSRequest DeleteSite where
  type AWSResponse DeleteSite = DeleteSiteResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSiteResponse'
            Prelude.<$> (x Core..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSite where
  hashWithSalt _salt DeleteSite' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` siteId

instance Prelude.NFData DeleteSite where
  rnf DeleteSite' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf siteId

instance Core.ToHeaders DeleteSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteSite where
  toPath DeleteSite' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/sites/",
        Core.toBS siteId
      ]

instance Core.ToQuery DeleteSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSiteResponse' smart constructor.
data DeleteSiteResponse = DeleteSiteResponse'
  { -- | Information about the site.
    site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'site', 'deleteSiteResponse_site' - Information about the site.
--
-- 'httpStatus', 'deleteSiteResponse_httpStatus' - The response's http status code.
newDeleteSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSiteResponse
newDeleteSiteResponse pHttpStatus_ =
  DeleteSiteResponse'
    { site = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the site.
deleteSiteResponse_site :: Lens.Lens' DeleteSiteResponse (Prelude.Maybe Site)
deleteSiteResponse_site = Lens.lens (\DeleteSiteResponse' {site} -> site) (\s@DeleteSiteResponse' {} a -> s {site = a} :: DeleteSiteResponse)

-- | The response's http status code.
deleteSiteResponse_httpStatus :: Lens.Lens' DeleteSiteResponse Prelude.Int
deleteSiteResponse_httpStatus = Lens.lens (\DeleteSiteResponse' {httpStatus} -> httpStatus) (\s@DeleteSiteResponse' {} a -> s {httpStatus = a} :: DeleteSiteResponse)

instance Prelude.NFData DeleteSiteResponse where
  rnf DeleteSiteResponse' {..} =
    Prelude.rnf site
      `Prelude.seq` Prelude.rnf httpStatus
