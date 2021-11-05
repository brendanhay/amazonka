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
-- Module      : Network.AWS.NetworkManager.UpdateSite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an existing site. To remove information for
-- any of the parameters, specify an empty string.
module Network.AWS.NetworkManager.UpdateSite
  ( -- * Creating a Request
    UpdateSite (..),
    newUpdateSite,

    -- * Request Lenses
    updateSite_location,
    updateSite_description,
    updateSite_globalNetworkId,
    updateSite_siteId,

    -- * Destructuring the Response
    UpdateSiteResponse (..),
    newUpdateSiteResponse,

    -- * Response Lenses
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSite' smart constructor.
data UpdateSite = UpdateSite'
  { -- | The site location:
    --
    -- -   @Address@: The physical address of the site.
    --
    -- -   @Latitude@: The latitude of the site.
    --
    -- -   @Longitude@: The longitude of the site.
    location :: Prelude.Maybe (Core.Sensitive Location),
    -- | A description of your site.
    --
    -- Length Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of your site.
    siteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'updateSite_location' - The site location:
--
-- -   @Address@: The physical address of the site.
--
-- -   @Latitude@: The latitude of the site.
--
-- -   @Longitude@: The longitude of the site.
--
-- 'description', 'updateSite_description' - A description of your site.
--
-- Length Constraints: Maximum length of 256 characters.
--
-- 'globalNetworkId', 'updateSite_globalNetworkId' - The ID of the global network.
--
-- 'siteId', 'updateSite_siteId' - The ID of your site.
newUpdateSite ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'siteId'
  Prelude.Text ->
  UpdateSite
newUpdateSite pGlobalNetworkId_ pSiteId_ =
  UpdateSite'
    { location = Prelude.Nothing,
      description = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_,
      siteId = pSiteId_
    }

-- | The site location:
--
-- -   @Address@: The physical address of the site.
--
-- -   @Latitude@: The latitude of the site.
--
-- -   @Longitude@: The longitude of the site.
updateSite_location :: Lens.Lens' UpdateSite (Prelude.Maybe Location)
updateSite_location = Lens.lens (\UpdateSite' {location} -> location) (\s@UpdateSite' {} a -> s {location = a} :: UpdateSite) Prelude.. Lens.mapping Core._Sensitive

-- | A description of your site.
--
-- Length Constraints: Maximum length of 256 characters.
updateSite_description :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_description = Lens.lens (\UpdateSite' {description} -> description) (\s@UpdateSite' {} a -> s {description = a} :: UpdateSite)

-- | The ID of the global network.
updateSite_globalNetworkId :: Lens.Lens' UpdateSite Prelude.Text
updateSite_globalNetworkId = Lens.lens (\UpdateSite' {globalNetworkId} -> globalNetworkId) (\s@UpdateSite' {} a -> s {globalNetworkId = a} :: UpdateSite)

-- | The ID of your site.
updateSite_siteId :: Lens.Lens' UpdateSite Prelude.Text
updateSite_siteId = Lens.lens (\UpdateSite' {siteId} -> siteId) (\s@UpdateSite' {} a -> s {siteId = a} :: UpdateSite)

instance Core.AWSRequest UpdateSite where
  type AWSResponse UpdateSite = UpdateSiteResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSiteResponse'
            Prelude.<$> (x Core..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSite

instance Prelude.NFData UpdateSite

instance Core.ToHeaders UpdateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSite where
  toJSON UpdateSite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Location" Core..=) Prelude.<$> location,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateSite where
  toPath UpdateSite' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/sites/",
        Core.toBS siteId
      ]

instance Core.ToQuery UpdateSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSiteResponse' smart constructor.
data UpdateSiteResponse = UpdateSiteResponse'
  { -- | Information about the site.
    site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'site', 'updateSiteResponse_site' - Information about the site.
--
-- 'httpStatus', 'updateSiteResponse_httpStatus' - The response's http status code.
newUpdateSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSiteResponse
newUpdateSiteResponse pHttpStatus_ =
  UpdateSiteResponse'
    { site = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the site.
updateSiteResponse_site :: Lens.Lens' UpdateSiteResponse (Prelude.Maybe Site)
updateSiteResponse_site = Lens.lens (\UpdateSiteResponse' {site} -> site) (\s@UpdateSiteResponse' {} a -> s {site = a} :: UpdateSiteResponse)

-- | The response's http status code.
updateSiteResponse_httpStatus :: Lens.Lens' UpdateSiteResponse Prelude.Int
updateSiteResponse_httpStatus = Lens.lens (\UpdateSiteResponse' {httpStatus} -> httpStatus) (\s@UpdateSiteResponse' {} a -> s {httpStatus = a} :: UpdateSiteResponse)

instance Prelude.NFData UpdateSiteResponse
