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
-- Module      : Amazonka.NetworkManager.UpdateSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the information for an existing site. To remove information for
-- any of the parameters, specify an empty string.
module Amazonka.NetworkManager.UpdateSite
  ( -- * Creating a Request
    UpdateSite (..),
    newUpdateSite,

    -- * Request Lenses
    updateSite_description,
    updateSite_location,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSite' smart constructor.
data UpdateSite = UpdateSite'
  { -- | A description of your site.
    --
    -- Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The site location:
    --
    -- -   @Address@: The physical address of the site.
    --
    -- -   @Latitude@: The latitude of the site.
    --
    -- -   @Longitude@: The longitude of the site.
    location :: Prelude.Maybe (Data.Sensitive Location),
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
-- 'description', 'updateSite_description' - A description of your site.
--
-- Constraints: Maximum length of 256 characters.
--
-- 'location', 'updateSite_location' - The site location:
--
-- -   @Address@: The physical address of the site.
--
-- -   @Latitude@: The latitude of the site.
--
-- -   @Longitude@: The longitude of the site.
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
    { description = Prelude.Nothing,
      location = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_,
      siteId = pSiteId_
    }

-- | A description of your site.
--
-- Constraints: Maximum length of 256 characters.
updateSite_description :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_description = Lens.lens (\UpdateSite' {description} -> description) (\s@UpdateSite' {} a -> s {description = a} :: UpdateSite)

-- | The site location:
--
-- -   @Address@: The physical address of the site.
--
-- -   @Latitude@: The latitude of the site.
--
-- -   @Longitude@: The longitude of the site.
updateSite_location :: Lens.Lens' UpdateSite (Prelude.Maybe Location)
updateSite_location = Lens.lens (\UpdateSite' {location} -> location) (\s@UpdateSite' {} a -> s {location = a} :: UpdateSite) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the global network.
updateSite_globalNetworkId :: Lens.Lens' UpdateSite Prelude.Text
updateSite_globalNetworkId = Lens.lens (\UpdateSite' {globalNetworkId} -> globalNetworkId) (\s@UpdateSite' {} a -> s {globalNetworkId = a} :: UpdateSite)

-- | The ID of your site.
updateSite_siteId :: Lens.Lens' UpdateSite Prelude.Text
updateSite_siteId = Lens.lens (\UpdateSite' {siteId} -> siteId) (\s@UpdateSite' {} a -> s {siteId = a} :: UpdateSite)

instance Core.AWSRequest UpdateSite where
  type AWSResponse UpdateSite = UpdateSiteResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSiteResponse'
            Prelude.<$> (x Data..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSite where
  hashWithSalt _salt UpdateSite' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` siteId

instance Prelude.NFData UpdateSite where
  rnf UpdateSite' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf siteId

instance Data.ToHeaders UpdateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSite where
  toJSON UpdateSite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Location" Data..=) Prelude.<$> location
          ]
      )

instance Data.ToPath UpdateSite where
  toPath UpdateSite' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/sites/",
        Data.toBS siteId
      ]

instance Data.ToQuery UpdateSite where
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

instance Prelude.NFData UpdateSiteResponse where
  rnf UpdateSiteResponse' {..} =
    Prelude.rnf site
      `Prelude.seq` Prelude.rnf httpStatus
