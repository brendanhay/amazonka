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
-- Module      : Amazonka.NetworkManager.CreateSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new site in a global network.
module Amazonka.NetworkManager.CreateSite
  ( -- * Creating a Request
    CreateSite (..),
    newCreateSite,

    -- * Request Lenses
    createSite_description,
    createSite_location,
    createSite_tags,
    createSite_globalNetworkId,

    -- * Destructuring the Response
    CreateSiteResponse (..),
    newCreateSiteResponse,

    -- * Response Lenses
    createSiteResponse_site,
    createSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSite' smart constructor.
data CreateSite = CreateSite'
  { -- | A description of your site.
    --
    -- Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The site location. This information is used for visualization in the
    -- Network Manager console. If you specify the address, the latitude and
    -- longitude are automatically calculated.
    --
    -- -   @Address@: The physical address of the site.
    --
    -- -   @Latitude@: The latitude of the site.
    --
    -- -   @Longitude@: The longitude of the site.
    location :: Prelude.Maybe (Data.Sensitive Location),
    -- | The tags to apply to the resource during creation.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSite_description' - A description of your site.
--
-- Constraints: Maximum length of 256 characters.
--
-- 'location', 'createSite_location' - The site location. This information is used for visualization in the
-- Network Manager console. If you specify the address, the latitude and
-- longitude are automatically calculated.
--
-- -   @Address@: The physical address of the site.
--
-- -   @Latitude@: The latitude of the site.
--
-- -   @Longitude@: The longitude of the site.
--
-- 'tags', 'createSite_tags' - The tags to apply to the resource during creation.
--
-- 'globalNetworkId', 'createSite_globalNetworkId' - The ID of the global network.
newCreateSite ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  CreateSite
newCreateSite pGlobalNetworkId_ =
  CreateSite'
    { description = Prelude.Nothing,
      location = Prelude.Nothing,
      tags = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | A description of your site.
--
-- Constraints: Maximum length of 256 characters.
createSite_description :: Lens.Lens' CreateSite (Prelude.Maybe Prelude.Text)
createSite_description = Lens.lens (\CreateSite' {description} -> description) (\s@CreateSite' {} a -> s {description = a} :: CreateSite)

-- | The site location. This information is used for visualization in the
-- Network Manager console. If you specify the address, the latitude and
-- longitude are automatically calculated.
--
-- -   @Address@: The physical address of the site.
--
-- -   @Latitude@: The latitude of the site.
--
-- -   @Longitude@: The longitude of the site.
createSite_location :: Lens.Lens' CreateSite (Prelude.Maybe Location)
createSite_location = Lens.lens (\CreateSite' {location} -> location) (\s@CreateSite' {} a -> s {location = a} :: CreateSite) Prelude.. Lens.mapping Data._Sensitive

-- | The tags to apply to the resource during creation.
createSite_tags :: Lens.Lens' CreateSite (Prelude.Maybe [Tag])
createSite_tags = Lens.lens (\CreateSite' {tags} -> tags) (\s@CreateSite' {} a -> s {tags = a} :: CreateSite) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the global network.
createSite_globalNetworkId :: Lens.Lens' CreateSite Prelude.Text
createSite_globalNetworkId = Lens.lens (\CreateSite' {globalNetworkId} -> globalNetworkId) (\s@CreateSite' {} a -> s {globalNetworkId = a} :: CreateSite)

instance Core.AWSRequest CreateSite where
  type AWSResponse CreateSite = CreateSiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSiteResponse'
            Prelude.<$> (x Data..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSite where
  hashWithSalt _salt CreateSite' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData CreateSite where
  rnf CreateSite' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders CreateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSite where
  toJSON CreateSite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Location" Data..=) Prelude.<$> location,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateSite where
  toPath CreateSite' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/sites"
      ]

instance Data.ToQuery CreateSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSiteResponse' smart constructor.
data CreateSiteResponse = CreateSiteResponse'
  { -- | Information about the site.
    site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'site', 'createSiteResponse_site' - Information about the site.
--
-- 'httpStatus', 'createSiteResponse_httpStatus' - The response's http status code.
newCreateSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSiteResponse
newCreateSiteResponse pHttpStatus_ =
  CreateSiteResponse'
    { site = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the site.
createSiteResponse_site :: Lens.Lens' CreateSiteResponse (Prelude.Maybe Site)
createSiteResponse_site = Lens.lens (\CreateSiteResponse' {site} -> site) (\s@CreateSiteResponse' {} a -> s {site = a} :: CreateSiteResponse)

-- | The response's http status code.
createSiteResponse_httpStatus :: Lens.Lens' CreateSiteResponse Prelude.Int
createSiteResponse_httpStatus = Lens.lens (\CreateSiteResponse' {httpStatus} -> httpStatus) (\s@CreateSiteResponse' {} a -> s {httpStatus = a} :: CreateSiteResponse)

instance Prelude.NFData CreateSiteResponse where
  rnf CreateSiteResponse' {..} =
    Prelude.rnf site
      `Prelude.seq` Prelude.rnf httpStatus
