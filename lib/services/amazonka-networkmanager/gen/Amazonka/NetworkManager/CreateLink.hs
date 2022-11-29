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
-- Module      : Amazonka.NetworkManager.CreateLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new link for a specified site.
module Amazonka.NetworkManager.CreateLink
  ( -- * Creating a Request
    CreateLink (..),
    newCreateLink,

    -- * Request Lenses
    createLink_tags,
    createLink_type,
    createLink_provider,
    createLink_description,
    createLink_globalNetworkId,
    createLink_bandwidth,
    createLink_siteId,

    -- * Destructuring the Response
    CreateLinkResponse (..),
    newCreateLinkResponse,

    -- * Response Lenses
    createLinkResponse_link,
    createLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLink' smart constructor.
data CreateLink = CreateLink'
  { -- | The tags to apply to the resource during creation.
    tags :: Prelude.Maybe [Tag],
    -- | The type of the link.
    --
    -- Constraints: Maximum length of 128 characters. Cannot include the
    -- following characters: | \\ ^
    type' :: Prelude.Maybe Prelude.Text,
    -- | The provider of the link.
    --
    -- Constraints: Maximum length of 128 characters. Cannot include the
    -- following characters: | \\ ^
    provider :: Prelude.Maybe Prelude.Text,
    -- | A description of the link.
    --
    -- Constraints: Maximum length of 256 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The upload speed and download speed in Mbps.
    bandwidth :: Bandwidth,
    -- | The ID of the site.
    siteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLink_tags' - The tags to apply to the resource during creation.
--
-- 'type'', 'createLink_type' - The type of the link.
--
-- Constraints: Maximum length of 128 characters. Cannot include the
-- following characters: | \\ ^
--
-- 'provider', 'createLink_provider' - The provider of the link.
--
-- Constraints: Maximum length of 128 characters. Cannot include the
-- following characters: | \\ ^
--
-- 'description', 'createLink_description' - A description of the link.
--
-- Constraints: Maximum length of 256 characters.
--
-- 'globalNetworkId', 'createLink_globalNetworkId' - The ID of the global network.
--
-- 'bandwidth', 'createLink_bandwidth' - The upload speed and download speed in Mbps.
--
-- 'siteId', 'createLink_siteId' - The ID of the site.
newCreateLink ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'bandwidth'
  Bandwidth ->
  -- | 'siteId'
  Prelude.Text ->
  CreateLink
newCreateLink pGlobalNetworkId_ pBandwidth_ pSiteId_ =
  CreateLink'
    { tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      provider = Prelude.Nothing,
      description = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_,
      bandwidth = pBandwidth_,
      siteId = pSiteId_
    }

-- | The tags to apply to the resource during creation.
createLink_tags :: Lens.Lens' CreateLink (Prelude.Maybe [Tag])
createLink_tags = Lens.lens (\CreateLink' {tags} -> tags) (\s@CreateLink' {} a -> s {tags = a} :: CreateLink) Prelude.. Lens.mapping Lens.coerced

-- | The type of the link.
--
-- Constraints: Maximum length of 128 characters. Cannot include the
-- following characters: | \\ ^
createLink_type :: Lens.Lens' CreateLink (Prelude.Maybe Prelude.Text)
createLink_type = Lens.lens (\CreateLink' {type'} -> type') (\s@CreateLink' {} a -> s {type' = a} :: CreateLink)

-- | The provider of the link.
--
-- Constraints: Maximum length of 128 characters. Cannot include the
-- following characters: | \\ ^
createLink_provider :: Lens.Lens' CreateLink (Prelude.Maybe Prelude.Text)
createLink_provider = Lens.lens (\CreateLink' {provider} -> provider) (\s@CreateLink' {} a -> s {provider = a} :: CreateLink)

-- | A description of the link.
--
-- Constraints: Maximum length of 256 characters.
createLink_description :: Lens.Lens' CreateLink (Prelude.Maybe Prelude.Text)
createLink_description = Lens.lens (\CreateLink' {description} -> description) (\s@CreateLink' {} a -> s {description = a} :: CreateLink)

-- | The ID of the global network.
createLink_globalNetworkId :: Lens.Lens' CreateLink Prelude.Text
createLink_globalNetworkId = Lens.lens (\CreateLink' {globalNetworkId} -> globalNetworkId) (\s@CreateLink' {} a -> s {globalNetworkId = a} :: CreateLink)

-- | The upload speed and download speed in Mbps.
createLink_bandwidth :: Lens.Lens' CreateLink Bandwidth
createLink_bandwidth = Lens.lens (\CreateLink' {bandwidth} -> bandwidth) (\s@CreateLink' {} a -> s {bandwidth = a} :: CreateLink)

-- | The ID of the site.
createLink_siteId :: Lens.Lens' CreateLink Prelude.Text
createLink_siteId = Lens.lens (\CreateLink' {siteId} -> siteId) (\s@CreateLink' {} a -> s {siteId = a} :: CreateLink)

instance Core.AWSRequest CreateLink where
  type AWSResponse CreateLink = CreateLinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLinkResponse'
            Prelude.<$> (x Core..?> "Link")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLink where
  hashWithSalt _salt CreateLink' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` siteId

instance Prelude.NFData CreateLink where
  rnf CreateLink' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf bandwidth
      `Prelude.seq` Prelude.rnf siteId

instance Core.ToHeaders CreateLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLink where
  toJSON CreateLink' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Type" Core..=) Prelude.<$> type',
            ("Provider" Core..=) Prelude.<$> provider,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Bandwidth" Core..= bandwidth),
            Prelude.Just ("SiteId" Core..= siteId)
          ]
      )

instance Core.ToPath CreateLink where
  toPath CreateLink' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/links"
      ]

instance Core.ToQuery CreateLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLinkResponse' smart constructor.
data CreateLinkResponse = CreateLinkResponse'
  { -- | Information about the link.
    link :: Prelude.Maybe Link,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'link', 'createLinkResponse_link' - Information about the link.
--
-- 'httpStatus', 'createLinkResponse_httpStatus' - The response's http status code.
newCreateLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLinkResponse
newCreateLinkResponse pHttpStatus_ =
  CreateLinkResponse'
    { link = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the link.
createLinkResponse_link :: Lens.Lens' CreateLinkResponse (Prelude.Maybe Link)
createLinkResponse_link = Lens.lens (\CreateLinkResponse' {link} -> link) (\s@CreateLinkResponse' {} a -> s {link = a} :: CreateLinkResponse)

-- | The response's http status code.
createLinkResponse_httpStatus :: Lens.Lens' CreateLinkResponse Prelude.Int
createLinkResponse_httpStatus = Lens.lens (\CreateLinkResponse' {httpStatus} -> httpStatus) (\s@CreateLinkResponse' {} a -> s {httpStatus = a} :: CreateLinkResponse)

instance Prelude.NFData CreateLinkResponse where
  rnf CreateLinkResponse' {..} =
    Prelude.rnf link
      `Prelude.seq` Prelude.rnf httpStatus
