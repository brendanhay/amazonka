{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kendra.Types.Urls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Urls where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.SeedUrlConfiguration
import Network.AWS.Kendra.Types.SiteMapsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the configuration information of the URLs to crawl.
--
-- You can only crawl websites that use the secure communication protocol,
-- Hypertext Transfer Protocol Secure (HTTPS). If you receive an error when
-- crawling a website, it could be that the website is blocked from
-- crawling.
--
-- /When selecting websites to index, you must adhere to the
-- <https://aws.amazon.com/aup/ Amazon Acceptable Use Policy> and all other
-- Amazon terms. Remember that you must only use the Amazon Kendra web
-- crawler to index your own webpages, or webpages that you have
-- authorization to index./
--
-- /See:/ 'newUrls' smart constructor.
data Urls = Urls'
  { -- | Provides the configuration of the seed or starting point URLs of the
    -- websites you want to crawl.
    --
    -- You can choose to crawl only the website host names, or the website host
    -- names with subdomains, or the website host names with subdomains and
    -- other domains that the webpages link to.
    --
    -- You can list up to 100 seed URLs.
    seedUrlConfiguration :: Prelude.Maybe SeedUrlConfiguration,
    -- | Provides the configuration of the sitemap URLs of the websites you want
    -- to crawl.
    --
    -- Only URLs belonging to the same website host names are crawled. You can
    -- list up to three sitemap URLs.
    siteMapsConfiguration :: Prelude.Maybe SiteMapsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Urls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'seedUrlConfiguration', 'urls_seedUrlConfiguration' - Provides the configuration of the seed or starting point URLs of the
-- websites you want to crawl.
--
-- You can choose to crawl only the website host names, or the website host
-- names with subdomains, or the website host names with subdomains and
-- other domains that the webpages link to.
--
-- You can list up to 100 seed URLs.
--
-- 'siteMapsConfiguration', 'urls_siteMapsConfiguration' - Provides the configuration of the sitemap URLs of the websites you want
-- to crawl.
--
-- Only URLs belonging to the same website host names are crawled. You can
-- list up to three sitemap URLs.
newUrls ::
  Urls
newUrls =
  Urls'
    { seedUrlConfiguration = Prelude.Nothing,
      siteMapsConfiguration = Prelude.Nothing
    }

-- | Provides the configuration of the seed or starting point URLs of the
-- websites you want to crawl.
--
-- You can choose to crawl only the website host names, or the website host
-- names with subdomains, or the website host names with subdomains and
-- other domains that the webpages link to.
--
-- You can list up to 100 seed URLs.
urls_seedUrlConfiguration :: Lens.Lens' Urls (Prelude.Maybe SeedUrlConfiguration)
urls_seedUrlConfiguration = Lens.lens (\Urls' {seedUrlConfiguration} -> seedUrlConfiguration) (\s@Urls' {} a -> s {seedUrlConfiguration = a} :: Urls)

-- | Provides the configuration of the sitemap URLs of the websites you want
-- to crawl.
--
-- Only URLs belonging to the same website host names are crawled. You can
-- list up to three sitemap URLs.
urls_siteMapsConfiguration :: Lens.Lens' Urls (Prelude.Maybe SiteMapsConfiguration)
urls_siteMapsConfiguration = Lens.lens (\Urls' {siteMapsConfiguration} -> siteMapsConfiguration) (\s@Urls' {} a -> s {siteMapsConfiguration = a} :: Urls)

instance Core.FromJSON Urls where
  parseJSON =
    Core.withObject
      "Urls"
      ( \x ->
          Urls'
            Prelude.<$> (x Core..:? "SeedUrlConfiguration")
            Prelude.<*> (x Core..:? "SiteMapsConfiguration")
      )

instance Prelude.Hashable Urls

instance Prelude.NFData Urls

instance Core.ToJSON Urls where
  toJSON Urls' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SeedUrlConfiguration" Core..=)
              Prelude.<$> seedUrlConfiguration,
            ("SiteMapsConfiguration" Core..=)
              Prelude.<$> siteMapsConfiguration
          ]
      )
