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
-- Module      : Amazonka.Kendra.Types.SeedUrlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SeedUrlConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.WebCrawlerMode
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for the seed or starting point
-- URLs to crawl.
--
-- /When selecting websites to index, you must adhere to the
-- <https://aws.amazon.com/aup/ Amazon Acceptable Use Policy> and all other
-- Amazon terms. Remember that you must only use Amazon Kendra Web Crawler
-- to index your own webpages, or webpages that you have authorization to
-- index./
--
-- /See:/ 'newSeedUrlConfiguration' smart constructor.
data SeedUrlConfiguration = SeedUrlConfiguration'
  { -- | You can choose one of the following modes:
    --
    -- -   @HOST_ONLY@ – crawl only the website host names. For example, if the
    --     seed URL is \"abc.example.com\", then only URLs with host name
    --     \"abc.example.com\" are crawled.
    --
    -- -   @SUBDOMAINS@ – crawl the website host names with subdomains. For
    --     example, if the seed URL is \"abc.example.com\", then
    --     \"a.abc.example.com\" and \"b.abc.example.com\" are also crawled.
    --
    -- -   @EVERYTHING@ – crawl the website host names with subdomains and
    --     other domains that the webpages link to.
    --
    -- The default mode is set to @HOST_ONLY@.
    webCrawlerMode :: Prelude.Maybe WebCrawlerMode,
    -- | The list of seed or starting point URLs of the websites you want to
    -- crawl.
    --
    -- The list can include a maximum of 100 seed URLs.
    seedUrls :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SeedUrlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webCrawlerMode', 'seedUrlConfiguration_webCrawlerMode' - You can choose one of the following modes:
--
-- -   @HOST_ONLY@ – crawl only the website host names. For example, if the
--     seed URL is \"abc.example.com\", then only URLs with host name
--     \"abc.example.com\" are crawled.
--
-- -   @SUBDOMAINS@ – crawl the website host names with subdomains. For
--     example, if the seed URL is \"abc.example.com\", then
--     \"a.abc.example.com\" and \"b.abc.example.com\" are also crawled.
--
-- -   @EVERYTHING@ – crawl the website host names with subdomains and
--     other domains that the webpages link to.
--
-- The default mode is set to @HOST_ONLY@.
--
-- 'seedUrls', 'seedUrlConfiguration_seedUrls' - The list of seed or starting point URLs of the websites you want to
-- crawl.
--
-- The list can include a maximum of 100 seed URLs.
newSeedUrlConfiguration ::
  SeedUrlConfiguration
newSeedUrlConfiguration =
  SeedUrlConfiguration'
    { webCrawlerMode =
        Prelude.Nothing,
      seedUrls = Prelude.mempty
    }

-- | You can choose one of the following modes:
--
-- -   @HOST_ONLY@ – crawl only the website host names. For example, if the
--     seed URL is \"abc.example.com\", then only URLs with host name
--     \"abc.example.com\" are crawled.
--
-- -   @SUBDOMAINS@ – crawl the website host names with subdomains. For
--     example, if the seed URL is \"abc.example.com\", then
--     \"a.abc.example.com\" and \"b.abc.example.com\" are also crawled.
--
-- -   @EVERYTHING@ – crawl the website host names with subdomains and
--     other domains that the webpages link to.
--
-- The default mode is set to @HOST_ONLY@.
seedUrlConfiguration_webCrawlerMode :: Lens.Lens' SeedUrlConfiguration (Prelude.Maybe WebCrawlerMode)
seedUrlConfiguration_webCrawlerMode = Lens.lens (\SeedUrlConfiguration' {webCrawlerMode} -> webCrawlerMode) (\s@SeedUrlConfiguration' {} a -> s {webCrawlerMode = a} :: SeedUrlConfiguration)

-- | The list of seed or starting point URLs of the websites you want to
-- crawl.
--
-- The list can include a maximum of 100 seed URLs.
seedUrlConfiguration_seedUrls :: Lens.Lens' SeedUrlConfiguration [Prelude.Text]
seedUrlConfiguration_seedUrls = Lens.lens (\SeedUrlConfiguration' {seedUrls} -> seedUrls) (\s@SeedUrlConfiguration' {} a -> s {seedUrls = a} :: SeedUrlConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON SeedUrlConfiguration where
  parseJSON =
    Data.withObject
      "SeedUrlConfiguration"
      ( \x ->
          SeedUrlConfiguration'
            Prelude.<$> (x Data..:? "WebCrawlerMode")
            Prelude.<*> (x Data..:? "SeedUrls" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SeedUrlConfiguration where
  hashWithSalt _salt SeedUrlConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` webCrawlerMode
      `Prelude.hashWithSalt` seedUrls

instance Prelude.NFData SeedUrlConfiguration where
  rnf SeedUrlConfiguration' {..} =
    Prelude.rnf webCrawlerMode `Prelude.seq`
      Prelude.rnf seedUrls

instance Data.ToJSON SeedUrlConfiguration where
  toJSON SeedUrlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WebCrawlerMode" Data..=)
              Prelude.<$> webCrawlerMode,
            Prelude.Just ("SeedUrls" Data..= seedUrls)
          ]
      )
