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
-- Module      : Amazonka.Kendra.Types.WebCrawlerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.WebCrawlerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AuthenticationConfiguration
import Amazonka.Kendra.Types.ProxyConfiguration
import Amazonka.Kendra.Types.Urls
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information required for Amazon Kendra Web
-- Crawler.
--
-- /See:/ 'newWebCrawlerConfiguration' smart constructor.
data WebCrawlerConfiguration = WebCrawlerConfiguration'
  { -- | Configuration information required to connect to websites using
    -- authentication.
    --
    -- You can connect to websites using basic authentication of user name and
    -- password. You use a secret in
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
    -- to store your authentication credentials.
    --
    -- You must provide the website host name and port number. For example, the
    -- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
    -- and the port is 443, the standard port for HTTPS.
    authenticationConfiguration :: Prelude.Maybe AuthenticationConfiguration,
    -- | The \'depth\' or number of levels from the seed level to crawl. For
    -- example, the seed URL page is depth 1 and any hyperlinks on this page
    -- that are also crawled are depth 2.
    crawlDepth :: Prelude.Maybe Prelude.Natural,
    -- | The maximum size (in MB) of a web page or attachment to crawl.
    --
    -- Files larger than this size (in MB) are skipped\/not crawled.
    --
    -- The default maximum size of a web page or attachment is set to 50 MB.
    maxContentSizePerPageInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The maximum number of URLs on a web page to include when crawling a
    -- website. This number is per web page.
    --
    -- As a website’s web pages are crawled, any URLs the web pages link to are
    -- also crawled. URLs on a web page are crawled in order of appearance.
    --
    -- The default maximum links per page is 100.
    maxLinksPerPage :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of URLs crawled per website host per minute.
    --
    -- A minimum of one URL is required.
    --
    -- The default maximum number of URLs crawled per website host per minute
    -- is 300.
    maxUrlsPerMinuteCrawlRate :: Prelude.Maybe Prelude.Natural,
    -- | Configuration information required to connect to your internal websites
    -- via a web proxy.
    --
    -- You must provide the website host name and port number. For example, the
    -- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
    -- and the port is 443, the standard port for HTTPS.
    --
    -- Web proxy credentials are optional and you can use them to connect to a
    -- web proxy server that requires basic authentication. To store web proxy
    -- credentials, you use a secret in
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>.
    proxyConfiguration :: Prelude.Maybe ProxyConfiguration,
    -- | A list of regular expression patterns to exclude certain URLs to crawl.
    -- URLs that match the patterns are excluded from the index. URLs that
    -- don\'t match the patterns are included in the index. If a URL matches
    -- both an inclusion and exclusion pattern, the exclusion pattern takes
    -- precedence and the URL file isn\'t included in the index.
    urlExclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to include certain URLs to crawl.
    -- URLs that match the patterns are included in the index. URLs that don\'t
    -- match the patterns are excluded from the index. If a URL matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the URL file isn\'t included in the index.
    urlInclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the seed or starting point URLs of the websites or the sitemap
    -- URLs of the websites you want to crawl.
    --
    -- You can include website subdomains. You can list up to 100 seed URLs and
    -- up to three sitemap URLs.
    --
    -- You can only crawl websites that use the secure communication protocol,
    -- Hypertext Transfer Protocol Secure (HTTPS). If you receive an error when
    -- crawling a website, it could be that the website is blocked from
    -- crawling.
    --
    -- /When selecting websites to index, you must adhere to the
    -- <https://aws.amazon.com/aup/ Amazon Acceptable Use Policy> and all other
    -- Amazon terms. Remember that you must only use Amazon Kendra Web Crawler
    -- to index your own web pages, or web pages that you have authorization to
    -- index./
    urls :: Urls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebCrawlerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationConfiguration', 'webCrawlerConfiguration_authenticationConfiguration' - Configuration information required to connect to websites using
-- authentication.
--
-- You can connect to websites using basic authentication of user name and
-- password. You use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
-- to store your authentication credentials.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- 'crawlDepth', 'webCrawlerConfiguration_crawlDepth' - The \'depth\' or number of levels from the seed level to crawl. For
-- example, the seed URL page is depth 1 and any hyperlinks on this page
-- that are also crawled are depth 2.
--
-- 'maxContentSizePerPageInMegaBytes', 'webCrawlerConfiguration_maxContentSizePerPageInMegaBytes' - The maximum size (in MB) of a web page or attachment to crawl.
--
-- Files larger than this size (in MB) are skipped\/not crawled.
--
-- The default maximum size of a web page or attachment is set to 50 MB.
--
-- 'maxLinksPerPage', 'webCrawlerConfiguration_maxLinksPerPage' - The maximum number of URLs on a web page to include when crawling a
-- website. This number is per web page.
--
-- As a website’s web pages are crawled, any URLs the web pages link to are
-- also crawled. URLs on a web page are crawled in order of appearance.
--
-- The default maximum links per page is 100.
--
-- 'maxUrlsPerMinuteCrawlRate', 'webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate' - The maximum number of URLs crawled per website host per minute.
--
-- A minimum of one URL is required.
--
-- The default maximum number of URLs crawled per website host per minute
-- is 300.
--
-- 'proxyConfiguration', 'webCrawlerConfiguration_proxyConfiguration' - Configuration information required to connect to your internal websites
-- via a web proxy.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- Web proxy credentials are optional and you can use them to connect to a
-- web proxy server that requires basic authentication. To store web proxy
-- credentials, you use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>.
--
-- 'urlExclusionPatterns', 'webCrawlerConfiguration_urlExclusionPatterns' - A list of regular expression patterns to exclude certain URLs to crawl.
-- URLs that match the patterns are excluded from the index. URLs that
-- don\'t match the patterns are included in the index. If a URL matches
-- both an inclusion and exclusion pattern, the exclusion pattern takes
-- precedence and the URL file isn\'t included in the index.
--
-- 'urlInclusionPatterns', 'webCrawlerConfiguration_urlInclusionPatterns' - A list of regular expression patterns to include certain URLs to crawl.
-- URLs that match the patterns are included in the index. URLs that don\'t
-- match the patterns are excluded from the index. If a URL matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the URL file isn\'t included in the index.
--
-- 'urls', 'webCrawlerConfiguration_urls' - Specifies the seed or starting point URLs of the websites or the sitemap
-- URLs of the websites you want to crawl.
--
-- You can include website subdomains. You can list up to 100 seed URLs and
-- up to three sitemap URLs.
--
-- You can only crawl websites that use the secure communication protocol,
-- Hypertext Transfer Protocol Secure (HTTPS). If you receive an error when
-- crawling a website, it could be that the website is blocked from
-- crawling.
--
-- /When selecting websites to index, you must adhere to the
-- <https://aws.amazon.com/aup/ Amazon Acceptable Use Policy> and all other
-- Amazon terms. Remember that you must only use Amazon Kendra Web Crawler
-- to index your own web pages, or web pages that you have authorization to
-- index./
newWebCrawlerConfiguration ::
  -- | 'urls'
  Urls ->
  WebCrawlerConfiguration
newWebCrawlerConfiguration pUrls_ =
  WebCrawlerConfiguration'
    { authenticationConfiguration =
        Prelude.Nothing,
      crawlDepth = Prelude.Nothing,
      maxContentSizePerPageInMegaBytes = Prelude.Nothing,
      maxLinksPerPage = Prelude.Nothing,
      maxUrlsPerMinuteCrawlRate = Prelude.Nothing,
      proxyConfiguration = Prelude.Nothing,
      urlExclusionPatterns = Prelude.Nothing,
      urlInclusionPatterns = Prelude.Nothing,
      urls = pUrls_
    }

-- | Configuration information required to connect to websites using
-- authentication.
--
-- You can connect to websites using basic authentication of user name and
-- password. You use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>
-- to store your authentication credentials.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
webCrawlerConfiguration_authenticationConfiguration :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe AuthenticationConfiguration)
webCrawlerConfiguration_authenticationConfiguration = Lens.lens (\WebCrawlerConfiguration' {authenticationConfiguration} -> authenticationConfiguration) (\s@WebCrawlerConfiguration' {} a -> s {authenticationConfiguration = a} :: WebCrawlerConfiguration)

-- | The \'depth\' or number of levels from the seed level to crawl. For
-- example, the seed URL page is depth 1 and any hyperlinks on this page
-- that are also crawled are depth 2.
webCrawlerConfiguration_crawlDepth :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Natural)
webCrawlerConfiguration_crawlDepth = Lens.lens (\WebCrawlerConfiguration' {crawlDepth} -> crawlDepth) (\s@WebCrawlerConfiguration' {} a -> s {crawlDepth = a} :: WebCrawlerConfiguration)

-- | The maximum size (in MB) of a web page or attachment to crawl.
--
-- Files larger than this size (in MB) are skipped\/not crawled.
--
-- The default maximum size of a web page or attachment is set to 50 MB.
webCrawlerConfiguration_maxContentSizePerPageInMegaBytes :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Double)
webCrawlerConfiguration_maxContentSizePerPageInMegaBytes = Lens.lens (\WebCrawlerConfiguration' {maxContentSizePerPageInMegaBytes} -> maxContentSizePerPageInMegaBytes) (\s@WebCrawlerConfiguration' {} a -> s {maxContentSizePerPageInMegaBytes = a} :: WebCrawlerConfiguration)

-- | The maximum number of URLs on a web page to include when crawling a
-- website. This number is per web page.
--
-- As a website’s web pages are crawled, any URLs the web pages link to are
-- also crawled. URLs on a web page are crawled in order of appearance.
--
-- The default maximum links per page is 100.
webCrawlerConfiguration_maxLinksPerPage :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Natural)
webCrawlerConfiguration_maxLinksPerPage = Lens.lens (\WebCrawlerConfiguration' {maxLinksPerPage} -> maxLinksPerPage) (\s@WebCrawlerConfiguration' {} a -> s {maxLinksPerPage = a} :: WebCrawlerConfiguration)

-- | The maximum number of URLs crawled per website host per minute.
--
-- A minimum of one URL is required.
--
-- The default maximum number of URLs crawled per website host per minute
-- is 300.
webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Natural)
webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate = Lens.lens (\WebCrawlerConfiguration' {maxUrlsPerMinuteCrawlRate} -> maxUrlsPerMinuteCrawlRate) (\s@WebCrawlerConfiguration' {} a -> s {maxUrlsPerMinuteCrawlRate = a} :: WebCrawlerConfiguration)

-- | Configuration information required to connect to your internal websites
-- via a web proxy.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- Web proxy credentials are optional and you can use them to connect to a
-- web proxy server that requires basic authentication. To store web proxy
-- credentials, you use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html Secrets Manager>.
webCrawlerConfiguration_proxyConfiguration :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe ProxyConfiguration)
webCrawlerConfiguration_proxyConfiguration = Lens.lens (\WebCrawlerConfiguration' {proxyConfiguration} -> proxyConfiguration) (\s@WebCrawlerConfiguration' {} a -> s {proxyConfiguration = a} :: WebCrawlerConfiguration)

-- | A list of regular expression patterns to exclude certain URLs to crawl.
-- URLs that match the patterns are excluded from the index. URLs that
-- don\'t match the patterns are included in the index. If a URL matches
-- both an inclusion and exclusion pattern, the exclusion pattern takes
-- precedence and the URL file isn\'t included in the index.
webCrawlerConfiguration_urlExclusionPatterns :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe [Prelude.Text])
webCrawlerConfiguration_urlExclusionPatterns = Lens.lens (\WebCrawlerConfiguration' {urlExclusionPatterns} -> urlExclusionPatterns) (\s@WebCrawlerConfiguration' {} a -> s {urlExclusionPatterns = a} :: WebCrawlerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain URLs to crawl.
-- URLs that match the patterns are included in the index. URLs that don\'t
-- match the patterns are excluded from the index. If a URL matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the URL file isn\'t included in the index.
webCrawlerConfiguration_urlInclusionPatterns :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe [Prelude.Text])
webCrawlerConfiguration_urlInclusionPatterns = Lens.lens (\WebCrawlerConfiguration' {urlInclusionPatterns} -> urlInclusionPatterns) (\s@WebCrawlerConfiguration' {} a -> s {urlInclusionPatterns = a} :: WebCrawlerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the seed or starting point URLs of the websites or the sitemap
-- URLs of the websites you want to crawl.
--
-- You can include website subdomains. You can list up to 100 seed URLs and
-- up to three sitemap URLs.
--
-- You can only crawl websites that use the secure communication protocol,
-- Hypertext Transfer Protocol Secure (HTTPS). If you receive an error when
-- crawling a website, it could be that the website is blocked from
-- crawling.
--
-- /When selecting websites to index, you must adhere to the
-- <https://aws.amazon.com/aup/ Amazon Acceptable Use Policy> and all other
-- Amazon terms. Remember that you must only use Amazon Kendra Web Crawler
-- to index your own web pages, or web pages that you have authorization to
-- index./
webCrawlerConfiguration_urls :: Lens.Lens' WebCrawlerConfiguration Urls
webCrawlerConfiguration_urls = Lens.lens (\WebCrawlerConfiguration' {urls} -> urls) (\s@WebCrawlerConfiguration' {} a -> s {urls = a} :: WebCrawlerConfiguration)

instance Data.FromJSON WebCrawlerConfiguration where
  parseJSON =
    Data.withObject
      "WebCrawlerConfiguration"
      ( \x ->
          WebCrawlerConfiguration'
            Prelude.<$> (x Data..:? "AuthenticationConfiguration")
            Prelude.<*> (x Data..:? "CrawlDepth")
            Prelude.<*> (x Data..:? "MaxContentSizePerPageInMegaBytes")
            Prelude.<*> (x Data..:? "MaxLinksPerPage")
            Prelude.<*> (x Data..:? "MaxUrlsPerMinuteCrawlRate")
            Prelude.<*> (x Data..:? "ProxyConfiguration")
            Prelude.<*> ( x
                            Data..:? "UrlExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "UrlInclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "Urls")
      )

instance Prelude.Hashable WebCrawlerConfiguration where
  hashWithSalt _salt WebCrawlerConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationConfiguration
      `Prelude.hashWithSalt` crawlDepth
      `Prelude.hashWithSalt` maxContentSizePerPageInMegaBytes
      `Prelude.hashWithSalt` maxLinksPerPage
      `Prelude.hashWithSalt` maxUrlsPerMinuteCrawlRate
      `Prelude.hashWithSalt` proxyConfiguration
      `Prelude.hashWithSalt` urlExclusionPatterns
      `Prelude.hashWithSalt` urlInclusionPatterns
      `Prelude.hashWithSalt` urls

instance Prelude.NFData WebCrawlerConfiguration where
  rnf WebCrawlerConfiguration' {..} =
    Prelude.rnf authenticationConfiguration
      `Prelude.seq` Prelude.rnf crawlDepth
      `Prelude.seq` Prelude.rnf maxContentSizePerPageInMegaBytes
      `Prelude.seq` Prelude.rnf maxLinksPerPage
      `Prelude.seq` Prelude.rnf maxUrlsPerMinuteCrawlRate
      `Prelude.seq` Prelude.rnf proxyConfiguration
      `Prelude.seq` Prelude.rnf urlExclusionPatterns
      `Prelude.seq` Prelude.rnf urlInclusionPatterns
      `Prelude.seq` Prelude.rnf urls

instance Data.ToJSON WebCrawlerConfiguration where
  toJSON WebCrawlerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationConfiguration" Data..=)
              Prelude.<$> authenticationConfiguration,
            ("CrawlDepth" Data..=) Prelude.<$> crawlDepth,
            ("MaxContentSizePerPageInMegaBytes" Data..=)
              Prelude.<$> maxContentSizePerPageInMegaBytes,
            ("MaxLinksPerPage" Data..=)
              Prelude.<$> maxLinksPerPage,
            ("MaxUrlsPerMinuteCrawlRate" Data..=)
              Prelude.<$> maxUrlsPerMinuteCrawlRate,
            ("ProxyConfiguration" Data..=)
              Prelude.<$> proxyConfiguration,
            ("UrlExclusionPatterns" Data..=)
              Prelude.<$> urlExclusionPatterns,
            ("UrlInclusionPatterns" Data..=)
              Prelude.<$> urlInclusionPatterns,
            Prelude.Just ("Urls" Data..= urls)
          ]
      )
