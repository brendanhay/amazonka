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
-- Module      : Network.AWS.Kendra.Types.WebCrawlerConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.WebCrawlerConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.AuthenticationConfiguration
import Network.AWS.Kendra.Types.ProxyConfiguration
import Network.AWS.Kendra.Types.Urls
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the configuration information required for Amazon Kendra web
-- crawler.
--
-- /See:/ 'newWebCrawlerConfiguration' smart constructor.
data WebCrawlerConfiguration = WebCrawlerConfiguration'
  { -- | The regular expression pattern to exclude certain URLs to crawl.
    --
    -- If there is a regular expression pattern to include certain URLs that
    -- conflicts with the exclude pattern, the exclude pattern takes
    -- precedence.
    urlExclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of URLs crawled per website host per minute.
    --
    -- A minimum of one URL is required.
    --
    -- The default maximum number of URLs crawled per website host per minute
    -- is 300.
    maxUrlsPerMinuteCrawlRate :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the number of levels in a website that you want to crawl.
    --
    -- The first level begins from the website seed or starting point URL. For
    -- example, if a website has 3 levels – index level (i.e. seed in this
    -- example), sections level, and subsections level – and you are only
    -- interested in crawling information up to the sections level (i.e. levels
    -- 0-1), you can set your depth to 1.
    --
    -- The default crawl depth is set to 2.
    crawlDepth :: Prelude.Maybe Prelude.Natural,
    -- | The maximum size (in MB) of a webpage or attachment to crawl.
    --
    -- Files larger than this size (in MB) are skipped\/not crawled.
    --
    -- The default maximum size of a webpage or attachment is set to 50 MB.
    maxContentSizePerPageInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | Provides configuration information required to connect to your internal
    -- websites via a web proxy.
    --
    -- You must provide the website host name and port number. For example, the
    -- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
    -- and the port is 443, the standard port for HTTPS.
    --
    -- Web proxy credentials are optional and you can use them to connect to a
    -- web proxy server that requires basic authentication. To store web proxy
    -- credentials, you use a secret in
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html AWS Secrets Manager>.
    proxyConfiguration :: Prelude.Maybe ProxyConfiguration,
    -- | Provides configuration information required to connect to websites using
    -- authentication.
    --
    -- You can connect to websites using basic authentication of user name and
    -- password.
    --
    -- You must provide the website host name and port number. For example, the
    -- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
    -- and the port is 443, the standard port for HTTPS. You use a secret in
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html AWS Secrets Manager>
    -- to store your authentication credentials.
    authenticationConfiguration :: Prelude.Maybe AuthenticationConfiguration,
    -- | The maximum number of URLs on a webpage to include when crawling a
    -- website. This number is per webpage.
    --
    -- As a website’s webpages are crawled, any URLs the webpages link to are
    -- also crawled. URLs on a webpage are crawled in order of appearance.
    --
    -- The default maximum links per page is 100.
    maxLinksPerPage :: Prelude.Maybe Prelude.Natural,
    -- | The regular expression pattern to include certain URLs to crawl.
    --
    -- If there is a regular expression pattern to exclude certain URLs that
    -- conflicts with the include pattern, the exclude pattern takes
    -- precedence.
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
    -- Amazon terms. Remember that you must only use the Amazon Kendra web
    -- crawler to index your own webpages, or webpages that you have
    -- authorization to index./
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
-- 'urlExclusionPatterns', 'webCrawlerConfiguration_urlExclusionPatterns' - The regular expression pattern to exclude certain URLs to crawl.
--
-- If there is a regular expression pattern to include certain URLs that
-- conflicts with the exclude pattern, the exclude pattern takes
-- precedence.
--
-- 'maxUrlsPerMinuteCrawlRate', 'webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate' - The maximum number of URLs crawled per website host per minute.
--
-- A minimum of one URL is required.
--
-- The default maximum number of URLs crawled per website host per minute
-- is 300.
--
-- 'crawlDepth', 'webCrawlerConfiguration_crawlDepth' - Specifies the number of levels in a website that you want to crawl.
--
-- The first level begins from the website seed or starting point URL. For
-- example, if a website has 3 levels – index level (i.e. seed in this
-- example), sections level, and subsections level – and you are only
-- interested in crawling information up to the sections level (i.e. levels
-- 0-1), you can set your depth to 1.
--
-- The default crawl depth is set to 2.
--
-- 'maxContentSizePerPageInMegaBytes', 'webCrawlerConfiguration_maxContentSizePerPageInMegaBytes' - The maximum size (in MB) of a webpage or attachment to crawl.
--
-- Files larger than this size (in MB) are skipped\/not crawled.
--
-- The default maximum size of a webpage or attachment is set to 50 MB.
--
-- 'proxyConfiguration', 'webCrawlerConfiguration_proxyConfiguration' - Provides configuration information required to connect to your internal
-- websites via a web proxy.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- Web proxy credentials are optional and you can use them to connect to a
-- web proxy server that requires basic authentication. To store web proxy
-- credentials, you use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html AWS Secrets Manager>.
--
-- 'authenticationConfiguration', 'webCrawlerConfiguration_authenticationConfiguration' - Provides configuration information required to connect to websites using
-- authentication.
--
-- You can connect to websites using basic authentication of user name and
-- password.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS. You use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html AWS Secrets Manager>
-- to store your authentication credentials.
--
-- 'maxLinksPerPage', 'webCrawlerConfiguration_maxLinksPerPage' - The maximum number of URLs on a webpage to include when crawling a
-- website. This number is per webpage.
--
-- As a website’s webpages are crawled, any URLs the webpages link to are
-- also crawled. URLs on a webpage are crawled in order of appearance.
--
-- The default maximum links per page is 100.
--
-- 'urlInclusionPatterns', 'webCrawlerConfiguration_urlInclusionPatterns' - The regular expression pattern to include certain URLs to crawl.
--
-- If there is a regular expression pattern to exclude certain URLs that
-- conflicts with the include pattern, the exclude pattern takes
-- precedence.
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
-- Amazon terms. Remember that you must only use the Amazon Kendra web
-- crawler to index your own webpages, or webpages that you have
-- authorization to index./
newWebCrawlerConfiguration ::
  -- | 'urls'
  Urls ->
  WebCrawlerConfiguration
newWebCrawlerConfiguration pUrls_ =
  WebCrawlerConfiguration'
    { urlExclusionPatterns =
        Prelude.Nothing,
      maxUrlsPerMinuteCrawlRate = Prelude.Nothing,
      crawlDepth = Prelude.Nothing,
      maxContentSizePerPageInMegaBytes = Prelude.Nothing,
      proxyConfiguration = Prelude.Nothing,
      authenticationConfiguration = Prelude.Nothing,
      maxLinksPerPage = Prelude.Nothing,
      urlInclusionPatterns = Prelude.Nothing,
      urls = pUrls_
    }

-- | The regular expression pattern to exclude certain URLs to crawl.
--
-- If there is a regular expression pattern to include certain URLs that
-- conflicts with the exclude pattern, the exclude pattern takes
-- precedence.
webCrawlerConfiguration_urlExclusionPatterns :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe [Prelude.Text])
webCrawlerConfiguration_urlExclusionPatterns = Lens.lens (\WebCrawlerConfiguration' {urlExclusionPatterns} -> urlExclusionPatterns) (\s@WebCrawlerConfiguration' {} a -> s {urlExclusionPatterns = a} :: WebCrawlerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of URLs crawled per website host per minute.
--
-- A minimum of one URL is required.
--
-- The default maximum number of URLs crawled per website host per minute
-- is 300.
webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Natural)
webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate = Lens.lens (\WebCrawlerConfiguration' {maxUrlsPerMinuteCrawlRate} -> maxUrlsPerMinuteCrawlRate) (\s@WebCrawlerConfiguration' {} a -> s {maxUrlsPerMinuteCrawlRate = a} :: WebCrawlerConfiguration)

-- | Specifies the number of levels in a website that you want to crawl.
--
-- The first level begins from the website seed or starting point URL. For
-- example, if a website has 3 levels – index level (i.e. seed in this
-- example), sections level, and subsections level – and you are only
-- interested in crawling information up to the sections level (i.e. levels
-- 0-1), you can set your depth to 1.
--
-- The default crawl depth is set to 2.
webCrawlerConfiguration_crawlDepth :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Natural)
webCrawlerConfiguration_crawlDepth = Lens.lens (\WebCrawlerConfiguration' {crawlDepth} -> crawlDepth) (\s@WebCrawlerConfiguration' {} a -> s {crawlDepth = a} :: WebCrawlerConfiguration)

-- | The maximum size (in MB) of a webpage or attachment to crawl.
--
-- Files larger than this size (in MB) are skipped\/not crawled.
--
-- The default maximum size of a webpage or attachment is set to 50 MB.
webCrawlerConfiguration_maxContentSizePerPageInMegaBytes :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Double)
webCrawlerConfiguration_maxContentSizePerPageInMegaBytes = Lens.lens (\WebCrawlerConfiguration' {maxContentSizePerPageInMegaBytes} -> maxContentSizePerPageInMegaBytes) (\s@WebCrawlerConfiguration' {} a -> s {maxContentSizePerPageInMegaBytes = a} :: WebCrawlerConfiguration)

-- | Provides configuration information required to connect to your internal
-- websites via a web proxy.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS.
--
-- Web proxy credentials are optional and you can use them to connect to a
-- web proxy server that requires basic authentication. To store web proxy
-- credentials, you use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html AWS Secrets Manager>.
webCrawlerConfiguration_proxyConfiguration :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe ProxyConfiguration)
webCrawlerConfiguration_proxyConfiguration = Lens.lens (\WebCrawlerConfiguration' {proxyConfiguration} -> proxyConfiguration) (\s@WebCrawlerConfiguration' {} a -> s {proxyConfiguration = a} :: WebCrawlerConfiguration)

-- | Provides configuration information required to connect to websites using
-- authentication.
--
-- You can connect to websites using basic authentication of user name and
-- password.
--
-- You must provide the website host name and port number. For example, the
-- host name of https:\/\/a.example.com\/page1.html is \"a.example.com\"
-- and the port is 443, the standard port for HTTPS. You use a secret in
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/intro.html AWS Secrets Manager>
-- to store your authentication credentials.
webCrawlerConfiguration_authenticationConfiguration :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe AuthenticationConfiguration)
webCrawlerConfiguration_authenticationConfiguration = Lens.lens (\WebCrawlerConfiguration' {authenticationConfiguration} -> authenticationConfiguration) (\s@WebCrawlerConfiguration' {} a -> s {authenticationConfiguration = a} :: WebCrawlerConfiguration)

-- | The maximum number of URLs on a webpage to include when crawling a
-- website. This number is per webpage.
--
-- As a website’s webpages are crawled, any URLs the webpages link to are
-- also crawled. URLs on a webpage are crawled in order of appearance.
--
-- The default maximum links per page is 100.
webCrawlerConfiguration_maxLinksPerPage :: Lens.Lens' WebCrawlerConfiguration (Prelude.Maybe Prelude.Natural)
webCrawlerConfiguration_maxLinksPerPage = Lens.lens (\WebCrawlerConfiguration' {maxLinksPerPage} -> maxLinksPerPage) (\s@WebCrawlerConfiguration' {} a -> s {maxLinksPerPage = a} :: WebCrawlerConfiguration)

-- | The regular expression pattern to include certain URLs to crawl.
--
-- If there is a regular expression pattern to exclude certain URLs that
-- conflicts with the include pattern, the exclude pattern takes
-- precedence.
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
-- Amazon terms. Remember that you must only use the Amazon Kendra web
-- crawler to index your own webpages, or webpages that you have
-- authorization to index./
webCrawlerConfiguration_urls :: Lens.Lens' WebCrawlerConfiguration Urls
webCrawlerConfiguration_urls = Lens.lens (\WebCrawlerConfiguration' {urls} -> urls) (\s@WebCrawlerConfiguration' {} a -> s {urls = a} :: WebCrawlerConfiguration)

instance Core.FromJSON WebCrawlerConfiguration where
  parseJSON =
    Core.withObject
      "WebCrawlerConfiguration"
      ( \x ->
          WebCrawlerConfiguration'
            Prelude.<$> ( x Core..:? "UrlExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "MaxUrlsPerMinuteCrawlRate")
            Prelude.<*> (x Core..:? "CrawlDepth")
            Prelude.<*> (x Core..:? "MaxContentSizePerPageInMegaBytes")
            Prelude.<*> (x Core..:? "ProxyConfiguration")
            Prelude.<*> (x Core..:? "AuthenticationConfiguration")
            Prelude.<*> (x Core..:? "MaxLinksPerPage")
            Prelude.<*> ( x Core..:? "UrlInclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Urls")
      )

instance Prelude.Hashable WebCrawlerConfiguration

instance Prelude.NFData WebCrawlerConfiguration

instance Core.ToJSON WebCrawlerConfiguration where
  toJSON WebCrawlerConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UrlExclusionPatterns" Core..=)
              Prelude.<$> urlExclusionPatterns,
            ("MaxUrlsPerMinuteCrawlRate" Core..=)
              Prelude.<$> maxUrlsPerMinuteCrawlRate,
            ("CrawlDepth" Core..=) Prelude.<$> crawlDepth,
            ("MaxContentSizePerPageInMegaBytes" Core..=)
              Prelude.<$> maxContentSizePerPageInMegaBytes,
            ("ProxyConfiguration" Core..=)
              Prelude.<$> proxyConfiguration,
            ("AuthenticationConfiguration" Core..=)
              Prelude.<$> authenticationConfiguration,
            ("MaxLinksPerPage" Core..=)
              Prelude.<$> maxLinksPerPage,
            ("UrlInclusionPatterns" Core..=)
              Prelude.<$> urlInclusionPatterns,
            Prelude.Just ("Urls" Core..= urls)
          ]
      )
