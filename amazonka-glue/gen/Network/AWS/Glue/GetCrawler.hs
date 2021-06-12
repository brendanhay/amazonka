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
-- Module      : Network.AWS.Glue.GetCrawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a specified crawler.
module Network.AWS.Glue.GetCrawler
  ( -- * Creating a Request
    GetCrawler (..),
    newGetCrawler,

    -- * Request Lenses
    getCrawler_name,

    -- * Destructuring the Response
    GetCrawlerResponse (..),
    newGetCrawlerResponse,

    -- * Response Lenses
    getCrawlerResponse_crawler,
    getCrawlerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCrawler' smart constructor.
data GetCrawler = GetCrawler'
  { -- | The name of the crawler to retrieve metadata for.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCrawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getCrawler_name' - The name of the crawler to retrieve metadata for.
newGetCrawler ::
  -- | 'name'
  Core.Text ->
  GetCrawler
newGetCrawler pName_ = GetCrawler' {name = pName_}

-- | The name of the crawler to retrieve metadata for.
getCrawler_name :: Lens.Lens' GetCrawler Core.Text
getCrawler_name = Lens.lens (\GetCrawler' {name} -> name) (\s@GetCrawler' {} a -> s {name = a} :: GetCrawler)

instance Core.AWSRequest GetCrawler where
  type AWSResponse GetCrawler = GetCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlerResponse'
            Core.<$> (x Core..?> "Crawler")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCrawler

instance Core.NFData GetCrawler

instance Core.ToHeaders GetCrawler where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetCrawler" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCrawler where
  toJSON GetCrawler' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath GetCrawler where
  toPath = Core.const "/"

instance Core.ToQuery GetCrawler where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'
  { -- | The metadata for the specified crawler.
    crawler :: Core.Maybe Crawler,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCrawlerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawler', 'getCrawlerResponse_crawler' - The metadata for the specified crawler.
--
-- 'httpStatus', 'getCrawlerResponse_httpStatus' - The response's http status code.
newGetCrawlerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCrawlerResponse
newGetCrawlerResponse pHttpStatus_ =
  GetCrawlerResponse'
    { crawler = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata for the specified crawler.
getCrawlerResponse_crawler :: Lens.Lens' GetCrawlerResponse (Core.Maybe Crawler)
getCrawlerResponse_crawler = Lens.lens (\GetCrawlerResponse' {crawler} -> crawler) (\s@GetCrawlerResponse' {} a -> s {crawler = a} :: GetCrawlerResponse)

-- | The response's http status code.
getCrawlerResponse_httpStatus :: Lens.Lens' GetCrawlerResponse Core.Int
getCrawlerResponse_httpStatus = Lens.lens (\GetCrawlerResponse' {httpStatus} -> httpStatus) (\s@GetCrawlerResponse' {} a -> s {httpStatus = a} :: GetCrawlerResponse)

instance Core.NFData GetCrawlerResponse
