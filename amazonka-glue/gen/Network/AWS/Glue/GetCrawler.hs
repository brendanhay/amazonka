{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCrawler' smart constructor.
data GetCrawler = GetCrawler'
  { -- | The name of the crawler to retrieve metadata for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetCrawler
newGetCrawler pName_ = GetCrawler' {name = pName_}

-- | The name of the crawler to retrieve metadata for.
getCrawler_name :: Lens.Lens' GetCrawler Prelude.Text
getCrawler_name = Lens.lens (\GetCrawler' {name} -> name) (\s@GetCrawler' {} a -> s {name = a} :: GetCrawler)

instance Prelude.AWSRequest GetCrawler where
  type Rs GetCrawler = GetCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlerResponse'
            Prelude.<$> (x Prelude..?> "Crawler")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCrawler

instance Prelude.NFData GetCrawler

instance Prelude.ToHeaders GetCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetCrawler" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCrawler where
  toJSON GetCrawler' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath GetCrawler where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'
  { -- | The metadata for the specified crawler.
    crawler :: Prelude.Maybe Crawler,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetCrawlerResponse
newGetCrawlerResponse pHttpStatus_ =
  GetCrawlerResponse'
    { crawler = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata for the specified crawler.
getCrawlerResponse_crawler :: Lens.Lens' GetCrawlerResponse (Prelude.Maybe Crawler)
getCrawlerResponse_crawler = Lens.lens (\GetCrawlerResponse' {crawler} -> crawler) (\s@GetCrawlerResponse' {} a -> s {crawler = a} :: GetCrawlerResponse)

-- | The response's http status code.
getCrawlerResponse_httpStatus :: Lens.Lens' GetCrawlerResponse Prelude.Int
getCrawlerResponse_httpStatus = Lens.lens (\GetCrawlerResponse' {httpStatus} -> httpStatus) (\s@GetCrawlerResponse' {} a -> s {httpStatus = a} :: GetCrawlerResponse)

instance Prelude.NFData GetCrawlerResponse
