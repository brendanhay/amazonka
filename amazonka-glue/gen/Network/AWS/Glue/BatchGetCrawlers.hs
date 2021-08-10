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
-- Module      : Network.AWS.Glue.BatchGetCrawlers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of crawler names.
-- After calling the @ListCrawlers@ operation, you can call this operation
-- to access the data to which you have been granted permissions. This
-- operation supports all IAM permissions, including permission conditions
-- that uses tags.
module Network.AWS.Glue.BatchGetCrawlers
  ( -- * Creating a Request
    BatchGetCrawlers (..),
    newBatchGetCrawlers,

    -- * Request Lenses
    batchGetCrawlers_crawlerNames,

    -- * Destructuring the Response
    BatchGetCrawlersResponse (..),
    newBatchGetCrawlersResponse,

    -- * Response Lenses
    batchGetCrawlersResponse_crawlers,
    batchGetCrawlersResponse_crawlersNotFound,
    batchGetCrawlersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetCrawlers' smart constructor.
data BatchGetCrawlers = BatchGetCrawlers'
  { -- | A list of crawler names, which might be the names returned from the
    -- @ListCrawlers@ operation.
    crawlerNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCrawlers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerNames', 'batchGetCrawlers_crawlerNames' - A list of crawler names, which might be the names returned from the
-- @ListCrawlers@ operation.
newBatchGetCrawlers ::
  BatchGetCrawlers
newBatchGetCrawlers =
  BatchGetCrawlers' {crawlerNames = Prelude.mempty}

-- | A list of crawler names, which might be the names returned from the
-- @ListCrawlers@ operation.
batchGetCrawlers_crawlerNames :: Lens.Lens' BatchGetCrawlers [Prelude.Text]
batchGetCrawlers_crawlerNames = Lens.lens (\BatchGetCrawlers' {crawlerNames} -> crawlerNames) (\s@BatchGetCrawlers' {} a -> s {crawlerNames = a} :: BatchGetCrawlers) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetCrawlers where
  type
    AWSResponse BatchGetCrawlers =
      BatchGetCrawlersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetCrawlersResponse'
            Prelude.<$> (x Core..?> "Crawlers" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "CrawlersNotFound"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetCrawlers

instance Prelude.NFData BatchGetCrawlers

instance Core.ToHeaders BatchGetCrawlers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchGetCrawlers" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetCrawlers where
  toJSON BatchGetCrawlers' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("CrawlerNames" Core..= crawlerNames)]
      )

instance Core.ToPath BatchGetCrawlers where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetCrawlers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetCrawlersResponse' smart constructor.
data BatchGetCrawlersResponse = BatchGetCrawlersResponse'
  { -- | A list of crawler definitions.
    crawlers :: Prelude.Maybe [Crawler],
    -- | A list of names of crawlers that were not found.
    crawlersNotFound :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCrawlersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlers', 'batchGetCrawlersResponse_crawlers' - A list of crawler definitions.
--
-- 'crawlersNotFound', 'batchGetCrawlersResponse_crawlersNotFound' - A list of names of crawlers that were not found.
--
-- 'httpStatus', 'batchGetCrawlersResponse_httpStatus' - The response's http status code.
newBatchGetCrawlersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetCrawlersResponse
newBatchGetCrawlersResponse pHttpStatus_ =
  BatchGetCrawlersResponse'
    { crawlers =
        Prelude.Nothing,
      crawlersNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of crawler definitions.
batchGetCrawlersResponse_crawlers :: Lens.Lens' BatchGetCrawlersResponse (Prelude.Maybe [Crawler])
batchGetCrawlersResponse_crawlers = Lens.lens (\BatchGetCrawlersResponse' {crawlers} -> crawlers) (\s@BatchGetCrawlersResponse' {} a -> s {crawlers = a} :: BatchGetCrawlersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of names of crawlers that were not found.
batchGetCrawlersResponse_crawlersNotFound :: Lens.Lens' BatchGetCrawlersResponse (Prelude.Maybe [Prelude.Text])
batchGetCrawlersResponse_crawlersNotFound = Lens.lens (\BatchGetCrawlersResponse' {crawlersNotFound} -> crawlersNotFound) (\s@BatchGetCrawlersResponse' {} a -> s {crawlersNotFound = a} :: BatchGetCrawlersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetCrawlersResponse_httpStatus :: Lens.Lens' BatchGetCrawlersResponse Prelude.Int
batchGetCrawlersResponse_httpStatus = Lens.lens (\BatchGetCrawlersResponse' {httpStatus} -> httpStatus) (\s@BatchGetCrawlersResponse' {} a -> s {httpStatus = a} :: BatchGetCrawlersResponse)

instance Prelude.NFData BatchGetCrawlersResponse
