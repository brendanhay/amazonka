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
-- Module      : Network.AWS.Glue.StartCrawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a crawl using the specified crawler, regardless of what is
-- scheduled. If the crawler is already running, returns a
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-exceptions.html#aws-glue-api-exceptions-CrawlerRunningException CrawlerRunningException>.
module Network.AWS.Glue.StartCrawler
  ( -- * Creating a Request
    StartCrawler (..),
    newStartCrawler,

    -- * Request Lenses
    startCrawler_name,

    -- * Destructuring the Response
    StartCrawlerResponse (..),
    newStartCrawlerResponse,

    -- * Response Lenses
    startCrawlerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartCrawler' smart constructor.
data StartCrawler = StartCrawler'
  { -- | Name of the crawler to start.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartCrawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startCrawler_name' - Name of the crawler to start.
newStartCrawler ::
  -- | 'name'
  Core.Text ->
  StartCrawler
newStartCrawler pName_ = StartCrawler' {name = pName_}

-- | Name of the crawler to start.
startCrawler_name :: Lens.Lens' StartCrawler Core.Text
startCrawler_name = Lens.lens (\StartCrawler' {name} -> name) (\s@StartCrawler' {} a -> s {name = a} :: StartCrawler)

instance Core.AWSRequest StartCrawler where
  type AWSResponse StartCrawler = StartCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCrawlerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartCrawler

instance Core.NFData StartCrawler

instance Core.ToHeaders StartCrawler where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StartCrawler" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartCrawler where
  toJSON StartCrawler' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StartCrawler where
  toPath = Core.const "/"

instance Core.ToQuery StartCrawler where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartCrawlerResponse' smart constructor.
data StartCrawlerResponse = StartCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartCrawlerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startCrawlerResponse_httpStatus' - The response's http status code.
newStartCrawlerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartCrawlerResponse
newStartCrawlerResponse pHttpStatus_ =
  StartCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startCrawlerResponse_httpStatus :: Lens.Lens' StartCrawlerResponse Core.Int
startCrawlerResponse_httpStatus = Lens.lens (\StartCrawlerResponse' {httpStatus} -> httpStatus) (\s@StartCrawlerResponse' {} a -> s {httpStatus = a} :: StartCrawlerResponse)

instance Core.NFData StartCrawlerResponse
