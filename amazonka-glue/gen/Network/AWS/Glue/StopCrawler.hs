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
-- Module      : Network.AWS.Glue.StopCrawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the specified crawler is running, stops the crawl.
module Network.AWS.Glue.StopCrawler
  ( -- * Creating a Request
    StopCrawler (..),
    newStopCrawler,

    -- * Request Lenses
    stopCrawler_name,

    -- * Destructuring the Response
    StopCrawlerResponse (..),
    newStopCrawlerResponse,

    -- * Response Lenses
    stopCrawlerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopCrawler' smart constructor.
data StopCrawler = StopCrawler'
  { -- | Name of the crawler to stop.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopCrawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopCrawler_name' - Name of the crawler to stop.
newStopCrawler ::
  -- | 'name'
  Core.Text ->
  StopCrawler
newStopCrawler pName_ = StopCrawler' {name = pName_}

-- | Name of the crawler to stop.
stopCrawler_name :: Lens.Lens' StopCrawler Core.Text
stopCrawler_name = Lens.lens (\StopCrawler' {name} -> name) (\s@StopCrawler' {} a -> s {name = a} :: StopCrawler)

instance Core.AWSRequest StopCrawler where
  type AWSResponse StopCrawler = StopCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCrawlerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopCrawler

instance Core.NFData StopCrawler

instance Core.ToHeaders StopCrawler where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StopCrawler" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopCrawler where
  toJSON StopCrawler' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StopCrawler where
  toPath = Core.const "/"

instance Core.ToQuery StopCrawler where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopCrawlerResponse' smart constructor.
data StopCrawlerResponse = StopCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopCrawlerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopCrawlerResponse_httpStatus' - The response's http status code.
newStopCrawlerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopCrawlerResponse
newStopCrawlerResponse pHttpStatus_ =
  StopCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopCrawlerResponse_httpStatus :: Lens.Lens' StopCrawlerResponse Core.Int
stopCrawlerResponse_httpStatus = Lens.lens (\StopCrawlerResponse' {httpStatus} -> httpStatus) (\s@StopCrawlerResponse' {} a -> s {httpStatus = a} :: StopCrawlerResponse)

instance Core.NFData StopCrawlerResponse
