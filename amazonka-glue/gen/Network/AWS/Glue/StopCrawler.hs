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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopCrawler' smart constructor.
data StopCrawler = StopCrawler'
  { -- | Name of the crawler to stop.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopCrawler
newStopCrawler pName_ = StopCrawler' {name = pName_}

-- | Name of the crawler to stop.
stopCrawler_name :: Lens.Lens' StopCrawler Prelude.Text
stopCrawler_name = Lens.lens (\StopCrawler' {name} -> name) (\s@StopCrawler' {} a -> s {name = a} :: StopCrawler)

instance Prelude.AWSRequest StopCrawler where
  type Rs StopCrawler = StopCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCrawlerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopCrawler

instance Prelude.NFData StopCrawler

instance Prelude.ToHeaders StopCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.StopCrawler" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopCrawler where
  toJSON StopCrawler' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StopCrawler where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCrawlerResponse' smart constructor.
data StopCrawlerResponse = StopCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopCrawlerResponse
newStopCrawlerResponse pHttpStatus_ =
  StopCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopCrawlerResponse_httpStatus :: Lens.Lens' StopCrawlerResponse Prelude.Int
stopCrawlerResponse_httpStatus = Lens.lens (\StopCrawlerResponse' {httpStatus} -> httpStatus) (\s@StopCrawlerResponse' {} a -> s {httpStatus = a} :: StopCrawlerResponse)

instance Prelude.NFData StopCrawlerResponse
