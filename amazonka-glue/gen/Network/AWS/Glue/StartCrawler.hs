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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartCrawler' smart constructor.
data StartCrawler = StartCrawler'
  { -- | Name of the crawler to start.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StartCrawler
newStartCrawler pName_ = StartCrawler' {name = pName_}

-- | Name of the crawler to start.
startCrawler_name :: Lens.Lens' StartCrawler Prelude.Text
startCrawler_name = Lens.lens (\StartCrawler' {name} -> name) (\s@StartCrawler' {} a -> s {name = a} :: StartCrawler)

instance Prelude.AWSRequest StartCrawler where
  type Rs StartCrawler = StartCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCrawlerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCrawler

instance Prelude.NFData StartCrawler

instance Prelude.ToHeaders StartCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.StartCrawler" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartCrawler where
  toJSON StartCrawler' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath StartCrawler where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCrawlerResponse' smart constructor.
data StartCrawlerResponse = StartCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StartCrawlerResponse
newStartCrawlerResponse pHttpStatus_ =
  StartCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startCrawlerResponse_httpStatus :: Lens.Lens' StartCrawlerResponse Prelude.Int
startCrawlerResponse_httpStatus = Lens.lens (\StartCrawlerResponse' {httpStatus} -> httpStatus) (\s@StartCrawlerResponse' {} a -> s {httpStatus = a} :: StartCrawlerResponse)

instance Prelude.NFData StartCrawlerResponse
