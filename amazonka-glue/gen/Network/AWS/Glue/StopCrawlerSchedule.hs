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
-- Module      : Network.AWS.Glue.StopCrawlerSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the schedule state of the specified crawler to @NOT_SCHEDULED@, but
-- does not stop the crawler if it is already running.
module Network.AWS.Glue.StopCrawlerSchedule
  ( -- * Creating a Request
    StopCrawlerSchedule (..),
    newStopCrawlerSchedule,

    -- * Request Lenses
    stopCrawlerSchedule_crawlerName,

    -- * Destructuring the Response
    StopCrawlerScheduleResponse (..),
    newStopCrawlerScheduleResponse,

    -- * Response Lenses
    stopCrawlerScheduleResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopCrawlerSchedule' smart constructor.
data StopCrawlerSchedule = StopCrawlerSchedule'
  { -- | Name of the crawler whose schedule state to set.
    crawlerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopCrawlerSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerName', 'stopCrawlerSchedule_crawlerName' - Name of the crawler whose schedule state to set.
newStopCrawlerSchedule ::
  -- | 'crawlerName'
  Prelude.Text ->
  StopCrawlerSchedule
newStopCrawlerSchedule pCrawlerName_ =
  StopCrawlerSchedule' {crawlerName = pCrawlerName_}

-- | Name of the crawler whose schedule state to set.
stopCrawlerSchedule_crawlerName :: Lens.Lens' StopCrawlerSchedule Prelude.Text
stopCrawlerSchedule_crawlerName = Lens.lens (\StopCrawlerSchedule' {crawlerName} -> crawlerName) (\s@StopCrawlerSchedule' {} a -> s {crawlerName = a} :: StopCrawlerSchedule)

instance Prelude.AWSRequest StopCrawlerSchedule where
  type
    Rs StopCrawlerSchedule =
      StopCrawlerScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCrawlerScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopCrawlerSchedule

instance Prelude.NFData StopCrawlerSchedule

instance Prelude.ToHeaders StopCrawlerSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.StopCrawlerSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopCrawlerSchedule where
  toJSON StopCrawlerSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CrawlerName" Prelude..= crawlerName)
          ]
      )

instance Prelude.ToPath StopCrawlerSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopCrawlerSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCrawlerScheduleResponse' smart constructor.
data StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopCrawlerScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopCrawlerScheduleResponse_httpStatus' - The response's http status code.
newStopCrawlerScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopCrawlerScheduleResponse
newStopCrawlerScheduleResponse pHttpStatus_ =
  StopCrawlerScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopCrawlerScheduleResponse_httpStatus :: Lens.Lens' StopCrawlerScheduleResponse Prelude.Int
stopCrawlerScheduleResponse_httpStatus = Lens.lens (\StopCrawlerScheduleResponse' {httpStatus} -> httpStatus) (\s@StopCrawlerScheduleResponse' {} a -> s {httpStatus = a} :: StopCrawlerScheduleResponse)

instance Prelude.NFData StopCrawlerScheduleResponse
