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
-- Module      : Network.AWS.Glue.StartCrawlerSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the schedule state of the specified crawler to @SCHEDULED@,
-- unless the crawler is already running or the schedule state is already
-- @SCHEDULED@.
module Network.AWS.Glue.StartCrawlerSchedule
  ( -- * Creating a Request
    StartCrawlerSchedule (..),
    newStartCrawlerSchedule,

    -- * Request Lenses
    startCrawlerSchedule_crawlerName,

    -- * Destructuring the Response
    StartCrawlerScheduleResponse (..),
    newStartCrawlerScheduleResponse,

    -- * Response Lenses
    startCrawlerScheduleResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartCrawlerSchedule' smart constructor.
data StartCrawlerSchedule = StartCrawlerSchedule'
  { -- | Name of the crawler to schedule.
    crawlerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartCrawlerSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerName', 'startCrawlerSchedule_crawlerName' - Name of the crawler to schedule.
newStartCrawlerSchedule ::
  -- | 'crawlerName'
  Prelude.Text ->
  StartCrawlerSchedule
newStartCrawlerSchedule pCrawlerName_ =
  StartCrawlerSchedule' {crawlerName = pCrawlerName_}

-- | Name of the crawler to schedule.
startCrawlerSchedule_crawlerName :: Lens.Lens' StartCrawlerSchedule Prelude.Text
startCrawlerSchedule_crawlerName = Lens.lens (\StartCrawlerSchedule' {crawlerName} -> crawlerName) (\s@StartCrawlerSchedule' {} a -> s {crawlerName = a} :: StartCrawlerSchedule)

instance Prelude.AWSRequest StartCrawlerSchedule where
  type
    Rs StartCrawlerSchedule =
      StartCrawlerScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCrawlerScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCrawlerSchedule

instance Prelude.NFData StartCrawlerSchedule

instance Prelude.ToHeaders StartCrawlerSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.StartCrawlerSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartCrawlerSchedule where
  toJSON StartCrawlerSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CrawlerName" Prelude..= crawlerName)
          ]
      )

instance Prelude.ToPath StartCrawlerSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartCrawlerSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCrawlerScheduleResponse' smart constructor.
data StartCrawlerScheduleResponse = StartCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartCrawlerScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startCrawlerScheduleResponse_httpStatus' - The response's http status code.
newStartCrawlerScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCrawlerScheduleResponse
newStartCrawlerScheduleResponse pHttpStatus_ =
  StartCrawlerScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startCrawlerScheduleResponse_httpStatus :: Lens.Lens' StartCrawlerScheduleResponse Prelude.Int
startCrawlerScheduleResponse_httpStatus = Lens.lens (\StartCrawlerScheduleResponse' {httpStatus} -> httpStatus) (\s@StartCrawlerScheduleResponse' {} a -> s {httpStatus = a} :: StartCrawlerScheduleResponse)

instance Prelude.NFData StartCrawlerScheduleResponse
