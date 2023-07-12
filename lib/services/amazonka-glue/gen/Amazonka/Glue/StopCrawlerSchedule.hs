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
-- Module      : Amazonka.Glue.StopCrawlerSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the schedule state of the specified crawler to @NOT_SCHEDULED@, but
-- does not stop the crawler if it is already running.
module Amazonka.Glue.StopCrawlerSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopCrawlerSchedule' smart constructor.
data StopCrawlerSchedule = StopCrawlerSchedule'
  { -- | Name of the crawler whose schedule state to set.
    crawlerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StopCrawlerSchedule where
  type
    AWSResponse StopCrawlerSchedule =
      StopCrawlerScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCrawlerScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopCrawlerSchedule where
  hashWithSalt _salt StopCrawlerSchedule' {..} =
    _salt `Prelude.hashWithSalt` crawlerName

instance Prelude.NFData StopCrawlerSchedule where
  rnf StopCrawlerSchedule' {..} =
    Prelude.rnf crawlerName

instance Data.ToHeaders StopCrawlerSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.StopCrawlerSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopCrawlerSchedule where
  toJSON StopCrawlerSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CrawlerName" Data..= crawlerName)]
      )

instance Data.ToPath StopCrawlerSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery StopCrawlerSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCrawlerScheduleResponse' smart constructor.
data StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData StopCrawlerScheduleResponse where
  rnf StopCrawlerScheduleResponse' {..} =
    Prelude.rnf httpStatus
