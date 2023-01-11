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
-- Module      : Amazonka.Glue.StartCrawlerSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the schedule state of the specified crawler to @SCHEDULED@,
-- unless the crawler is already running or the schedule state is already
-- @SCHEDULED@.
module Amazonka.Glue.StartCrawlerSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartCrawlerSchedule' smart constructor.
data StartCrawlerSchedule = StartCrawlerSchedule'
  { -- | Name of the crawler to schedule.
    crawlerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StartCrawlerSchedule where
  type
    AWSResponse StartCrawlerSchedule =
      StartCrawlerScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCrawlerScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCrawlerSchedule where
  hashWithSalt _salt StartCrawlerSchedule' {..} =
    _salt `Prelude.hashWithSalt` crawlerName

instance Prelude.NFData StartCrawlerSchedule where
  rnf StartCrawlerSchedule' {..} =
    Prelude.rnf crawlerName

instance Data.ToHeaders StartCrawlerSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.StartCrawlerSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCrawlerSchedule where
  toJSON StartCrawlerSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CrawlerName" Data..= crawlerName)]
      )

instance Data.ToPath StartCrawlerSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery StartCrawlerSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCrawlerScheduleResponse' smart constructor.
data StartCrawlerScheduleResponse = StartCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData StartCrawlerScheduleResponse where
  rnf StartCrawlerScheduleResponse' {..} =
    Prelude.rnf httpStatus
