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
-- Module      : Network.AWS.CloudWatchLogs.StopQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a CloudWatch Logs Insights query that is in progress. If the query
-- has already ended, the operation returns an error indicating that the
-- specified query is not running.
module Network.AWS.CloudWatchLogs.StopQuery
  ( -- * Creating a Request
    StopQuery (..),
    newStopQuery,

    -- * Request Lenses
    stopQuery_queryId,

    -- * Destructuring the Response
    StopQueryResponse (..),
    newStopQueryResponse,

    -- * Response Lenses
    stopQueryResponse_success,
    stopQueryResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopQuery' smart constructor.
data StopQuery = StopQuery'
  { -- | The ID number of the query to stop. To find this ID number, use
    -- @DescribeQueries@.
    queryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'stopQuery_queryId' - The ID number of the query to stop. To find this ID number, use
-- @DescribeQueries@.
newStopQuery ::
  -- | 'queryId'
  Core.Text ->
  StopQuery
newStopQuery pQueryId_ =
  StopQuery' {queryId = pQueryId_}

-- | The ID number of the query to stop. To find this ID number, use
-- @DescribeQueries@.
stopQuery_queryId :: Lens.Lens' StopQuery Core.Text
stopQuery_queryId = Lens.lens (\StopQuery' {queryId} -> queryId) (\s@StopQuery' {} a -> s {queryId = a} :: StopQuery)

instance Core.AWSRequest StopQuery where
  type AWSResponse StopQuery = StopQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopQueryResponse'
            Core.<$> (x Core..?> "success")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopQuery

instance Core.NFData StopQuery

instance Core.ToHeaders StopQuery where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.StopQuery" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopQuery where
  toJSON StopQuery' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("queryId" Core..= queryId)]
      )

instance Core.ToPath StopQuery where
  toPath = Core.const "/"

instance Core.ToQuery StopQuery where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopQueryResponse' smart constructor.
data StopQueryResponse = StopQueryResponse'
  { -- | This is true if the query was stopped by the @StopQuery@ operation.
    success :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'success', 'stopQueryResponse_success' - This is true if the query was stopped by the @StopQuery@ operation.
--
-- 'httpStatus', 'stopQueryResponse_httpStatus' - The response's http status code.
newStopQueryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopQueryResponse
newStopQueryResponse pHttpStatus_ =
  StopQueryResponse'
    { success = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is true if the query was stopped by the @StopQuery@ operation.
stopQueryResponse_success :: Lens.Lens' StopQueryResponse (Core.Maybe Core.Bool)
stopQueryResponse_success = Lens.lens (\StopQueryResponse' {success} -> success) (\s@StopQueryResponse' {} a -> s {success = a} :: StopQueryResponse)

-- | The response's http status code.
stopQueryResponse_httpStatus :: Lens.Lens' StopQueryResponse Core.Int
stopQueryResponse_httpStatus = Lens.lens (\StopQueryResponse' {httpStatus} -> httpStatus) (\s@StopQueryResponse' {} a -> s {httpStatus = a} :: StopQueryResponse)

instance Core.NFData StopQueryResponse
