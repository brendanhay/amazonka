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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopQuery' smart constructor.
data StopQuery = StopQuery'
  { -- | The ID number of the query to stop. To find this ID number, use
    -- @DescribeQueries@.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopQuery
newStopQuery pQueryId_ =
  StopQuery' {queryId = pQueryId_}

-- | The ID number of the query to stop. To find this ID number, use
-- @DescribeQueries@.
stopQuery_queryId :: Lens.Lens' StopQuery Prelude.Text
stopQuery_queryId = Lens.lens (\StopQuery' {queryId} -> queryId) (\s@StopQuery' {} a -> s {queryId = a} :: StopQuery)

instance Prelude.AWSRequest StopQuery where
  type Rs StopQuery = StopQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopQueryResponse'
            Prelude.<$> (x Prelude..?> "success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopQuery

instance Prelude.NFData StopQuery

instance Prelude.ToHeaders StopQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("Logs_20140328.StopQuery" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopQuery where
  toJSON StopQuery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("queryId" Prelude..= queryId)]
      )

instance Prelude.ToPath StopQuery where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopQueryResponse' smart constructor.
data StopQueryResponse = StopQueryResponse'
  { -- | This is true if the query was stopped by the @StopQuery@ operation.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopQueryResponse
newStopQueryResponse pHttpStatus_ =
  StopQueryResponse'
    { success = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is true if the query was stopped by the @StopQuery@ operation.
stopQueryResponse_success :: Lens.Lens' StopQueryResponse (Prelude.Maybe Prelude.Bool)
stopQueryResponse_success = Lens.lens (\StopQueryResponse' {success} -> success) (\s@StopQueryResponse' {} a -> s {success = a} :: StopQueryResponse)

-- | The response's http status code.
stopQueryResponse_httpStatus :: Lens.Lens' StopQueryResponse Prelude.Int
stopQueryResponse_httpStatus = Lens.lens (\StopQueryResponse' {httpStatus} -> httpStatus) (\s@StopQueryResponse' {} a -> s {httpStatus = a} :: StopQueryResponse)

instance Prelude.NFData StopQueryResponse
