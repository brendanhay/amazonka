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
-- Module      : Amazonka.CloudWatchLogs.StopQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a CloudWatch Logs Insights query that is in progress. If the query
-- has already ended, the operation returns an error indicating that the
-- specified query is not running.
module Amazonka.CloudWatchLogs.StopQuery
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopQuery' smart constructor.
data StopQuery = StopQuery'
  { -- | The ID number of the query to stop. To find this ID number, use
    -- @DescribeQueries@.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StopQuery where
  type AWSResponse StopQuery = StopQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopQueryResponse'
            Prelude.<$> (x Data..?> "success")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopQuery where
  hashWithSalt _salt StopQuery' {..} =
    _salt `Prelude.hashWithSalt` queryId

instance Prelude.NFData StopQuery where
  rnf StopQuery' {..} = Prelude.rnf queryId

instance Data.ToHeaders StopQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Logs_20140328.StopQuery" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopQuery where
  toJSON StopQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("queryId" Data..= queryId)]
      )

instance Data.ToPath StopQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery StopQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopQueryResponse' smart constructor.
data StopQueryResponse = StopQueryResponse'
  { -- | This is true if the query was stopped by the @StopQuery@ operation.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData StopQueryResponse where
  rnf StopQueryResponse' {..} =
    Prelude.rnf success
      `Prelude.seq` Prelude.rnf httpStatus
