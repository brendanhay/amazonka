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
-- Module      : Network.AWS.Athena.GetQueryExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single execution of a query if you have
-- access to the workgroup in which the query ran. Each time a query
-- executes, information about the query execution is saved with a unique
-- ID.
module Network.AWS.Athena.GetQueryExecution
  ( -- * Creating a Request
    GetQueryExecution (..),
    newGetQueryExecution,

    -- * Request Lenses
    getQueryExecution_queryExecutionId,

    -- * Destructuring the Response
    GetQueryExecutionResponse (..),
    newGetQueryExecutionResponse,

    -- * Response Lenses
    getQueryExecutionResponse_queryExecution,
    getQueryExecutionResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetQueryExecution' smart constructor.
data GetQueryExecution = GetQueryExecution'
  { -- | The unique ID of the query execution.
    queryExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetQueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionId', 'getQueryExecution_queryExecutionId' - The unique ID of the query execution.
newGetQueryExecution ::
  -- | 'queryExecutionId'
  Core.Text ->
  GetQueryExecution
newGetQueryExecution pQueryExecutionId_ =
  GetQueryExecution'
    { queryExecutionId =
        pQueryExecutionId_
    }

-- | The unique ID of the query execution.
getQueryExecution_queryExecutionId :: Lens.Lens' GetQueryExecution Core.Text
getQueryExecution_queryExecutionId = Lens.lens (\GetQueryExecution' {queryExecutionId} -> queryExecutionId) (\s@GetQueryExecution' {} a -> s {queryExecutionId = a} :: GetQueryExecution)

instance Core.AWSRequest GetQueryExecution where
  type
    AWSResponse GetQueryExecution =
      GetQueryExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryExecutionResponse'
            Core.<$> (x Core..?> "QueryExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetQueryExecution

instance Core.NFData GetQueryExecution

instance Core.ToHeaders GetQueryExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.GetQueryExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetQueryExecution where
  toJSON GetQueryExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QueryExecutionId" Core..= queryExecutionId)
          ]
      )

instance Core.ToPath GetQueryExecution where
  toPath = Core.const "/"

instance Core.ToQuery GetQueryExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetQueryExecutionResponse' smart constructor.
data GetQueryExecutionResponse = GetQueryExecutionResponse'
  { -- | Information about the query execution.
    queryExecution :: Core.Maybe QueryExecution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetQueryExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecution', 'getQueryExecutionResponse_queryExecution' - Information about the query execution.
--
-- 'httpStatus', 'getQueryExecutionResponse_httpStatus' - The response's http status code.
newGetQueryExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetQueryExecutionResponse
newGetQueryExecutionResponse pHttpStatus_ =
  GetQueryExecutionResponse'
    { queryExecution =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the query execution.
getQueryExecutionResponse_queryExecution :: Lens.Lens' GetQueryExecutionResponse (Core.Maybe QueryExecution)
getQueryExecutionResponse_queryExecution = Lens.lens (\GetQueryExecutionResponse' {queryExecution} -> queryExecution) (\s@GetQueryExecutionResponse' {} a -> s {queryExecution = a} :: GetQueryExecutionResponse)

-- | The response's http status code.
getQueryExecutionResponse_httpStatus :: Lens.Lens' GetQueryExecutionResponse Core.Int
getQueryExecutionResponse_httpStatus = Lens.lens (\GetQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@GetQueryExecutionResponse' {} a -> s {httpStatus = a} :: GetQueryExecutionResponse)

instance Core.NFData GetQueryExecutionResponse
