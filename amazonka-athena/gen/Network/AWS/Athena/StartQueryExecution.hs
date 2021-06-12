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
-- Module      : Network.AWS.Athena.StartQueryExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs the SQL query statements contained in the @Query@. Requires you to
-- have access to the workgroup in which the query ran. Running queries
-- against an external catalog requires GetDataCatalog permission to the
-- catalog. For code samples using the AWS SDK for Java, see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Network.AWS.Athena.StartQueryExecution
  ( -- * Creating a Request
    StartQueryExecution (..),
    newStartQueryExecution,

    -- * Request Lenses
    startQueryExecution_queryExecutionContext,
    startQueryExecution_resultConfiguration,
    startQueryExecution_workGroup,
    startQueryExecution_clientRequestToken,
    startQueryExecution_queryString,

    -- * Destructuring the Response
    StartQueryExecutionResponse (..),
    newStartQueryExecutionResponse,

    -- * Response Lenses
    startQueryExecutionResponse_queryExecutionId,
    startQueryExecutionResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartQueryExecution' smart constructor.
data StartQueryExecution = StartQueryExecution'
  { -- | The database within which the query executes.
    queryExecutionContext :: Core.Maybe QueryExecutionContext,
    -- | Specifies information about where and how to save the results of the
    -- query execution. If the query runs in a workgroup, then workgroup\'s
    -- settings may override query settings. This affects the query results
    -- location. The workgroup settings override is specified in
    -- EnforceWorkGroupConfiguration (true\/false) in the
    -- WorkGroupConfiguration. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
    resultConfiguration :: Core.Maybe ResultConfiguration,
    -- | The name of the workgroup in which the query is being started.
    workGroup :: Core.Maybe Core.Text,
    -- | A unique case-sensitive string used to ensure the request to create the
    -- query is idempotent (executes only once). If another
    -- @StartQueryExecution@ request is received, the same response is returned
    -- and another query is not created. If a parameter has changed, for
    -- example, the @QueryString@, an error is returned.
    --
    -- This token is listed as not required because AWS SDKs (for example the
    -- AWS SDK for Java) auto-generate the token for users. If you are not
    -- using the AWS SDK or the AWS CLI, you must provide this token or the
    -- action will fail.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The SQL query statements to be executed.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartQueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionContext', 'startQueryExecution_queryExecutionContext' - The database within which the query executes.
--
-- 'resultConfiguration', 'startQueryExecution_resultConfiguration' - Specifies information about where and how to save the results of the
-- query execution. If the query runs in a workgroup, then workgroup\'s
-- settings may override query settings. This affects the query results
-- location. The workgroup settings override is specified in
-- EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
--
-- 'workGroup', 'startQueryExecution_workGroup' - The name of the workgroup in which the query is being started.
--
-- 'clientRequestToken', 'startQueryExecution_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- query is idempotent (executes only once). If another
-- @StartQueryExecution@ request is received, the same response is returned
-- and another query is not created. If a parameter has changed, for
-- example, the @QueryString@, an error is returned.
--
-- This token is listed as not required because AWS SDKs (for example the
-- AWS SDK for Java) auto-generate the token for users. If you are not
-- using the AWS SDK or the AWS CLI, you must provide this token or the
-- action will fail.
--
-- 'queryString', 'startQueryExecution_queryString' - The SQL query statements to be executed.
newStartQueryExecution ::
  -- | 'queryString'
  Core.Text ->
  StartQueryExecution
newStartQueryExecution pQueryString_ =
  StartQueryExecution'
    { queryExecutionContext =
        Core.Nothing,
      resultConfiguration = Core.Nothing,
      workGroup = Core.Nothing,
      clientRequestToken = Core.Nothing,
      queryString = pQueryString_
    }

-- | The database within which the query executes.
startQueryExecution_queryExecutionContext :: Lens.Lens' StartQueryExecution (Core.Maybe QueryExecutionContext)
startQueryExecution_queryExecutionContext = Lens.lens (\StartQueryExecution' {queryExecutionContext} -> queryExecutionContext) (\s@StartQueryExecution' {} a -> s {queryExecutionContext = a} :: StartQueryExecution)

-- | Specifies information about where and how to save the results of the
-- query execution. If the query runs in a workgroup, then workgroup\'s
-- settings may override query settings. This affects the query results
-- location. The workgroup settings override is specified in
-- EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
startQueryExecution_resultConfiguration :: Lens.Lens' StartQueryExecution (Core.Maybe ResultConfiguration)
startQueryExecution_resultConfiguration = Lens.lens (\StartQueryExecution' {resultConfiguration} -> resultConfiguration) (\s@StartQueryExecution' {} a -> s {resultConfiguration = a} :: StartQueryExecution)

-- | The name of the workgroup in which the query is being started.
startQueryExecution_workGroup :: Lens.Lens' StartQueryExecution (Core.Maybe Core.Text)
startQueryExecution_workGroup = Lens.lens (\StartQueryExecution' {workGroup} -> workGroup) (\s@StartQueryExecution' {} a -> s {workGroup = a} :: StartQueryExecution)

-- | A unique case-sensitive string used to ensure the request to create the
-- query is idempotent (executes only once). If another
-- @StartQueryExecution@ request is received, the same response is returned
-- and another query is not created. If a parameter has changed, for
-- example, the @QueryString@, an error is returned.
--
-- This token is listed as not required because AWS SDKs (for example the
-- AWS SDK for Java) auto-generate the token for users. If you are not
-- using the AWS SDK or the AWS CLI, you must provide this token or the
-- action will fail.
startQueryExecution_clientRequestToken :: Lens.Lens' StartQueryExecution (Core.Maybe Core.Text)
startQueryExecution_clientRequestToken = Lens.lens (\StartQueryExecution' {clientRequestToken} -> clientRequestToken) (\s@StartQueryExecution' {} a -> s {clientRequestToken = a} :: StartQueryExecution)

-- | The SQL query statements to be executed.
startQueryExecution_queryString :: Lens.Lens' StartQueryExecution Core.Text
startQueryExecution_queryString = Lens.lens (\StartQueryExecution' {queryString} -> queryString) (\s@StartQueryExecution' {} a -> s {queryString = a} :: StartQueryExecution)

instance Core.AWSRequest StartQueryExecution where
  type
    AWSResponse StartQueryExecution =
      StartQueryExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryExecutionResponse'
            Core.<$> (x Core..?> "QueryExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartQueryExecution

instance Core.NFData StartQueryExecution

instance Core.ToHeaders StartQueryExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonAthena.StartQueryExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartQueryExecution where
  toJSON StartQueryExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("QueryExecutionContext" Core..=)
              Core.<$> queryExecutionContext,
            ("ResultConfiguration" Core..=)
              Core.<$> resultConfiguration,
            ("WorkGroup" Core..=) Core.<$> workGroup,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("QueryString" Core..= queryString)
          ]
      )

instance Core.ToPath StartQueryExecution where
  toPath = Core.const "/"

instance Core.ToQuery StartQueryExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartQueryExecutionResponse' smart constructor.
data StartQueryExecutionResponse = StartQueryExecutionResponse'
  { -- | The unique ID of the query that ran as a result of this request.
    queryExecutionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartQueryExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionId', 'startQueryExecutionResponse_queryExecutionId' - The unique ID of the query that ran as a result of this request.
--
-- 'httpStatus', 'startQueryExecutionResponse_httpStatus' - The response's http status code.
newStartQueryExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartQueryExecutionResponse
newStartQueryExecutionResponse pHttpStatus_ =
  StartQueryExecutionResponse'
    { queryExecutionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the query that ran as a result of this request.
startQueryExecutionResponse_queryExecutionId :: Lens.Lens' StartQueryExecutionResponse (Core.Maybe Core.Text)
startQueryExecutionResponse_queryExecutionId = Lens.lens (\StartQueryExecutionResponse' {queryExecutionId} -> queryExecutionId) (\s@StartQueryExecutionResponse' {} a -> s {queryExecutionId = a} :: StartQueryExecutionResponse)

-- | The response's http status code.
startQueryExecutionResponse_httpStatus :: Lens.Lens' StartQueryExecutionResponse Core.Int
startQueryExecutionResponse_httpStatus = Lens.lens (\StartQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@StartQueryExecutionResponse' {} a -> s {httpStatus = a} :: StartQueryExecutionResponse)

instance Core.NFData StartQueryExecutionResponse
