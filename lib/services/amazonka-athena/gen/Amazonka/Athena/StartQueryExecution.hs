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
-- Module      : Amazonka.Athena.StartQueryExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs the SQL query statements contained in the @Query@. Requires you to
-- have access to the workgroup in which the query ran. Running queries
-- against an external catalog requires GetDataCatalog permission to the
-- catalog. For code samples using the Amazon Web Services SDK for Java,
-- see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Amazonka.Athena.StartQueryExecution
  ( -- * Creating a Request
    StartQueryExecution (..),
    newStartQueryExecution,

    -- * Request Lenses
    startQueryExecution_clientRequestToken,
    startQueryExecution_executionParameters,
    startQueryExecution_queryExecutionContext,
    startQueryExecution_resultConfiguration,
    startQueryExecution_resultReuseConfiguration,
    startQueryExecution_workGroup,
    startQueryExecution_queryString,

    -- * Destructuring the Response
    StartQueryExecutionResponse (..),
    newStartQueryExecutionResponse,

    -- * Response Lenses
    startQueryExecutionResponse_queryExecutionId,
    startQueryExecutionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartQueryExecution' smart constructor.
data StartQueryExecution = StartQueryExecution'
  { -- | A unique case-sensitive string used to ensure the request to create the
    -- query is idempotent (executes only once). If another
    -- @StartQueryExecution@ request is received, the same response is returned
    -- and another query is not created. If a parameter has changed, for
    -- example, the @QueryString@, an error is returned.
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for users. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A list of values for the parameters in a query. The values are applied
    -- sequentially to the parameters in the query in the order in which the
    -- parameters occur.
    executionParameters :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The database within which the query executes.
    queryExecutionContext :: Prelude.Maybe QueryExecutionContext,
    -- | Specifies information about where and how to save the results of the
    -- query execution. If the query runs in a workgroup, then workgroup\'s
    -- settings may override query settings. This affects the query results
    -- location. The workgroup settings override is specified in
    -- EnforceWorkGroupConfiguration (true\/false) in the
    -- WorkGroupConfiguration. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
    resultConfiguration :: Prelude.Maybe ResultConfiguration,
    -- | Specifies the query result reuse behavior for the query.
    resultReuseConfiguration :: Prelude.Maybe ResultReuseConfiguration,
    -- | The name of the workgroup in which the query is being started.
    workGroup :: Prelude.Maybe Prelude.Text,
    -- | The SQL query statements to be executed.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startQueryExecution_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- query is idempotent (executes only once). If another
-- @StartQueryExecution@ request is received, the same response is returned
-- and another query is not created. If a parameter has changed, for
-- example, the @QueryString@, an error is returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'executionParameters', 'startQueryExecution_executionParameters' - A list of values for the parameters in a query. The values are applied
-- sequentially to the parameters in the query in the order in which the
-- parameters occur.
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
-- 'resultReuseConfiguration', 'startQueryExecution_resultReuseConfiguration' - Specifies the query result reuse behavior for the query.
--
-- 'workGroup', 'startQueryExecution_workGroup' - The name of the workgroup in which the query is being started.
--
-- 'queryString', 'startQueryExecution_queryString' - The SQL query statements to be executed.
newStartQueryExecution ::
  -- | 'queryString'
  Prelude.Text ->
  StartQueryExecution
newStartQueryExecution pQueryString_ =
  StartQueryExecution'
    { clientRequestToken =
        Prelude.Nothing,
      executionParameters = Prelude.Nothing,
      queryExecutionContext = Prelude.Nothing,
      resultConfiguration = Prelude.Nothing,
      resultReuseConfiguration = Prelude.Nothing,
      workGroup = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | A unique case-sensitive string used to ensure the request to create the
-- query is idempotent (executes only once). If another
-- @StartQueryExecution@ request is received, the same response is returned
-- and another query is not created. If a parameter has changed, for
-- example, the @QueryString@, an error is returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
startQueryExecution_clientRequestToken :: Lens.Lens' StartQueryExecution (Prelude.Maybe Prelude.Text)
startQueryExecution_clientRequestToken = Lens.lens (\StartQueryExecution' {clientRequestToken} -> clientRequestToken) (\s@StartQueryExecution' {} a -> s {clientRequestToken = a} :: StartQueryExecution)

-- | A list of values for the parameters in a query. The values are applied
-- sequentially to the parameters in the query in the order in which the
-- parameters occur.
startQueryExecution_executionParameters :: Lens.Lens' StartQueryExecution (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
startQueryExecution_executionParameters = Lens.lens (\StartQueryExecution' {executionParameters} -> executionParameters) (\s@StartQueryExecution' {} a -> s {executionParameters = a} :: StartQueryExecution) Prelude.. Lens.mapping Lens.coerced

-- | The database within which the query executes.
startQueryExecution_queryExecutionContext :: Lens.Lens' StartQueryExecution (Prelude.Maybe QueryExecutionContext)
startQueryExecution_queryExecutionContext = Lens.lens (\StartQueryExecution' {queryExecutionContext} -> queryExecutionContext) (\s@StartQueryExecution' {} a -> s {queryExecutionContext = a} :: StartQueryExecution)

-- | Specifies information about where and how to save the results of the
-- query execution. If the query runs in a workgroup, then workgroup\'s
-- settings may override query settings. This affects the query results
-- location. The workgroup settings override is specified in
-- EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
startQueryExecution_resultConfiguration :: Lens.Lens' StartQueryExecution (Prelude.Maybe ResultConfiguration)
startQueryExecution_resultConfiguration = Lens.lens (\StartQueryExecution' {resultConfiguration} -> resultConfiguration) (\s@StartQueryExecution' {} a -> s {resultConfiguration = a} :: StartQueryExecution)

-- | Specifies the query result reuse behavior for the query.
startQueryExecution_resultReuseConfiguration :: Lens.Lens' StartQueryExecution (Prelude.Maybe ResultReuseConfiguration)
startQueryExecution_resultReuseConfiguration = Lens.lens (\StartQueryExecution' {resultReuseConfiguration} -> resultReuseConfiguration) (\s@StartQueryExecution' {} a -> s {resultReuseConfiguration = a} :: StartQueryExecution)

-- | The name of the workgroup in which the query is being started.
startQueryExecution_workGroup :: Lens.Lens' StartQueryExecution (Prelude.Maybe Prelude.Text)
startQueryExecution_workGroup = Lens.lens (\StartQueryExecution' {workGroup} -> workGroup) (\s@StartQueryExecution' {} a -> s {workGroup = a} :: StartQueryExecution)

-- | The SQL query statements to be executed.
startQueryExecution_queryString :: Lens.Lens' StartQueryExecution Prelude.Text
startQueryExecution_queryString = Lens.lens (\StartQueryExecution' {queryString} -> queryString) (\s@StartQueryExecution' {} a -> s {queryString = a} :: StartQueryExecution)

instance Core.AWSRequest StartQueryExecution where
  type
    AWSResponse StartQueryExecution =
      StartQueryExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryExecutionResponse'
            Prelude.<$> (x Data..?> "QueryExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartQueryExecution where
  hashWithSalt _salt StartQueryExecution' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` executionParameters
      `Prelude.hashWithSalt` queryExecutionContext
      `Prelude.hashWithSalt` resultConfiguration
      `Prelude.hashWithSalt` resultReuseConfiguration
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData StartQueryExecution where
  rnf StartQueryExecution' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf executionParameters
      `Prelude.seq` Prelude.rnf queryExecutionContext
      `Prelude.seq` Prelude.rnf resultConfiguration
      `Prelude.seq` Prelude.rnf resultReuseConfiguration
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders StartQueryExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.StartQueryExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartQueryExecution where
  toJSON StartQueryExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("ExecutionParameters" Data..=)
              Prelude.<$> executionParameters,
            ("QueryExecutionContext" Data..=)
              Prelude.<$> queryExecutionContext,
            ("ResultConfiguration" Data..=)
              Prelude.<$> resultConfiguration,
            ("ResultReuseConfiguration" Data..=)
              Prelude.<$> resultReuseConfiguration,
            ("WorkGroup" Data..=) Prelude.<$> workGroup,
            Prelude.Just ("QueryString" Data..= queryString)
          ]
      )

instance Data.ToPath StartQueryExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartQueryExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartQueryExecutionResponse' smart constructor.
data StartQueryExecutionResponse = StartQueryExecutionResponse'
  { -- | The unique ID of the query that ran as a result of this request.
    queryExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartQueryExecutionResponse
newStartQueryExecutionResponse pHttpStatus_ =
  StartQueryExecutionResponse'
    { queryExecutionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the query that ran as a result of this request.
startQueryExecutionResponse_queryExecutionId :: Lens.Lens' StartQueryExecutionResponse (Prelude.Maybe Prelude.Text)
startQueryExecutionResponse_queryExecutionId = Lens.lens (\StartQueryExecutionResponse' {queryExecutionId} -> queryExecutionId) (\s@StartQueryExecutionResponse' {} a -> s {queryExecutionId = a} :: StartQueryExecutionResponse)

-- | The response's http status code.
startQueryExecutionResponse_httpStatus :: Lens.Lens' StartQueryExecutionResponse Prelude.Int
startQueryExecutionResponse_httpStatus = Lens.lens (\StartQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@StartQueryExecutionResponse' {} a -> s {httpStatus = a} :: StartQueryExecutionResponse)

instance Prelude.NFData StartQueryExecutionResponse where
  rnf StartQueryExecutionResponse' {..} =
    Prelude.rnf queryExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
