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
-- Module      : Network.AWS.Athena.StopQueryExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a query execution. Requires you to have access to the workgroup in
-- which the query ran.
--
-- For code samples using the AWS SDK for Java, see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Network.AWS.Athena.StopQueryExecution
  ( -- * Creating a Request
    StopQueryExecution (..),
    newStopQueryExecution,

    -- * Request Lenses
    stopQueryExecution_queryExecutionId,

    -- * Destructuring the Response
    StopQueryExecutionResponse (..),
    newStopQueryExecutionResponse,

    -- * Response Lenses
    stopQueryExecutionResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopQueryExecution' smart constructor.
data StopQueryExecution = StopQueryExecution'
  { -- | The unique ID of the query execution to stop.
    queryExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopQueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryExecutionId', 'stopQueryExecution_queryExecutionId' - The unique ID of the query execution to stop.
newStopQueryExecution ::
  -- | 'queryExecutionId'
  Prelude.Text ->
  StopQueryExecution
newStopQueryExecution pQueryExecutionId_ =
  StopQueryExecution'
    { queryExecutionId =
        pQueryExecutionId_
    }

-- | The unique ID of the query execution to stop.
stopQueryExecution_queryExecutionId :: Lens.Lens' StopQueryExecution Prelude.Text
stopQueryExecution_queryExecutionId = Lens.lens (\StopQueryExecution' {queryExecutionId} -> queryExecutionId) (\s@StopQueryExecution' {} a -> s {queryExecutionId = a} :: StopQueryExecution)

instance Prelude.AWSRequest StopQueryExecution where
  type
    Rs StopQueryExecution =
      StopQueryExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopQueryExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopQueryExecution

instance Prelude.NFData StopQueryExecution

instance Prelude.ToHeaders StopQueryExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.StopQueryExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopQueryExecution where
  toJSON StopQueryExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueryExecutionId" Prelude..= queryExecutionId)
          ]
      )

instance Prelude.ToPath StopQueryExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopQueryExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopQueryExecutionResponse' smart constructor.
data StopQueryExecutionResponse = StopQueryExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopQueryExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopQueryExecutionResponse_httpStatus' - The response's http status code.
newStopQueryExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopQueryExecutionResponse
newStopQueryExecutionResponse pHttpStatus_ =
  StopQueryExecutionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopQueryExecutionResponse_httpStatus :: Lens.Lens' StopQueryExecutionResponse Prelude.Int
stopQueryExecutionResponse_httpStatus = Lens.lens (\StopQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@StopQueryExecutionResponse' {} a -> s {httpStatus = a} :: StopQueryExecutionResponse)

instance Prelude.NFData StopQueryExecutionResponse
