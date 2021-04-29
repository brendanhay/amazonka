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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetQueryExecution' smart constructor.
data GetQueryExecution = GetQueryExecution'
  { -- | The unique ID of the query execution.
    queryExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetQueryExecution
newGetQueryExecution pQueryExecutionId_ =
  GetQueryExecution'
    { queryExecutionId =
        pQueryExecutionId_
    }

-- | The unique ID of the query execution.
getQueryExecution_queryExecutionId :: Lens.Lens' GetQueryExecution Prelude.Text
getQueryExecution_queryExecutionId = Lens.lens (\GetQueryExecution' {queryExecutionId} -> queryExecutionId) (\s@GetQueryExecution' {} a -> s {queryExecutionId = a} :: GetQueryExecution)

instance Prelude.AWSRequest GetQueryExecution where
  type Rs GetQueryExecution = GetQueryExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryExecutionResponse'
            Prelude.<$> (x Prelude..?> "QueryExecution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueryExecution

instance Prelude.NFData GetQueryExecution

instance Prelude.ToHeaders GetQueryExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.GetQueryExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetQueryExecution where
  toJSON GetQueryExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QueryExecutionId" Prelude..= queryExecutionId)
          ]
      )

instance Prelude.ToPath GetQueryExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetQueryExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryExecutionResponse' smart constructor.
data GetQueryExecutionResponse = GetQueryExecutionResponse'
  { -- | Information about the query execution.
    queryExecution :: Prelude.Maybe QueryExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetQueryExecutionResponse
newGetQueryExecutionResponse pHttpStatus_ =
  GetQueryExecutionResponse'
    { queryExecution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the query execution.
getQueryExecutionResponse_queryExecution :: Lens.Lens' GetQueryExecutionResponse (Prelude.Maybe QueryExecution)
getQueryExecutionResponse_queryExecution = Lens.lens (\GetQueryExecutionResponse' {queryExecution} -> queryExecution) (\s@GetQueryExecutionResponse' {} a -> s {queryExecution = a} :: GetQueryExecutionResponse)

-- | The response's http status code.
getQueryExecutionResponse_httpStatus :: Lens.Lens' GetQueryExecutionResponse Prelude.Int
getQueryExecutionResponse_httpStatus = Lens.lens (\GetQueryExecutionResponse' {httpStatus} -> httpStatus) (\s@GetQueryExecutionResponse' {} a -> s {httpStatus = a} :: GetQueryExecutionResponse)

instance Prelude.NFData GetQueryExecutionResponse
