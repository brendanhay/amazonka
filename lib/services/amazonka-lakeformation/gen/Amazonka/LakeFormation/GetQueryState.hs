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
-- Module      : Amazonka.LakeFormation.GetQueryState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of a query previously submitted. Clients are expected
-- to poll @GetQueryState@ to monitor the current state of the planning
-- before retrieving the work units. A query state is only visible to the
-- principal that made the initial call to @StartQueryPlanning@.
module Amazonka.LakeFormation.GetQueryState
  ( -- * Creating a Request
    GetQueryState (..),
    newGetQueryState,

    -- * Request Lenses
    getQueryState_queryId,

    -- * Destructuring the Response
    GetQueryStateResponse (..),
    newGetQueryStateResponse,

    -- * Response Lenses
    getQueryStateResponse_error,
    getQueryStateResponse_httpStatus,
    getQueryStateResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueryState' smart constructor.
data GetQueryState = GetQueryState'
  { -- | The ID of the plan query operation.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'getQueryState_queryId' - The ID of the plan query operation.
newGetQueryState ::
  -- | 'queryId'
  Prelude.Text ->
  GetQueryState
newGetQueryState pQueryId_ =
  GetQueryState' {queryId = pQueryId_}

-- | The ID of the plan query operation.
getQueryState_queryId :: Lens.Lens' GetQueryState Prelude.Text
getQueryState_queryId = Lens.lens (\GetQueryState' {queryId} -> queryId) (\s@GetQueryState' {} a -> s {queryId = a} :: GetQueryState)

instance Core.AWSRequest GetQueryState where
  type
    AWSResponse GetQueryState =
      GetQueryStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryStateResponse'
            Prelude.<$> (x Data..?> "Error")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "State")
      )

instance Prelude.Hashable GetQueryState where
  hashWithSalt _salt GetQueryState' {..} =
    _salt `Prelude.hashWithSalt` queryId

instance Prelude.NFData GetQueryState where
  rnf GetQueryState' {..} = Prelude.rnf queryId

instance Data.ToHeaders GetQueryState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQueryState where
  toJSON GetQueryState' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueryId" Data..= queryId)]
      )

instance Data.ToPath GetQueryState where
  toPath = Prelude.const "/GetQueryState"

instance Data.ToQuery GetQueryState where
  toQuery = Prelude.const Prelude.mempty

-- | A structure for the output.
--
-- /See:/ 'newGetQueryStateResponse' smart constructor.
data GetQueryStateResponse = GetQueryStateResponse'
  { -- | An error message when the operation fails.
    error :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The state of a query previously submitted. The possible states are:
    --
    -- -   PENDING: the query is pending.
    --
    -- -   WORKUNITS_AVAILABLE: some work units are ready for retrieval and
    --     execution.
    --
    -- -   FINISHED: the query planning finished successfully, and all work
    --     units are ready for retrieval and execution.
    --
    -- -   ERROR: an error occurred with the query, such as an invalid query ID
    --     or a backend error.
    state :: QueryStateString
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'getQueryStateResponse_error' - An error message when the operation fails.
--
-- 'httpStatus', 'getQueryStateResponse_httpStatus' - The response's http status code.
--
-- 'state', 'getQueryStateResponse_state' - The state of a query previously submitted. The possible states are:
--
-- -   PENDING: the query is pending.
--
-- -   WORKUNITS_AVAILABLE: some work units are ready for retrieval and
--     execution.
--
-- -   FINISHED: the query planning finished successfully, and all work
--     units are ready for retrieval and execution.
--
-- -   ERROR: an error occurred with the query, such as an invalid query ID
--     or a backend error.
newGetQueryStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'state'
  QueryStateString ->
  GetQueryStateResponse
newGetQueryStateResponse pHttpStatus_ pState_ =
  GetQueryStateResponse'
    { error = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      state = pState_
    }

-- | An error message when the operation fails.
getQueryStateResponse_error :: Lens.Lens' GetQueryStateResponse (Prelude.Maybe Prelude.Text)
getQueryStateResponse_error = Lens.lens (\GetQueryStateResponse' {error} -> error) (\s@GetQueryStateResponse' {} a -> s {error = a} :: GetQueryStateResponse)

-- | The response's http status code.
getQueryStateResponse_httpStatus :: Lens.Lens' GetQueryStateResponse Prelude.Int
getQueryStateResponse_httpStatus = Lens.lens (\GetQueryStateResponse' {httpStatus} -> httpStatus) (\s@GetQueryStateResponse' {} a -> s {httpStatus = a} :: GetQueryStateResponse)

-- | The state of a query previously submitted. The possible states are:
--
-- -   PENDING: the query is pending.
--
-- -   WORKUNITS_AVAILABLE: some work units are ready for retrieval and
--     execution.
--
-- -   FINISHED: the query planning finished successfully, and all work
--     units are ready for retrieval and execution.
--
-- -   ERROR: an error occurred with the query, such as an invalid query ID
--     or a backend error.
getQueryStateResponse_state :: Lens.Lens' GetQueryStateResponse QueryStateString
getQueryStateResponse_state = Lens.lens (\GetQueryStateResponse' {state} -> state) (\s@GetQueryStateResponse' {} a -> s {state = a} :: GetQueryStateResponse)

instance Prelude.NFData GetQueryStateResponse where
  rnf GetQueryStateResponse' {..} =
    Prelude.rnf error `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf state
