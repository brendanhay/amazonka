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
-- Module      : Amazonka.CloudTrail.CancelQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a query if the query is not in a terminated state, such as
-- @CANCELLED@, @FAILED@, @TIMED_OUT@, or @FINISHED@. You must specify an
-- ARN value for @EventDataStore@. The ID of the query that you want to
-- cancel is also required. When you run @CancelQuery@, the query status
-- might show as @CANCELLED@ even if the operation is not yet finished.
module Amazonka.CloudTrail.CancelQuery
  ( -- * Creating a Request
    CancelQuery (..),
    newCancelQuery,

    -- * Request Lenses
    cancelQuery_eventDataStore,
    cancelQuery_queryId,

    -- * Destructuring the Response
    CancelQueryResponse (..),
    newCancelQueryResponse,

    -- * Response Lenses
    cancelQueryResponse_httpStatus,
    cancelQueryResponse_queryId,
    cancelQueryResponse_queryStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelQuery' smart constructor.
data CancelQuery = CancelQuery'
  { -- | The ARN (or the ID suffix of the ARN) of an event data store on which
    -- the specified query is running.
    eventDataStore :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query that you want to cancel. The @QueryId@ comes from
    -- the response of a @StartQuery@ operation.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataStore', 'cancelQuery_eventDataStore' - The ARN (or the ID suffix of the ARN) of an event data store on which
-- the specified query is running.
--
-- 'queryId', 'cancelQuery_queryId' - The ID of the query that you want to cancel. The @QueryId@ comes from
-- the response of a @StartQuery@ operation.
newCancelQuery ::
  -- | 'queryId'
  Prelude.Text ->
  CancelQuery
newCancelQuery pQueryId_ =
  CancelQuery'
    { eventDataStore = Prelude.Nothing,
      queryId = pQueryId_
    }

-- | The ARN (or the ID suffix of the ARN) of an event data store on which
-- the specified query is running.
cancelQuery_eventDataStore :: Lens.Lens' CancelQuery (Prelude.Maybe Prelude.Text)
cancelQuery_eventDataStore = Lens.lens (\CancelQuery' {eventDataStore} -> eventDataStore) (\s@CancelQuery' {} a -> s {eventDataStore = a} :: CancelQuery)

-- | The ID of the query that you want to cancel. The @QueryId@ comes from
-- the response of a @StartQuery@ operation.
cancelQuery_queryId :: Lens.Lens' CancelQuery Prelude.Text
cancelQuery_queryId = Lens.lens (\CancelQuery' {queryId} -> queryId) (\s@CancelQuery' {} a -> s {queryId = a} :: CancelQuery)

instance Core.AWSRequest CancelQuery where
  type AWSResponse CancelQuery = CancelQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "QueryId")
            Prelude.<*> (x Data..:> "QueryStatus")
      )

instance Prelude.Hashable CancelQuery where
  hashWithSalt _salt CancelQuery' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore
      `Prelude.hashWithSalt` queryId

instance Prelude.NFData CancelQuery where
  rnf CancelQuery' {..} =
    Prelude.rnf eventDataStore
      `Prelude.seq` Prelude.rnf queryId

instance Data.ToHeaders CancelQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CancelQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelQuery where
  toJSON CancelQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventDataStore" Data..=)
              Prelude.<$> eventDataStore,
            Prelude.Just ("QueryId" Data..= queryId)
          ]
      )

instance Data.ToPath CancelQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelQueryResponse' smart constructor.
data CancelQueryResponse = CancelQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the canceled query.
    queryId :: Prelude.Text,
    -- | Shows the status of a query after a @CancelQuery@ request. Typically,
    -- the values shown are either @RUNNING@ or @CANCELLED@.
    queryStatus :: QueryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelQueryResponse_httpStatus' - The response's http status code.
--
-- 'queryId', 'cancelQueryResponse_queryId' - The ID of the canceled query.
--
-- 'queryStatus', 'cancelQueryResponse_queryStatus' - Shows the status of a query after a @CancelQuery@ request. Typically,
-- the values shown are either @RUNNING@ or @CANCELLED@.
newCancelQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queryId'
  Prelude.Text ->
  -- | 'queryStatus'
  QueryStatus ->
  CancelQueryResponse
newCancelQueryResponse
  pHttpStatus_
  pQueryId_
  pQueryStatus_ =
    CancelQueryResponse'
      { httpStatus = pHttpStatus_,
        queryId = pQueryId_,
        queryStatus = pQueryStatus_
      }

-- | The response's http status code.
cancelQueryResponse_httpStatus :: Lens.Lens' CancelQueryResponse Prelude.Int
cancelQueryResponse_httpStatus = Lens.lens (\CancelQueryResponse' {httpStatus} -> httpStatus) (\s@CancelQueryResponse' {} a -> s {httpStatus = a} :: CancelQueryResponse)

-- | The ID of the canceled query.
cancelQueryResponse_queryId :: Lens.Lens' CancelQueryResponse Prelude.Text
cancelQueryResponse_queryId = Lens.lens (\CancelQueryResponse' {queryId} -> queryId) (\s@CancelQueryResponse' {} a -> s {queryId = a} :: CancelQueryResponse)

-- | Shows the status of a query after a @CancelQuery@ request. Typically,
-- the values shown are either @RUNNING@ or @CANCELLED@.
cancelQueryResponse_queryStatus :: Lens.Lens' CancelQueryResponse QueryStatus
cancelQueryResponse_queryStatus = Lens.lens (\CancelQueryResponse' {queryStatus} -> queryStatus) (\s@CancelQueryResponse' {} a -> s {queryStatus = a} :: CancelQueryResponse)

instance Prelude.NFData CancelQueryResponse where
  rnf CancelQueryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf queryStatus
