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
-- Module      : Amazonka.CloudTrail.DescribeQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a query, including query run time in
-- milliseconds, number of events scanned and matched, and query status.
-- You must specify an ARN for @EventDataStore@, and a value for @QueryID@.
module Amazonka.CloudTrail.DescribeQuery
  ( -- * Creating a Request
    DescribeQuery (..),
    newDescribeQuery,

    -- * Request Lenses
    describeQuery_eventDataStore,
    describeQuery_queryId,

    -- * Destructuring the Response
    DescribeQueryResponse (..),
    newDescribeQueryResponse,

    -- * Response Lenses
    describeQueryResponse_queryStatistics,
    describeQueryResponse_queryStatus,
    describeQueryResponse_errorMessage,
    describeQueryResponse_queryId,
    describeQueryResponse_deliveryStatus,
    describeQueryResponse_deliveryS3Uri,
    describeQueryResponse_queryString,
    describeQueryResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeQuery' smart constructor.
data DescribeQuery = DescribeQuery'
  { -- | The ARN (or the ID suffix of the ARN) of an event data store on which
    -- the specified query was run.
    eventDataStore :: Prelude.Maybe Prelude.Text,
    -- | The query ID.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataStore', 'describeQuery_eventDataStore' - The ARN (or the ID suffix of the ARN) of an event data store on which
-- the specified query was run.
--
-- 'queryId', 'describeQuery_queryId' - The query ID.
newDescribeQuery ::
  -- | 'queryId'
  Prelude.Text ->
  DescribeQuery
newDescribeQuery pQueryId_ =
  DescribeQuery'
    { eventDataStore = Prelude.Nothing,
      queryId = pQueryId_
    }

-- | The ARN (or the ID suffix of the ARN) of an event data store on which
-- the specified query was run.
describeQuery_eventDataStore :: Lens.Lens' DescribeQuery (Prelude.Maybe Prelude.Text)
describeQuery_eventDataStore = Lens.lens (\DescribeQuery' {eventDataStore} -> eventDataStore) (\s@DescribeQuery' {} a -> s {eventDataStore = a} :: DescribeQuery)

-- | The query ID.
describeQuery_queryId :: Lens.Lens' DescribeQuery Prelude.Text
describeQuery_queryId = Lens.lens (\DescribeQuery' {queryId} -> queryId) (\s@DescribeQuery' {} a -> s {queryId = a} :: DescribeQuery)

instance Core.AWSRequest DescribeQuery where
  type
    AWSResponse DescribeQuery =
      DescribeQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQueryResponse'
            Prelude.<$> (x Core..?> "QueryStatistics")
            Prelude.<*> (x Core..?> "QueryStatus")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "QueryId")
            Prelude.<*> (x Core..?> "DeliveryStatus")
            Prelude.<*> (x Core..?> "DeliveryS3Uri")
            Prelude.<*> (x Core..?> "QueryString")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeQuery where
  hashWithSalt _salt DescribeQuery' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore
      `Prelude.hashWithSalt` queryId

instance Prelude.NFData DescribeQuery where
  rnf DescribeQuery' {..} =
    Prelude.rnf eventDataStore
      `Prelude.seq` Prelude.rnf queryId

instance Core.ToHeaders DescribeQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DescribeQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeQuery where
  toJSON DescribeQuery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EventDataStore" Core..=)
              Prelude.<$> eventDataStore,
            Prelude.Just ("QueryId" Core..= queryId)
          ]
      )

instance Core.ToPath DescribeQuery where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQueryResponse' smart constructor.
data DescribeQueryResponse = DescribeQueryResponse'
  { -- | Metadata about a query, including the number of events that were
    -- matched, the total number of events scanned, the query run time in
    -- milliseconds, and the query\'s creation time.
    queryStatistics :: Prelude.Maybe QueryStatisticsForDescribeQuery,
    -- | The status of a query. Values for @QueryStatus@ include @QUEUED@,
    -- @RUNNING@, @FINISHED@, @FAILED@, @TIMED_OUT@, or @CANCELLED@
    queryStatus :: Prelude.Maybe QueryStatus,
    -- | The error message returned if a query failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query.
    queryId :: Prelude.Maybe Prelude.Text,
    -- | The delivery status.
    deliveryStatus :: Prelude.Maybe DeliveryStatus,
    -- | The URI for the S3 bucket where CloudTrail delivered query results, if
    -- applicable.
    deliveryS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The SQL code of a query.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStatistics', 'describeQueryResponse_queryStatistics' - Metadata about a query, including the number of events that were
-- matched, the total number of events scanned, the query run time in
-- milliseconds, and the query\'s creation time.
--
-- 'queryStatus', 'describeQueryResponse_queryStatus' - The status of a query. Values for @QueryStatus@ include @QUEUED@,
-- @RUNNING@, @FINISHED@, @FAILED@, @TIMED_OUT@, or @CANCELLED@
--
-- 'errorMessage', 'describeQueryResponse_errorMessage' - The error message returned if a query failed.
--
-- 'queryId', 'describeQueryResponse_queryId' - The ID of the query.
--
-- 'deliveryStatus', 'describeQueryResponse_deliveryStatus' - The delivery status.
--
-- 'deliveryS3Uri', 'describeQueryResponse_deliveryS3Uri' - The URI for the S3 bucket where CloudTrail delivered query results, if
-- applicable.
--
-- 'queryString', 'describeQueryResponse_queryString' - The SQL code of a query.
--
-- 'httpStatus', 'describeQueryResponse_httpStatus' - The response's http status code.
newDescribeQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQueryResponse
newDescribeQueryResponse pHttpStatus_ =
  DescribeQueryResponse'
    { queryStatistics =
        Prelude.Nothing,
      queryStatus = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      queryId = Prelude.Nothing,
      deliveryStatus = Prelude.Nothing,
      deliveryS3Uri = Prelude.Nothing,
      queryString = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata about a query, including the number of events that were
-- matched, the total number of events scanned, the query run time in
-- milliseconds, and the query\'s creation time.
describeQueryResponse_queryStatistics :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe QueryStatisticsForDescribeQuery)
describeQueryResponse_queryStatistics = Lens.lens (\DescribeQueryResponse' {queryStatistics} -> queryStatistics) (\s@DescribeQueryResponse' {} a -> s {queryStatistics = a} :: DescribeQueryResponse)

-- | The status of a query. Values for @QueryStatus@ include @QUEUED@,
-- @RUNNING@, @FINISHED@, @FAILED@, @TIMED_OUT@, or @CANCELLED@
describeQueryResponse_queryStatus :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe QueryStatus)
describeQueryResponse_queryStatus = Lens.lens (\DescribeQueryResponse' {queryStatus} -> queryStatus) (\s@DescribeQueryResponse' {} a -> s {queryStatus = a} :: DescribeQueryResponse)

-- | The error message returned if a query failed.
describeQueryResponse_errorMessage :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe Prelude.Text)
describeQueryResponse_errorMessage = Lens.lens (\DescribeQueryResponse' {errorMessage} -> errorMessage) (\s@DescribeQueryResponse' {} a -> s {errorMessage = a} :: DescribeQueryResponse)

-- | The ID of the query.
describeQueryResponse_queryId :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe Prelude.Text)
describeQueryResponse_queryId = Lens.lens (\DescribeQueryResponse' {queryId} -> queryId) (\s@DescribeQueryResponse' {} a -> s {queryId = a} :: DescribeQueryResponse)

-- | The delivery status.
describeQueryResponse_deliveryStatus :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe DeliveryStatus)
describeQueryResponse_deliveryStatus = Lens.lens (\DescribeQueryResponse' {deliveryStatus} -> deliveryStatus) (\s@DescribeQueryResponse' {} a -> s {deliveryStatus = a} :: DescribeQueryResponse)

-- | The URI for the S3 bucket where CloudTrail delivered query results, if
-- applicable.
describeQueryResponse_deliveryS3Uri :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe Prelude.Text)
describeQueryResponse_deliveryS3Uri = Lens.lens (\DescribeQueryResponse' {deliveryS3Uri} -> deliveryS3Uri) (\s@DescribeQueryResponse' {} a -> s {deliveryS3Uri = a} :: DescribeQueryResponse)

-- | The SQL code of a query.
describeQueryResponse_queryString :: Lens.Lens' DescribeQueryResponse (Prelude.Maybe Prelude.Text)
describeQueryResponse_queryString = Lens.lens (\DescribeQueryResponse' {queryString} -> queryString) (\s@DescribeQueryResponse' {} a -> s {queryString = a} :: DescribeQueryResponse)

-- | The response's http status code.
describeQueryResponse_httpStatus :: Lens.Lens' DescribeQueryResponse Prelude.Int
describeQueryResponse_httpStatus = Lens.lens (\DescribeQueryResponse' {httpStatus} -> httpStatus) (\s@DescribeQueryResponse' {} a -> s {httpStatus = a} :: DescribeQueryResponse)

instance Prelude.NFData DescribeQueryResponse where
  rnf DescribeQueryResponse' {..} =
    Prelude.rnf queryStatistics
      `Prelude.seq` Prelude.rnf queryStatus
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf deliveryStatus
      `Prelude.seq` Prelude.rnf deliveryS3Uri
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf httpStatus
