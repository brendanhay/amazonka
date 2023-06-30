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
-- Module      : Amazonka.MarketplaceCatalog.DescribeChangeSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a given change set.
module Amazonka.MarketplaceCatalog.DescribeChangeSet
  ( -- * Creating a Request
    DescribeChangeSet (..),
    newDescribeChangeSet,

    -- * Request Lenses
    describeChangeSet_catalog,
    describeChangeSet_changeSetId,

    -- * Destructuring the Response
    DescribeChangeSetResponse (..),
    newDescribeChangeSetResponse,

    -- * Response Lenses
    describeChangeSetResponse_changeSet,
    describeChangeSetResponse_changeSetArn,
    describeChangeSetResponse_changeSetId,
    describeChangeSetResponse_changeSetName,
    describeChangeSetResponse_endTime,
    describeChangeSetResponse_failureCode,
    describeChangeSetResponse_failureDescription,
    describeChangeSetResponse_startTime,
    describeChangeSetResponse_status,
    describeChangeSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChangeSet' smart constructor.
data DescribeChangeSet = DescribeChangeSet'
  { -- | Required. The catalog related to the request. Fixed value:
    -- @AWSMarketplace@
    catalog :: Prelude.Text,
    -- | Required. The unique identifier for the @StartChangeSet@ request that
    -- you want to describe the details for.
    changeSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalog', 'describeChangeSet_catalog' - Required. The catalog related to the request. Fixed value:
-- @AWSMarketplace@
--
-- 'changeSetId', 'describeChangeSet_changeSetId' - Required. The unique identifier for the @StartChangeSet@ request that
-- you want to describe the details for.
newDescribeChangeSet ::
  -- | 'catalog'
  Prelude.Text ->
  -- | 'changeSetId'
  Prelude.Text ->
  DescribeChangeSet
newDescribeChangeSet pCatalog_ pChangeSetId_ =
  DescribeChangeSet'
    { catalog = pCatalog_,
      changeSetId = pChangeSetId_
    }

-- | Required. The catalog related to the request. Fixed value:
-- @AWSMarketplace@
describeChangeSet_catalog :: Lens.Lens' DescribeChangeSet Prelude.Text
describeChangeSet_catalog = Lens.lens (\DescribeChangeSet' {catalog} -> catalog) (\s@DescribeChangeSet' {} a -> s {catalog = a} :: DescribeChangeSet)

-- | Required. The unique identifier for the @StartChangeSet@ request that
-- you want to describe the details for.
describeChangeSet_changeSetId :: Lens.Lens' DescribeChangeSet Prelude.Text
describeChangeSet_changeSetId = Lens.lens (\DescribeChangeSet' {changeSetId} -> changeSetId) (\s@DescribeChangeSet' {} a -> s {changeSetId = a} :: DescribeChangeSet)

instance Core.AWSRequest DescribeChangeSet where
  type
    AWSResponse DescribeChangeSet =
      DescribeChangeSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChangeSetResponse'
            Prelude.<$> (x Data..?> "ChangeSet" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ChangeSetArn")
            Prelude.<*> (x Data..?> "ChangeSetId")
            Prelude.<*> (x Data..?> "ChangeSetName")
            Prelude.<*> (x Data..?> "EndTime")
            Prelude.<*> (x Data..?> "FailureCode")
            Prelude.<*> (x Data..?> "FailureDescription")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChangeSet where
  hashWithSalt _salt DescribeChangeSet' {..} =
    _salt
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` changeSetId

instance Prelude.NFData DescribeChangeSet where
  rnf DescribeChangeSet' {..} =
    Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf changeSetId

instance Data.ToHeaders DescribeChangeSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeChangeSet where
  toPath = Prelude.const "/DescribeChangeSet"

instance Data.ToQuery DescribeChangeSet where
  toQuery DescribeChangeSet' {..} =
    Prelude.mconcat
      [ "catalog" Data.=: catalog,
        "changeSetId" Data.=: changeSetId
      ]

-- | /See:/ 'newDescribeChangeSetResponse' smart constructor.
data DescribeChangeSetResponse = DescribeChangeSetResponse'
  { -- | An array of @ChangeSummary@ objects.
    changeSet :: Prelude.Maybe [ChangeSummary],
    -- | The ARN associated with the unique identifier for the change set
    -- referenced in this request.
    changeSetArn :: Prelude.Maybe Prelude.Text,
    -- | Required. The unique identifier for the change set referenced in this
    -- request.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The optional name provided in the @StartChangeSet@ request. If you do
    -- not provide a name, one is set by default.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format (2018-02-27T13:45:22Z), the
    -- request transitioned to a terminal state. The change cannot transition
    -- to a different state. Null if the request is not in a terminal state.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | Returned if the change set is in @FAILED@ status. Can be either
    -- @CLIENT_ERROR@, which means that there are issues with the request (see
    -- the @ErrorDetailList@), or @SERVER_FAULT@, which means that there is a
    -- problem in the system, and you should retry your request.
    failureCode :: Prelude.Maybe FailureCode,
    -- | Returned if there is a failure on the change set, but that failure is
    -- not related to any of the changes in the request.
    failureDescription :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format (2018-02-27T13:45:22Z), the
    -- request started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the change request.
    status :: Prelude.Maybe ChangeStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSet', 'describeChangeSetResponse_changeSet' - An array of @ChangeSummary@ objects.
--
-- 'changeSetArn', 'describeChangeSetResponse_changeSetArn' - The ARN associated with the unique identifier for the change set
-- referenced in this request.
--
-- 'changeSetId', 'describeChangeSetResponse_changeSetId' - Required. The unique identifier for the change set referenced in this
-- request.
--
-- 'changeSetName', 'describeChangeSetResponse_changeSetName' - The optional name provided in the @StartChangeSet@ request. If you do
-- not provide a name, one is set by default.
--
-- 'endTime', 'describeChangeSetResponse_endTime' - The date and time, in ISO 8601 format (2018-02-27T13:45:22Z), the
-- request transitioned to a terminal state. The change cannot transition
-- to a different state. Null if the request is not in a terminal state.
--
-- 'failureCode', 'describeChangeSetResponse_failureCode' - Returned if the change set is in @FAILED@ status. Can be either
-- @CLIENT_ERROR@, which means that there are issues with the request (see
-- the @ErrorDetailList@), or @SERVER_FAULT@, which means that there is a
-- problem in the system, and you should retry your request.
--
-- 'failureDescription', 'describeChangeSetResponse_failureDescription' - Returned if there is a failure on the change set, but that failure is
-- not related to any of the changes in the request.
--
-- 'startTime', 'describeChangeSetResponse_startTime' - The date and time, in ISO 8601 format (2018-02-27T13:45:22Z), the
-- request started.
--
-- 'status', 'describeChangeSetResponse_status' - The status of the change request.
--
-- 'httpStatus', 'describeChangeSetResponse_httpStatus' - The response's http status code.
newDescribeChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChangeSetResponse
newDescribeChangeSetResponse pHttpStatus_ =
  DescribeChangeSetResponse'
    { changeSet =
        Prelude.Nothing,
      changeSetArn = Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      changeSetName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureDescription = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ChangeSummary@ objects.
describeChangeSetResponse_changeSet :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe [ChangeSummary])
describeChangeSetResponse_changeSet = Lens.lens (\DescribeChangeSetResponse' {changeSet} -> changeSet) (\s@DescribeChangeSetResponse' {} a -> s {changeSet = a} :: DescribeChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN associated with the unique identifier for the change set
-- referenced in this request.
describeChangeSetResponse_changeSetArn :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetArn = Lens.lens (\DescribeChangeSetResponse' {changeSetArn} -> changeSetArn) (\s@DescribeChangeSetResponse' {} a -> s {changeSetArn = a} :: DescribeChangeSetResponse)

-- | Required. The unique identifier for the change set referenced in this
-- request.
describeChangeSetResponse_changeSetId :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetId = Lens.lens (\DescribeChangeSetResponse' {changeSetId} -> changeSetId) (\s@DescribeChangeSetResponse' {} a -> s {changeSetId = a} :: DescribeChangeSetResponse)

-- | The optional name provided in the @StartChangeSet@ request. If you do
-- not provide a name, one is set by default.
describeChangeSetResponse_changeSetName :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_changeSetName = Lens.lens (\DescribeChangeSetResponse' {changeSetName} -> changeSetName) (\s@DescribeChangeSetResponse' {} a -> s {changeSetName = a} :: DescribeChangeSetResponse)

-- | The date and time, in ISO 8601 format (2018-02-27T13:45:22Z), the
-- request transitioned to a terminal state. The change cannot transition
-- to a different state. Null if the request is not in a terminal state.
describeChangeSetResponse_endTime :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_endTime = Lens.lens (\DescribeChangeSetResponse' {endTime} -> endTime) (\s@DescribeChangeSetResponse' {} a -> s {endTime = a} :: DescribeChangeSetResponse)

-- | Returned if the change set is in @FAILED@ status. Can be either
-- @CLIENT_ERROR@, which means that there are issues with the request (see
-- the @ErrorDetailList@), or @SERVER_FAULT@, which means that there is a
-- problem in the system, and you should retry your request.
describeChangeSetResponse_failureCode :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe FailureCode)
describeChangeSetResponse_failureCode = Lens.lens (\DescribeChangeSetResponse' {failureCode} -> failureCode) (\s@DescribeChangeSetResponse' {} a -> s {failureCode = a} :: DescribeChangeSetResponse)

-- | Returned if there is a failure on the change set, but that failure is
-- not related to any of the changes in the request.
describeChangeSetResponse_failureDescription :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_failureDescription = Lens.lens (\DescribeChangeSetResponse' {failureDescription} -> failureDescription) (\s@DescribeChangeSetResponse' {} a -> s {failureDescription = a} :: DescribeChangeSetResponse)

-- | The date and time, in ISO 8601 format (2018-02-27T13:45:22Z), the
-- request started.
describeChangeSetResponse_startTime :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe Prelude.Text)
describeChangeSetResponse_startTime = Lens.lens (\DescribeChangeSetResponse' {startTime} -> startTime) (\s@DescribeChangeSetResponse' {} a -> s {startTime = a} :: DescribeChangeSetResponse)

-- | The status of the change request.
describeChangeSetResponse_status :: Lens.Lens' DescribeChangeSetResponse (Prelude.Maybe ChangeStatus)
describeChangeSetResponse_status = Lens.lens (\DescribeChangeSetResponse' {status} -> status) (\s@DescribeChangeSetResponse' {} a -> s {status = a} :: DescribeChangeSetResponse)

-- | The response's http status code.
describeChangeSetResponse_httpStatus :: Lens.Lens' DescribeChangeSetResponse Prelude.Int
describeChangeSetResponse_httpStatus = Lens.lens (\DescribeChangeSetResponse' {httpStatus} -> httpStatus) (\s@DescribeChangeSetResponse' {} a -> s {httpStatus = a} :: DescribeChangeSetResponse)

instance Prelude.NFData DescribeChangeSetResponse where
  rnf DescribeChangeSetResponse' {..} =
    Prelude.rnf changeSet
      `Prelude.seq` Prelude.rnf changeSetArn
      `Prelude.seq` Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf changeSetName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureDescription
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
