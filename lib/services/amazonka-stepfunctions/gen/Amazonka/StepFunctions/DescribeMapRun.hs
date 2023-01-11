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
-- Module      : Amazonka.StepFunctions.DescribeMapRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a Map Run\'s configuration, progress, and
-- results. For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-examine-map-run.html Examining Map Run>
-- in the /Step Functions Developer Guide/.
module Amazonka.StepFunctions.DescribeMapRun
  ( -- * Creating a Request
    DescribeMapRun (..),
    newDescribeMapRun,

    -- * Request Lenses
    describeMapRun_mapRunArn,

    -- * Destructuring the Response
    DescribeMapRunResponse (..),
    newDescribeMapRunResponse,

    -- * Response Lenses
    describeMapRunResponse_stopDate,
    describeMapRunResponse_httpStatus,
    describeMapRunResponse_mapRunArn,
    describeMapRunResponse_executionArn,
    describeMapRunResponse_status,
    describeMapRunResponse_startDate,
    describeMapRunResponse_maxConcurrency,
    describeMapRunResponse_toleratedFailurePercentage,
    describeMapRunResponse_toleratedFailureCount,
    describeMapRunResponse_itemCounts,
    describeMapRunResponse_executionCounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDescribeMapRun' smart constructor.
data DescribeMapRun = DescribeMapRun'
  { -- | The Amazon Resource Name (ARN) that identifies a Map Run.
    mapRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMapRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapRunArn', 'describeMapRun_mapRunArn' - The Amazon Resource Name (ARN) that identifies a Map Run.
newDescribeMapRun ::
  -- | 'mapRunArn'
  Prelude.Text ->
  DescribeMapRun
newDescribeMapRun pMapRunArn_ =
  DescribeMapRun' {mapRunArn = pMapRunArn_}

-- | The Amazon Resource Name (ARN) that identifies a Map Run.
describeMapRun_mapRunArn :: Lens.Lens' DescribeMapRun Prelude.Text
describeMapRun_mapRunArn = Lens.lens (\DescribeMapRun' {mapRunArn} -> mapRunArn) (\s@DescribeMapRun' {} a -> s {mapRunArn = a} :: DescribeMapRun)

instance Core.AWSRequest DescribeMapRun where
  type
    AWSResponse DescribeMapRun =
      DescribeMapRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMapRunResponse'
            Prelude.<$> (x Data..?> "stopDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "mapRunArn")
            Prelude.<*> (x Data..:> "executionArn")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "startDate")
            Prelude.<*> (x Data..:> "maxConcurrency")
            Prelude.<*> (x Data..:> "toleratedFailurePercentage")
            Prelude.<*> (x Data..:> "toleratedFailureCount")
            Prelude.<*> (x Data..:> "itemCounts")
            Prelude.<*> (x Data..:> "executionCounts")
      )

instance Prelude.Hashable DescribeMapRun where
  hashWithSalt _salt DescribeMapRun' {..} =
    _salt `Prelude.hashWithSalt` mapRunArn

instance Prelude.NFData DescribeMapRun where
  rnf DescribeMapRun' {..} = Prelude.rnf mapRunArn

instance Data.ToHeaders DescribeMapRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DescribeMapRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMapRun where
  toJSON DescribeMapRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("mapRunArn" Data..= mapRunArn)]
      )

instance Data.ToPath DescribeMapRun where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMapRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMapRunResponse' smart constructor.
data DescribeMapRunResponse = DescribeMapRunResponse'
  { -- | The date when the Map Run was stopped.
    stopDate :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies a Map Run.
    mapRunArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the execution in which
    -- the Map Run was started.
    executionArn :: Prelude.Text,
    -- | The current status of the Map Run.
    status :: MapRunStatus,
    -- | The date when the Map Run was started.
    startDate :: Data.POSIX,
    -- | The maximum number of child workflow executions configured to run in
    -- parallel for the Map Run at the same time.
    maxConcurrency :: Prelude.Natural,
    -- | The maximum percentage of failed child workflow executions before the
    -- Map Run fails.
    toleratedFailurePercentage :: Prelude.Double,
    -- | The maximum number of failed child workflow executions before the Map
    -- Run fails.
    toleratedFailureCount :: Prelude.Natural,
    -- | A JSON object that contains information about the total number of items,
    -- and the item count for each processing status, such as @pending@ and
    -- @failed@.
    itemCounts :: MapRunItemCounts,
    -- | A JSON object that contains information about the total number of child
    -- workflow executions for the Map Run, and the count of child workflow
    -- executions for each status, such as @failed@ and @succeeded@.
    executionCounts :: MapRunExecutionCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMapRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopDate', 'describeMapRunResponse_stopDate' - The date when the Map Run was stopped.
--
-- 'httpStatus', 'describeMapRunResponse_httpStatus' - The response's http status code.
--
-- 'mapRunArn', 'describeMapRunResponse_mapRunArn' - The Amazon Resource Name (ARN) that identifies a Map Run.
--
-- 'executionArn', 'describeMapRunResponse_executionArn' - The Amazon Resource Name (ARN) that identifies the execution in which
-- the Map Run was started.
--
-- 'status', 'describeMapRunResponse_status' - The current status of the Map Run.
--
-- 'startDate', 'describeMapRunResponse_startDate' - The date when the Map Run was started.
--
-- 'maxConcurrency', 'describeMapRunResponse_maxConcurrency' - The maximum number of child workflow executions configured to run in
-- parallel for the Map Run at the same time.
--
-- 'toleratedFailurePercentage', 'describeMapRunResponse_toleratedFailurePercentage' - The maximum percentage of failed child workflow executions before the
-- Map Run fails.
--
-- 'toleratedFailureCount', 'describeMapRunResponse_toleratedFailureCount' - The maximum number of failed child workflow executions before the Map
-- Run fails.
--
-- 'itemCounts', 'describeMapRunResponse_itemCounts' - A JSON object that contains information about the total number of items,
-- and the item count for each processing status, such as @pending@ and
-- @failed@.
--
-- 'executionCounts', 'describeMapRunResponse_executionCounts' - A JSON object that contains information about the total number of child
-- workflow executions for the Map Run, and the count of child workflow
-- executions for each status, such as @failed@ and @succeeded@.
newDescribeMapRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mapRunArn'
  Prelude.Text ->
  -- | 'executionArn'
  Prelude.Text ->
  -- | 'status'
  MapRunStatus ->
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'maxConcurrency'
  Prelude.Natural ->
  -- | 'toleratedFailurePercentage'
  Prelude.Double ->
  -- | 'toleratedFailureCount'
  Prelude.Natural ->
  -- | 'itemCounts'
  MapRunItemCounts ->
  -- | 'executionCounts'
  MapRunExecutionCounts ->
  DescribeMapRunResponse
newDescribeMapRunResponse
  pHttpStatus_
  pMapRunArn_
  pExecutionArn_
  pStatus_
  pStartDate_
  pMaxConcurrency_
  pToleratedFailurePercentage_
  pToleratedFailureCount_
  pItemCounts_
  pExecutionCounts_ =
    DescribeMapRunResponse'
      { stopDate = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        mapRunArn = pMapRunArn_,
        executionArn = pExecutionArn_,
        status = pStatus_,
        startDate = Data._Time Lens.# pStartDate_,
        maxConcurrency = pMaxConcurrency_,
        toleratedFailurePercentage =
          pToleratedFailurePercentage_,
        toleratedFailureCount = pToleratedFailureCount_,
        itemCounts = pItemCounts_,
        executionCounts = pExecutionCounts_
      }

-- | The date when the Map Run was stopped.
describeMapRunResponse_stopDate :: Lens.Lens' DescribeMapRunResponse (Prelude.Maybe Prelude.UTCTime)
describeMapRunResponse_stopDate = Lens.lens (\DescribeMapRunResponse' {stopDate} -> stopDate) (\s@DescribeMapRunResponse' {} a -> s {stopDate = a} :: DescribeMapRunResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeMapRunResponse_httpStatus :: Lens.Lens' DescribeMapRunResponse Prelude.Int
describeMapRunResponse_httpStatus = Lens.lens (\DescribeMapRunResponse' {httpStatus} -> httpStatus) (\s@DescribeMapRunResponse' {} a -> s {httpStatus = a} :: DescribeMapRunResponse)

-- | The Amazon Resource Name (ARN) that identifies a Map Run.
describeMapRunResponse_mapRunArn :: Lens.Lens' DescribeMapRunResponse Prelude.Text
describeMapRunResponse_mapRunArn = Lens.lens (\DescribeMapRunResponse' {mapRunArn} -> mapRunArn) (\s@DescribeMapRunResponse' {} a -> s {mapRunArn = a} :: DescribeMapRunResponse)

-- | The Amazon Resource Name (ARN) that identifies the execution in which
-- the Map Run was started.
describeMapRunResponse_executionArn :: Lens.Lens' DescribeMapRunResponse Prelude.Text
describeMapRunResponse_executionArn = Lens.lens (\DescribeMapRunResponse' {executionArn} -> executionArn) (\s@DescribeMapRunResponse' {} a -> s {executionArn = a} :: DescribeMapRunResponse)

-- | The current status of the Map Run.
describeMapRunResponse_status :: Lens.Lens' DescribeMapRunResponse MapRunStatus
describeMapRunResponse_status = Lens.lens (\DescribeMapRunResponse' {status} -> status) (\s@DescribeMapRunResponse' {} a -> s {status = a} :: DescribeMapRunResponse)

-- | The date when the Map Run was started.
describeMapRunResponse_startDate :: Lens.Lens' DescribeMapRunResponse Prelude.UTCTime
describeMapRunResponse_startDate = Lens.lens (\DescribeMapRunResponse' {startDate} -> startDate) (\s@DescribeMapRunResponse' {} a -> s {startDate = a} :: DescribeMapRunResponse) Prelude.. Data._Time

-- | The maximum number of child workflow executions configured to run in
-- parallel for the Map Run at the same time.
describeMapRunResponse_maxConcurrency :: Lens.Lens' DescribeMapRunResponse Prelude.Natural
describeMapRunResponse_maxConcurrency = Lens.lens (\DescribeMapRunResponse' {maxConcurrency} -> maxConcurrency) (\s@DescribeMapRunResponse' {} a -> s {maxConcurrency = a} :: DescribeMapRunResponse)

-- | The maximum percentage of failed child workflow executions before the
-- Map Run fails.
describeMapRunResponse_toleratedFailurePercentage :: Lens.Lens' DescribeMapRunResponse Prelude.Double
describeMapRunResponse_toleratedFailurePercentage = Lens.lens (\DescribeMapRunResponse' {toleratedFailurePercentage} -> toleratedFailurePercentage) (\s@DescribeMapRunResponse' {} a -> s {toleratedFailurePercentage = a} :: DescribeMapRunResponse)

-- | The maximum number of failed child workflow executions before the Map
-- Run fails.
describeMapRunResponse_toleratedFailureCount :: Lens.Lens' DescribeMapRunResponse Prelude.Natural
describeMapRunResponse_toleratedFailureCount = Lens.lens (\DescribeMapRunResponse' {toleratedFailureCount} -> toleratedFailureCount) (\s@DescribeMapRunResponse' {} a -> s {toleratedFailureCount = a} :: DescribeMapRunResponse)

-- | A JSON object that contains information about the total number of items,
-- and the item count for each processing status, such as @pending@ and
-- @failed@.
describeMapRunResponse_itemCounts :: Lens.Lens' DescribeMapRunResponse MapRunItemCounts
describeMapRunResponse_itemCounts = Lens.lens (\DescribeMapRunResponse' {itemCounts} -> itemCounts) (\s@DescribeMapRunResponse' {} a -> s {itemCounts = a} :: DescribeMapRunResponse)

-- | A JSON object that contains information about the total number of child
-- workflow executions for the Map Run, and the count of child workflow
-- executions for each status, such as @failed@ and @succeeded@.
describeMapRunResponse_executionCounts :: Lens.Lens' DescribeMapRunResponse MapRunExecutionCounts
describeMapRunResponse_executionCounts = Lens.lens (\DescribeMapRunResponse' {executionCounts} -> executionCounts) (\s@DescribeMapRunResponse' {} a -> s {executionCounts = a} :: DescribeMapRunResponse)

instance Prelude.NFData DescribeMapRunResponse where
  rnf DescribeMapRunResponse' {..} =
    Prelude.rnf stopDate
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mapRunArn
      `Prelude.seq` Prelude.rnf executionArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf toleratedFailurePercentage
      `Prelude.seq` Prelude.rnf toleratedFailureCount
      `Prelude.seq` Prelude.rnf itemCounts
      `Prelude.seq` Prelude.rnf executionCounts
