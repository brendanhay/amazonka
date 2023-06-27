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
-- Module      : Amazonka.LexV2Models.CreateTestSetDiscrepancyReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a report that describes the differences between the bot and the
-- test set.
module Amazonka.LexV2Models.CreateTestSetDiscrepancyReport
  ( -- * Creating a Request
    CreateTestSetDiscrepancyReport (..),
    newCreateTestSetDiscrepancyReport,

    -- * Request Lenses
    createTestSetDiscrepancyReport_testSetId,
    createTestSetDiscrepancyReport_target,

    -- * Destructuring the Response
    CreateTestSetDiscrepancyReportResponse (..),
    newCreateTestSetDiscrepancyReportResponse,

    -- * Response Lenses
    createTestSetDiscrepancyReportResponse_creationDateTime,
    createTestSetDiscrepancyReportResponse_target,
    createTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId,
    createTestSetDiscrepancyReportResponse_testSetId,
    createTestSetDiscrepancyReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTestSetDiscrepancyReport' smart constructor.
data CreateTestSetDiscrepancyReport = CreateTestSetDiscrepancyReport'
  { -- | The test set Id for the test set discrepancy report.
    testSetId :: Prelude.Text,
    -- | The target bot for the test set discrepancy report.
    target :: TestSetDiscrepancyReportResourceTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTestSetDiscrepancyReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testSetId', 'createTestSetDiscrepancyReport_testSetId' - The test set Id for the test set discrepancy report.
--
-- 'target', 'createTestSetDiscrepancyReport_target' - The target bot for the test set discrepancy report.
newCreateTestSetDiscrepancyReport ::
  -- | 'testSetId'
  Prelude.Text ->
  -- | 'target'
  TestSetDiscrepancyReportResourceTarget ->
  CreateTestSetDiscrepancyReport
newCreateTestSetDiscrepancyReport
  pTestSetId_
  pTarget_ =
    CreateTestSetDiscrepancyReport'
      { testSetId =
          pTestSetId_,
        target = pTarget_
      }

-- | The test set Id for the test set discrepancy report.
createTestSetDiscrepancyReport_testSetId :: Lens.Lens' CreateTestSetDiscrepancyReport Prelude.Text
createTestSetDiscrepancyReport_testSetId = Lens.lens (\CreateTestSetDiscrepancyReport' {testSetId} -> testSetId) (\s@CreateTestSetDiscrepancyReport' {} a -> s {testSetId = a} :: CreateTestSetDiscrepancyReport)

-- | The target bot for the test set discrepancy report.
createTestSetDiscrepancyReport_target :: Lens.Lens' CreateTestSetDiscrepancyReport TestSetDiscrepancyReportResourceTarget
createTestSetDiscrepancyReport_target = Lens.lens (\CreateTestSetDiscrepancyReport' {target} -> target) (\s@CreateTestSetDiscrepancyReport' {} a -> s {target = a} :: CreateTestSetDiscrepancyReport)

instance
  Core.AWSRequest
    CreateTestSetDiscrepancyReport
  where
  type
    AWSResponse CreateTestSetDiscrepancyReport =
      CreateTestSetDiscrepancyReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTestSetDiscrepancyReportResponse'
            Prelude.<$> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "target")
            Prelude.<*> (x Data..?> "testSetDiscrepancyReportId")
            Prelude.<*> (x Data..?> "testSetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTestSetDiscrepancyReport
  where
  hashWithSalt
    _salt
    CreateTestSetDiscrepancyReport' {..} =
      _salt
        `Prelude.hashWithSalt` testSetId
        `Prelude.hashWithSalt` target

instance
  Prelude.NFData
    CreateTestSetDiscrepancyReport
  where
  rnf CreateTestSetDiscrepancyReport' {..} =
    Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf target

instance
  Data.ToHeaders
    CreateTestSetDiscrepancyReport
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTestSetDiscrepancyReport where
  toJSON CreateTestSetDiscrepancyReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("target" Data..= target)]
      )

instance Data.ToPath CreateTestSetDiscrepancyReport where
  toPath CreateTestSetDiscrepancyReport' {..} =
    Prelude.mconcat
      [ "/testsets/",
        Data.toBS testSetId,
        "/testsetdiscrepancy"
      ]

instance Data.ToQuery CreateTestSetDiscrepancyReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTestSetDiscrepancyReportResponse' smart constructor.
data CreateTestSetDiscrepancyReportResponse = CreateTestSetDiscrepancyReportResponse'
  { -- | The creation date and time for the test set discrepancy report.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The target bot for the test set discrepancy report.
    target :: Prelude.Maybe TestSetDiscrepancyReportResourceTarget,
    -- | The unique identifier of the test set discrepancy report to describe.
    testSetDiscrepancyReportId :: Prelude.Maybe Prelude.Text,
    -- | The test set Id for the test set discrepancy report.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTestSetDiscrepancyReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'createTestSetDiscrepancyReportResponse_creationDateTime' - The creation date and time for the test set discrepancy report.
--
-- 'target', 'createTestSetDiscrepancyReportResponse_target' - The target bot for the test set discrepancy report.
--
-- 'testSetDiscrepancyReportId', 'createTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId' - The unique identifier of the test set discrepancy report to describe.
--
-- 'testSetId', 'createTestSetDiscrepancyReportResponse_testSetId' - The test set Id for the test set discrepancy report.
--
-- 'httpStatus', 'createTestSetDiscrepancyReportResponse_httpStatus' - The response's http status code.
newCreateTestSetDiscrepancyReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTestSetDiscrepancyReportResponse
newCreateTestSetDiscrepancyReportResponse
  pHttpStatus_ =
    CreateTestSetDiscrepancyReportResponse'
      { creationDateTime =
          Prelude.Nothing,
        target = Prelude.Nothing,
        testSetDiscrepancyReportId =
          Prelude.Nothing,
        testSetId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The creation date and time for the test set discrepancy report.
createTestSetDiscrepancyReportResponse_creationDateTime :: Lens.Lens' CreateTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.UTCTime)
createTestSetDiscrepancyReportResponse_creationDateTime = Lens.lens (\CreateTestSetDiscrepancyReportResponse' {creationDateTime} -> creationDateTime) (\s@CreateTestSetDiscrepancyReportResponse' {} a -> s {creationDateTime = a} :: CreateTestSetDiscrepancyReportResponse) Prelude.. Lens.mapping Data._Time

-- | The target bot for the test set discrepancy report.
createTestSetDiscrepancyReportResponse_target :: Lens.Lens' CreateTestSetDiscrepancyReportResponse (Prelude.Maybe TestSetDiscrepancyReportResourceTarget)
createTestSetDiscrepancyReportResponse_target = Lens.lens (\CreateTestSetDiscrepancyReportResponse' {target} -> target) (\s@CreateTestSetDiscrepancyReportResponse' {} a -> s {target = a} :: CreateTestSetDiscrepancyReportResponse)

-- | The unique identifier of the test set discrepancy report to describe.
createTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId :: Lens.Lens' CreateTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.Text)
createTestSetDiscrepancyReportResponse_testSetDiscrepancyReportId = Lens.lens (\CreateTestSetDiscrepancyReportResponse' {testSetDiscrepancyReportId} -> testSetDiscrepancyReportId) (\s@CreateTestSetDiscrepancyReportResponse' {} a -> s {testSetDiscrepancyReportId = a} :: CreateTestSetDiscrepancyReportResponse)

-- | The test set Id for the test set discrepancy report.
createTestSetDiscrepancyReportResponse_testSetId :: Lens.Lens' CreateTestSetDiscrepancyReportResponse (Prelude.Maybe Prelude.Text)
createTestSetDiscrepancyReportResponse_testSetId = Lens.lens (\CreateTestSetDiscrepancyReportResponse' {testSetId} -> testSetId) (\s@CreateTestSetDiscrepancyReportResponse' {} a -> s {testSetId = a} :: CreateTestSetDiscrepancyReportResponse)

-- | The response's http status code.
createTestSetDiscrepancyReportResponse_httpStatus :: Lens.Lens' CreateTestSetDiscrepancyReportResponse Prelude.Int
createTestSetDiscrepancyReportResponse_httpStatus = Lens.lens (\CreateTestSetDiscrepancyReportResponse' {httpStatus} -> httpStatus) (\s@CreateTestSetDiscrepancyReportResponse' {} a -> s {httpStatus = a} :: CreateTestSetDiscrepancyReportResponse)

instance
  Prelude.NFData
    CreateTestSetDiscrepancyReportResponse
  where
  rnf CreateTestSetDiscrepancyReportResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf testSetDiscrepancyReportId
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf httpStatus
