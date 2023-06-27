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
-- Module      : Amazonka.LexV2Models.DescribeTestExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about the test execution.
module Amazonka.LexV2Models.DescribeTestExecution
  ( -- * Creating a Request
    DescribeTestExecution (..),
    newDescribeTestExecution,

    -- * Request Lenses
    describeTestExecution_testExecutionId,

    -- * Destructuring the Response
    DescribeTestExecutionResponse (..),
    newDescribeTestExecutionResponse,

    -- * Response Lenses
    describeTestExecutionResponse_apiMode,
    describeTestExecutionResponse_creationDateTime,
    describeTestExecutionResponse_failureReasons,
    describeTestExecutionResponse_lastUpdatedDateTime,
    describeTestExecutionResponse_target,
    describeTestExecutionResponse_testExecutionId,
    describeTestExecutionResponse_testExecutionModality,
    describeTestExecutionResponse_testExecutionStatus,
    describeTestExecutionResponse_testSetId,
    describeTestExecutionResponse_testSetName,
    describeTestExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTestExecution' smart constructor.
data DescribeTestExecution = DescribeTestExecution'
  { -- | The execution Id of the test set execution.
    testExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testExecutionId', 'describeTestExecution_testExecutionId' - The execution Id of the test set execution.
newDescribeTestExecution ::
  -- | 'testExecutionId'
  Prelude.Text ->
  DescribeTestExecution
newDescribeTestExecution pTestExecutionId_ =
  DescribeTestExecution'
    { testExecutionId =
        pTestExecutionId_
    }

-- | The execution Id of the test set execution.
describeTestExecution_testExecutionId :: Lens.Lens' DescribeTestExecution Prelude.Text
describeTestExecution_testExecutionId = Lens.lens (\DescribeTestExecution' {testExecutionId} -> testExecutionId) (\s@DescribeTestExecution' {} a -> s {testExecutionId = a} :: DescribeTestExecution)

instance Core.AWSRequest DescribeTestExecution where
  type
    AWSResponse DescribeTestExecution =
      DescribeTestExecutionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestExecutionResponse'
            Prelude.<$> (x Data..?> "apiMode")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "target")
            Prelude.<*> (x Data..?> "testExecutionId")
            Prelude.<*> (x Data..?> "testExecutionModality")
            Prelude.<*> (x Data..?> "testExecutionStatus")
            Prelude.<*> (x Data..?> "testSetId")
            Prelude.<*> (x Data..?> "testSetName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTestExecution where
  hashWithSalt _salt DescribeTestExecution' {..} =
    _salt `Prelude.hashWithSalt` testExecutionId

instance Prelude.NFData DescribeTestExecution where
  rnf DescribeTestExecution' {..} =
    Prelude.rnf testExecutionId

instance Data.ToHeaders DescribeTestExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTestExecution where
  toPath DescribeTestExecution' {..} =
    Prelude.mconcat
      ["/testexecutions/", Data.toBS testExecutionId]

instance Data.ToQuery DescribeTestExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTestExecutionResponse' smart constructor.
data DescribeTestExecutionResponse = DescribeTestExecutionResponse'
  { -- | Indicates whether we use streaming or non-streaming APIs are used for
    -- the test set execution. For streaming, @StartConversation@ Amazon Lex
    -- Runtime API is used. Whereas for non-streaming, @RecognizeUtterance@ and
    -- @RecognizeText@ Amazon Lex Runtime API is used.
    apiMode :: Prelude.Maybe TestExecutionApiMode,
    -- | The execution creation date and time for the test set execution.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | Reasons for the failure of the test set execution.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The date and time of the last update for the execution.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The target bot for the test set execution details.
    target :: Prelude.Maybe TestExecutionTarget,
    -- | The execution Id for the test set execution.
    testExecutionId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether test set is audio or text.
    testExecutionModality :: Prelude.Maybe TestExecutionModality,
    -- | The test execution status for the test execution.
    testExecutionStatus :: Prelude.Maybe TestExecutionStatus,
    -- | The test set Id for the test set execution.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The test set name of the test set execution.
    testSetName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMode', 'describeTestExecutionResponse_apiMode' - Indicates whether we use streaming or non-streaming APIs are used for
-- the test set execution. For streaming, @StartConversation@ Amazon Lex
-- Runtime API is used. Whereas for non-streaming, @RecognizeUtterance@ and
-- @RecognizeText@ Amazon Lex Runtime API is used.
--
-- 'creationDateTime', 'describeTestExecutionResponse_creationDateTime' - The execution creation date and time for the test set execution.
--
-- 'failureReasons', 'describeTestExecutionResponse_failureReasons' - Reasons for the failure of the test set execution.
--
-- 'lastUpdatedDateTime', 'describeTestExecutionResponse_lastUpdatedDateTime' - The date and time of the last update for the execution.
--
-- 'target', 'describeTestExecutionResponse_target' - The target bot for the test set execution details.
--
-- 'testExecutionId', 'describeTestExecutionResponse_testExecutionId' - The execution Id for the test set execution.
--
-- 'testExecutionModality', 'describeTestExecutionResponse_testExecutionModality' - Indicates whether test set is audio or text.
--
-- 'testExecutionStatus', 'describeTestExecutionResponse_testExecutionStatus' - The test execution status for the test execution.
--
-- 'testSetId', 'describeTestExecutionResponse_testSetId' - The test set Id for the test set execution.
--
-- 'testSetName', 'describeTestExecutionResponse_testSetName' - The test set name of the test set execution.
--
-- 'httpStatus', 'describeTestExecutionResponse_httpStatus' - The response's http status code.
newDescribeTestExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTestExecutionResponse
newDescribeTestExecutionResponse pHttpStatus_ =
  DescribeTestExecutionResponse'
    { apiMode =
        Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      target = Prelude.Nothing,
      testExecutionId = Prelude.Nothing,
      testExecutionModality = Prelude.Nothing,
      testExecutionStatus = Prelude.Nothing,
      testSetId = Prelude.Nothing,
      testSetName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether we use streaming or non-streaming APIs are used for
-- the test set execution. For streaming, @StartConversation@ Amazon Lex
-- Runtime API is used. Whereas for non-streaming, @RecognizeUtterance@ and
-- @RecognizeText@ Amazon Lex Runtime API is used.
describeTestExecutionResponse_apiMode :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe TestExecutionApiMode)
describeTestExecutionResponse_apiMode = Lens.lens (\DescribeTestExecutionResponse' {apiMode} -> apiMode) (\s@DescribeTestExecutionResponse' {} a -> s {apiMode = a} :: DescribeTestExecutionResponse)

-- | The execution creation date and time for the test set execution.
describeTestExecutionResponse_creationDateTime :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeTestExecutionResponse_creationDateTime = Lens.lens (\DescribeTestExecutionResponse' {creationDateTime} -> creationDateTime) (\s@DescribeTestExecutionResponse' {} a -> s {creationDateTime = a} :: DescribeTestExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | Reasons for the failure of the test set execution.
describeTestExecutionResponse_failureReasons :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe [Prelude.Text])
describeTestExecutionResponse_failureReasons = Lens.lens (\DescribeTestExecutionResponse' {failureReasons} -> failureReasons) (\s@DescribeTestExecutionResponse' {} a -> s {failureReasons = a} :: DescribeTestExecutionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time of the last update for the execution.
describeTestExecutionResponse_lastUpdatedDateTime :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeTestExecutionResponse_lastUpdatedDateTime = Lens.lens (\DescribeTestExecutionResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeTestExecutionResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeTestExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The target bot for the test set execution details.
describeTestExecutionResponse_target :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe TestExecutionTarget)
describeTestExecutionResponse_target = Lens.lens (\DescribeTestExecutionResponse' {target} -> target) (\s@DescribeTestExecutionResponse' {} a -> s {target = a} :: DescribeTestExecutionResponse)

-- | The execution Id for the test set execution.
describeTestExecutionResponse_testExecutionId :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe Prelude.Text)
describeTestExecutionResponse_testExecutionId = Lens.lens (\DescribeTestExecutionResponse' {testExecutionId} -> testExecutionId) (\s@DescribeTestExecutionResponse' {} a -> s {testExecutionId = a} :: DescribeTestExecutionResponse)

-- | Indicates whether test set is audio or text.
describeTestExecutionResponse_testExecutionModality :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe TestExecutionModality)
describeTestExecutionResponse_testExecutionModality = Lens.lens (\DescribeTestExecutionResponse' {testExecutionModality} -> testExecutionModality) (\s@DescribeTestExecutionResponse' {} a -> s {testExecutionModality = a} :: DescribeTestExecutionResponse)

-- | The test execution status for the test execution.
describeTestExecutionResponse_testExecutionStatus :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe TestExecutionStatus)
describeTestExecutionResponse_testExecutionStatus = Lens.lens (\DescribeTestExecutionResponse' {testExecutionStatus} -> testExecutionStatus) (\s@DescribeTestExecutionResponse' {} a -> s {testExecutionStatus = a} :: DescribeTestExecutionResponse)

-- | The test set Id for the test set execution.
describeTestExecutionResponse_testSetId :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe Prelude.Text)
describeTestExecutionResponse_testSetId = Lens.lens (\DescribeTestExecutionResponse' {testSetId} -> testSetId) (\s@DescribeTestExecutionResponse' {} a -> s {testSetId = a} :: DescribeTestExecutionResponse)

-- | The test set name of the test set execution.
describeTestExecutionResponse_testSetName :: Lens.Lens' DescribeTestExecutionResponse (Prelude.Maybe Prelude.Text)
describeTestExecutionResponse_testSetName = Lens.lens (\DescribeTestExecutionResponse' {testSetName} -> testSetName) (\s@DescribeTestExecutionResponse' {} a -> s {testSetName = a} :: DescribeTestExecutionResponse)

-- | The response's http status code.
describeTestExecutionResponse_httpStatus :: Lens.Lens' DescribeTestExecutionResponse Prelude.Int
describeTestExecutionResponse_httpStatus = Lens.lens (\DescribeTestExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeTestExecutionResponse' {} a -> s {httpStatus = a} :: DescribeTestExecutionResponse)

instance Prelude.NFData DescribeTestExecutionResponse where
  rnf DescribeTestExecutionResponse' {..} =
    Prelude.rnf apiMode
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf testExecutionId
      `Prelude.seq` Prelude.rnf testExecutionModality
      `Prelude.seq` Prelude.rnf testExecutionStatus
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf testSetName
      `Prelude.seq` Prelude.rnf httpStatus
