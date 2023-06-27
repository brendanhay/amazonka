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
-- Module      : Amazonka.LexV2Models.StartTestExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The action to start test set execution.
module Amazonka.LexV2Models.StartTestExecution
  ( -- * Creating a Request
    StartTestExecution (..),
    newStartTestExecution,

    -- * Request Lenses
    startTestExecution_testExecutionModality,
    startTestExecution_testSetId,
    startTestExecution_target,
    startTestExecution_apiMode,

    -- * Destructuring the Response
    StartTestExecutionResponse (..),
    newStartTestExecutionResponse,

    -- * Response Lenses
    startTestExecutionResponse_apiMode,
    startTestExecutionResponse_creationDateTime,
    startTestExecutionResponse_target,
    startTestExecutionResponse_testExecutionId,
    startTestExecutionResponse_testExecutionModality,
    startTestExecutionResponse_testSetId,
    startTestExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTestExecution' smart constructor.
data StartTestExecution = StartTestExecution'
  { -- | Indicates whether audio or text is used.
    testExecutionModality :: Prelude.Maybe TestExecutionModality,
    -- | The test set Id for the test set execution.
    testSetId :: Prelude.Text,
    -- | The target bot for the test set execution.
    target :: TestExecutionTarget,
    -- | Indicates whether we use streaming or non-streaming APIs for the test
    -- set execution. For streaming, StartConversation Runtime API is used.
    -- Whereas, for non-streaming, RecognizeUtterance and RecognizeText Amazon
    -- Lex Runtime API are used.
    apiMode :: TestExecutionApiMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTestExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testExecutionModality', 'startTestExecution_testExecutionModality' - Indicates whether audio or text is used.
--
-- 'testSetId', 'startTestExecution_testSetId' - The test set Id for the test set execution.
--
-- 'target', 'startTestExecution_target' - The target bot for the test set execution.
--
-- 'apiMode', 'startTestExecution_apiMode' - Indicates whether we use streaming or non-streaming APIs for the test
-- set execution. For streaming, StartConversation Runtime API is used.
-- Whereas, for non-streaming, RecognizeUtterance and RecognizeText Amazon
-- Lex Runtime API are used.
newStartTestExecution ::
  -- | 'testSetId'
  Prelude.Text ->
  -- | 'target'
  TestExecutionTarget ->
  -- | 'apiMode'
  TestExecutionApiMode ->
  StartTestExecution
newStartTestExecution pTestSetId_ pTarget_ pApiMode_ =
  StartTestExecution'
    { testExecutionModality =
        Prelude.Nothing,
      testSetId = pTestSetId_,
      target = pTarget_,
      apiMode = pApiMode_
    }

-- | Indicates whether audio or text is used.
startTestExecution_testExecutionModality :: Lens.Lens' StartTestExecution (Prelude.Maybe TestExecutionModality)
startTestExecution_testExecutionModality = Lens.lens (\StartTestExecution' {testExecutionModality} -> testExecutionModality) (\s@StartTestExecution' {} a -> s {testExecutionModality = a} :: StartTestExecution)

-- | The test set Id for the test set execution.
startTestExecution_testSetId :: Lens.Lens' StartTestExecution Prelude.Text
startTestExecution_testSetId = Lens.lens (\StartTestExecution' {testSetId} -> testSetId) (\s@StartTestExecution' {} a -> s {testSetId = a} :: StartTestExecution)

-- | The target bot for the test set execution.
startTestExecution_target :: Lens.Lens' StartTestExecution TestExecutionTarget
startTestExecution_target = Lens.lens (\StartTestExecution' {target} -> target) (\s@StartTestExecution' {} a -> s {target = a} :: StartTestExecution)

-- | Indicates whether we use streaming or non-streaming APIs for the test
-- set execution. For streaming, StartConversation Runtime API is used.
-- Whereas, for non-streaming, RecognizeUtterance and RecognizeText Amazon
-- Lex Runtime API are used.
startTestExecution_apiMode :: Lens.Lens' StartTestExecution TestExecutionApiMode
startTestExecution_apiMode = Lens.lens (\StartTestExecution' {apiMode} -> apiMode) (\s@StartTestExecution' {} a -> s {apiMode = a} :: StartTestExecution)

instance Core.AWSRequest StartTestExecution where
  type
    AWSResponse StartTestExecution =
      StartTestExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTestExecutionResponse'
            Prelude.<$> (x Data..?> "apiMode")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "target")
            Prelude.<*> (x Data..?> "testExecutionId")
            Prelude.<*> (x Data..?> "testExecutionModality")
            Prelude.<*> (x Data..?> "testSetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTestExecution where
  hashWithSalt _salt StartTestExecution' {..} =
    _salt
      `Prelude.hashWithSalt` testExecutionModality
      `Prelude.hashWithSalt` testSetId
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` apiMode

instance Prelude.NFData StartTestExecution where
  rnf StartTestExecution' {..} =
    Prelude.rnf testExecutionModality
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf apiMode

instance Data.ToHeaders StartTestExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTestExecution where
  toJSON StartTestExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("testExecutionModality" Data..=)
              Prelude.<$> testExecutionModality,
            Prelude.Just ("target" Data..= target),
            Prelude.Just ("apiMode" Data..= apiMode)
          ]
      )

instance Data.ToPath StartTestExecution where
  toPath StartTestExecution' {..} =
    Prelude.mconcat
      [ "/testsets/",
        Data.toBS testSetId,
        "/testexecutions"
      ]

instance Data.ToQuery StartTestExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTestExecutionResponse' smart constructor.
data StartTestExecutionResponse = StartTestExecutionResponse'
  { -- | Indicates whether we use streaming or non-streaming APIs for the test
    -- set execution. For streaming, StartConversation Amazon Lex Runtime API
    -- is used. Whereas for non-streaming, RecognizeUtterance and RecognizeText
    -- Amazon Lex Runtime API are used.
    apiMode :: Prelude.Maybe TestExecutionApiMode,
    -- | The creation date and time for the test set execution.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The target bot for the test set execution.
    target :: Prelude.Maybe TestExecutionTarget,
    -- | The unique identifier of the test set execution.
    testExecutionId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether audio or text is used.
    testExecutionModality :: Prelude.Maybe TestExecutionModality,
    -- | The test set Id for the test set execution.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTestExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMode', 'startTestExecutionResponse_apiMode' - Indicates whether we use streaming or non-streaming APIs for the test
-- set execution. For streaming, StartConversation Amazon Lex Runtime API
-- is used. Whereas for non-streaming, RecognizeUtterance and RecognizeText
-- Amazon Lex Runtime API are used.
--
-- 'creationDateTime', 'startTestExecutionResponse_creationDateTime' - The creation date and time for the test set execution.
--
-- 'target', 'startTestExecutionResponse_target' - The target bot for the test set execution.
--
-- 'testExecutionId', 'startTestExecutionResponse_testExecutionId' - The unique identifier of the test set execution.
--
-- 'testExecutionModality', 'startTestExecutionResponse_testExecutionModality' - Indicates whether audio or text is used.
--
-- 'testSetId', 'startTestExecutionResponse_testSetId' - The test set Id for the test set execution.
--
-- 'httpStatus', 'startTestExecutionResponse_httpStatus' - The response's http status code.
newStartTestExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTestExecutionResponse
newStartTestExecutionResponse pHttpStatus_ =
  StartTestExecutionResponse'
    { apiMode =
        Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      target = Prelude.Nothing,
      testExecutionId = Prelude.Nothing,
      testExecutionModality = Prelude.Nothing,
      testSetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether we use streaming or non-streaming APIs for the test
-- set execution. For streaming, StartConversation Amazon Lex Runtime API
-- is used. Whereas for non-streaming, RecognizeUtterance and RecognizeText
-- Amazon Lex Runtime API are used.
startTestExecutionResponse_apiMode :: Lens.Lens' StartTestExecutionResponse (Prelude.Maybe TestExecutionApiMode)
startTestExecutionResponse_apiMode = Lens.lens (\StartTestExecutionResponse' {apiMode} -> apiMode) (\s@StartTestExecutionResponse' {} a -> s {apiMode = a} :: StartTestExecutionResponse)

-- | The creation date and time for the test set execution.
startTestExecutionResponse_creationDateTime :: Lens.Lens' StartTestExecutionResponse (Prelude.Maybe Prelude.UTCTime)
startTestExecutionResponse_creationDateTime = Lens.lens (\StartTestExecutionResponse' {creationDateTime} -> creationDateTime) (\s@StartTestExecutionResponse' {} a -> s {creationDateTime = a} :: StartTestExecutionResponse) Prelude.. Lens.mapping Data._Time

-- | The target bot for the test set execution.
startTestExecutionResponse_target :: Lens.Lens' StartTestExecutionResponse (Prelude.Maybe TestExecutionTarget)
startTestExecutionResponse_target = Lens.lens (\StartTestExecutionResponse' {target} -> target) (\s@StartTestExecutionResponse' {} a -> s {target = a} :: StartTestExecutionResponse)

-- | The unique identifier of the test set execution.
startTestExecutionResponse_testExecutionId :: Lens.Lens' StartTestExecutionResponse (Prelude.Maybe Prelude.Text)
startTestExecutionResponse_testExecutionId = Lens.lens (\StartTestExecutionResponse' {testExecutionId} -> testExecutionId) (\s@StartTestExecutionResponse' {} a -> s {testExecutionId = a} :: StartTestExecutionResponse)

-- | Indicates whether audio or text is used.
startTestExecutionResponse_testExecutionModality :: Lens.Lens' StartTestExecutionResponse (Prelude.Maybe TestExecutionModality)
startTestExecutionResponse_testExecutionModality = Lens.lens (\StartTestExecutionResponse' {testExecutionModality} -> testExecutionModality) (\s@StartTestExecutionResponse' {} a -> s {testExecutionModality = a} :: StartTestExecutionResponse)

-- | The test set Id for the test set execution.
startTestExecutionResponse_testSetId :: Lens.Lens' StartTestExecutionResponse (Prelude.Maybe Prelude.Text)
startTestExecutionResponse_testSetId = Lens.lens (\StartTestExecutionResponse' {testSetId} -> testSetId) (\s@StartTestExecutionResponse' {} a -> s {testSetId = a} :: StartTestExecutionResponse)

-- | The response's http status code.
startTestExecutionResponse_httpStatus :: Lens.Lens' StartTestExecutionResponse Prelude.Int
startTestExecutionResponse_httpStatus = Lens.lens (\StartTestExecutionResponse' {httpStatus} -> httpStatus) (\s@StartTestExecutionResponse' {} a -> s {httpStatus = a} :: StartTestExecutionResponse)

instance Prelude.NFData StartTestExecutionResponse where
  rnf StartTestExecutionResponse' {..} =
    Prelude.rnf apiMode
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf testExecutionId
      `Prelude.seq` Prelude.rnf testExecutionModality
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf httpStatus
