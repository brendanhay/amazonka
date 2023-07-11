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
-- Module      : Amazonka.SageMakerA2IRuntime.DescribeHumanLoop
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified human loop. If the human loop
-- was deleted, this operation will return a @ResourceNotFoundException@
-- error.
module Amazonka.SageMakerA2IRuntime.DescribeHumanLoop
  ( -- * Creating a Request
    DescribeHumanLoop (..),
    newDescribeHumanLoop,

    -- * Request Lenses
    describeHumanLoop_humanLoopName,

    -- * Destructuring the Response
    DescribeHumanLoopResponse (..),
    newDescribeHumanLoopResponse,

    -- * Response Lenses
    describeHumanLoopResponse_failureCode,
    describeHumanLoopResponse_failureReason,
    describeHumanLoopResponse_humanLoopOutput,
    describeHumanLoopResponse_httpStatus,
    describeHumanLoopResponse_creationTime,
    describeHumanLoopResponse_humanLoopStatus,
    describeHumanLoopResponse_humanLoopName,
    describeHumanLoopResponse_humanLoopArn,
    describeHumanLoopResponse_flowDefinitionArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerA2IRuntime.Types

-- | /See:/ 'newDescribeHumanLoop' smart constructor.
data DescribeHumanLoop = DescribeHumanLoop'
  { -- | The name of the human loop that you want information about.
    humanLoopName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHumanLoop' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopName', 'describeHumanLoop_humanLoopName' - The name of the human loop that you want information about.
newDescribeHumanLoop ::
  -- | 'humanLoopName'
  Prelude.Text ->
  DescribeHumanLoop
newDescribeHumanLoop pHumanLoopName_ =
  DescribeHumanLoop' {humanLoopName = pHumanLoopName_}

-- | The name of the human loop that you want information about.
describeHumanLoop_humanLoopName :: Lens.Lens' DescribeHumanLoop Prelude.Text
describeHumanLoop_humanLoopName = Lens.lens (\DescribeHumanLoop' {humanLoopName} -> humanLoopName) (\s@DescribeHumanLoop' {} a -> s {humanLoopName = a} :: DescribeHumanLoop)

instance Core.AWSRequest DescribeHumanLoop where
  type
    AWSResponse DescribeHumanLoop =
      DescribeHumanLoopResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHumanLoopResponse'
            Prelude.<$> (x Data..?> "FailureCode")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "HumanLoopOutput")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "HumanLoopStatus")
            Prelude.<*> (x Data..:> "HumanLoopName")
            Prelude.<*> (x Data..:> "HumanLoopArn")
            Prelude.<*> (x Data..:> "FlowDefinitionArn")
      )

instance Prelude.Hashable DescribeHumanLoop where
  hashWithSalt _salt DescribeHumanLoop' {..} =
    _salt `Prelude.hashWithSalt` humanLoopName

instance Prelude.NFData DescribeHumanLoop where
  rnf DescribeHumanLoop' {..} =
    Prelude.rnf humanLoopName

instance Data.ToHeaders DescribeHumanLoop where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeHumanLoop where
  toPath DescribeHumanLoop' {..} =
    Prelude.mconcat
      ["/human-loops/", Data.toBS humanLoopName]

instance Data.ToQuery DescribeHumanLoop where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHumanLoopResponse' smart constructor.
data DescribeHumanLoopResponse = DescribeHumanLoopResponse'
  { -- | A failure code that identifies the type of failure.
    --
    -- Possible values: @ValidationError@, @Expired@, @InternalError@
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The reason why a human loop failed. The failure reason is returned when
    -- the status of the human loop is @Failed@.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about the output of the human loop.
    humanLoopOutput :: Prelude.Maybe HumanLoopOutput,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The creation time when Amazon Augmented AI created the human loop.
    creationTime :: Data.ISO8601,
    -- | The status of the human loop.
    humanLoopStatus :: HumanLoopStatus,
    -- | The name of the human loop. The name must be lowercase, unique within
    -- the Region in your account, and can have up to 63 characters. Valid
    -- characters: a-z, 0-9, and - (hyphen).
    humanLoopName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the human loop.
    humanLoopArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHumanLoopResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'describeHumanLoopResponse_failureCode' - A failure code that identifies the type of failure.
--
-- Possible values: @ValidationError@, @Expired@, @InternalError@
--
-- 'failureReason', 'describeHumanLoopResponse_failureReason' - The reason why a human loop failed. The failure reason is returned when
-- the status of the human loop is @Failed@.
--
-- 'humanLoopOutput', 'describeHumanLoopResponse_humanLoopOutput' - An object that contains information about the output of the human loop.
--
-- 'httpStatus', 'describeHumanLoopResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'describeHumanLoopResponse_creationTime' - The creation time when Amazon Augmented AI created the human loop.
--
-- 'humanLoopStatus', 'describeHumanLoopResponse_humanLoopStatus' - The status of the human loop.
--
-- 'humanLoopName', 'describeHumanLoopResponse_humanLoopName' - The name of the human loop. The name must be lowercase, unique within
-- the Region in your account, and can have up to 63 characters. Valid
-- characters: a-z, 0-9, and - (hyphen).
--
-- 'humanLoopArn', 'describeHumanLoopResponse_humanLoopArn' - The Amazon Resource Name (ARN) of the human loop.
--
-- 'flowDefinitionArn', 'describeHumanLoopResponse_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow definition.
newDescribeHumanLoopResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'humanLoopStatus'
  HumanLoopStatus ->
  -- | 'humanLoopName'
  Prelude.Text ->
  -- | 'humanLoopArn'
  Prelude.Text ->
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  DescribeHumanLoopResponse
newDescribeHumanLoopResponse
  pHttpStatus_
  pCreationTime_
  pHumanLoopStatus_
  pHumanLoopName_
  pHumanLoopArn_
  pFlowDefinitionArn_ =
    DescribeHumanLoopResponse'
      { failureCode =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        humanLoopOutput = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        humanLoopStatus = pHumanLoopStatus_,
        humanLoopName = pHumanLoopName_,
        humanLoopArn = pHumanLoopArn_,
        flowDefinitionArn = pFlowDefinitionArn_
      }

-- | A failure code that identifies the type of failure.
--
-- Possible values: @ValidationError@, @Expired@, @InternalError@
describeHumanLoopResponse_failureCode :: Lens.Lens' DescribeHumanLoopResponse (Prelude.Maybe Prelude.Text)
describeHumanLoopResponse_failureCode = Lens.lens (\DescribeHumanLoopResponse' {failureCode} -> failureCode) (\s@DescribeHumanLoopResponse' {} a -> s {failureCode = a} :: DescribeHumanLoopResponse)

-- | The reason why a human loop failed. The failure reason is returned when
-- the status of the human loop is @Failed@.
describeHumanLoopResponse_failureReason :: Lens.Lens' DescribeHumanLoopResponse (Prelude.Maybe Prelude.Text)
describeHumanLoopResponse_failureReason = Lens.lens (\DescribeHumanLoopResponse' {failureReason} -> failureReason) (\s@DescribeHumanLoopResponse' {} a -> s {failureReason = a} :: DescribeHumanLoopResponse)

-- | An object that contains information about the output of the human loop.
describeHumanLoopResponse_humanLoopOutput :: Lens.Lens' DescribeHumanLoopResponse (Prelude.Maybe HumanLoopOutput)
describeHumanLoopResponse_humanLoopOutput = Lens.lens (\DescribeHumanLoopResponse' {humanLoopOutput} -> humanLoopOutput) (\s@DescribeHumanLoopResponse' {} a -> s {humanLoopOutput = a} :: DescribeHumanLoopResponse)

-- | The response's http status code.
describeHumanLoopResponse_httpStatus :: Lens.Lens' DescribeHumanLoopResponse Prelude.Int
describeHumanLoopResponse_httpStatus = Lens.lens (\DescribeHumanLoopResponse' {httpStatus} -> httpStatus) (\s@DescribeHumanLoopResponse' {} a -> s {httpStatus = a} :: DescribeHumanLoopResponse)

-- | The creation time when Amazon Augmented AI created the human loop.
describeHumanLoopResponse_creationTime :: Lens.Lens' DescribeHumanLoopResponse Prelude.UTCTime
describeHumanLoopResponse_creationTime = Lens.lens (\DescribeHumanLoopResponse' {creationTime} -> creationTime) (\s@DescribeHumanLoopResponse' {} a -> s {creationTime = a} :: DescribeHumanLoopResponse) Prelude.. Data._Time

-- | The status of the human loop.
describeHumanLoopResponse_humanLoopStatus :: Lens.Lens' DescribeHumanLoopResponse HumanLoopStatus
describeHumanLoopResponse_humanLoopStatus = Lens.lens (\DescribeHumanLoopResponse' {humanLoopStatus} -> humanLoopStatus) (\s@DescribeHumanLoopResponse' {} a -> s {humanLoopStatus = a} :: DescribeHumanLoopResponse)

-- | The name of the human loop. The name must be lowercase, unique within
-- the Region in your account, and can have up to 63 characters. Valid
-- characters: a-z, 0-9, and - (hyphen).
describeHumanLoopResponse_humanLoopName :: Lens.Lens' DescribeHumanLoopResponse Prelude.Text
describeHumanLoopResponse_humanLoopName = Lens.lens (\DescribeHumanLoopResponse' {humanLoopName} -> humanLoopName) (\s@DescribeHumanLoopResponse' {} a -> s {humanLoopName = a} :: DescribeHumanLoopResponse)

-- | The Amazon Resource Name (ARN) of the human loop.
describeHumanLoopResponse_humanLoopArn :: Lens.Lens' DescribeHumanLoopResponse Prelude.Text
describeHumanLoopResponse_humanLoopArn = Lens.lens (\DescribeHumanLoopResponse' {humanLoopArn} -> humanLoopArn) (\s@DescribeHumanLoopResponse' {} a -> s {humanLoopArn = a} :: DescribeHumanLoopResponse)

-- | The Amazon Resource Name (ARN) of the flow definition.
describeHumanLoopResponse_flowDefinitionArn :: Lens.Lens' DescribeHumanLoopResponse Prelude.Text
describeHumanLoopResponse_flowDefinitionArn = Lens.lens (\DescribeHumanLoopResponse' {flowDefinitionArn} -> flowDefinitionArn) (\s@DescribeHumanLoopResponse' {} a -> s {flowDefinitionArn = a} :: DescribeHumanLoopResponse)

instance Prelude.NFData DescribeHumanLoopResponse where
  rnf DescribeHumanLoopResponse' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf humanLoopOutput
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf humanLoopStatus
      `Prelude.seq` Prelude.rnf humanLoopName
      `Prelude.seq` Prelude.rnf humanLoopArn
      `Prelude.seq` Prelude.rnf flowDefinitionArn
