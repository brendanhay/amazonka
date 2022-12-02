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
-- Module      : Amazonka.SageMaker.DescribeFlowDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified flow definition.
module Amazonka.SageMaker.DescribeFlowDefinition
  ( -- * Creating a Request
    DescribeFlowDefinition (..),
    newDescribeFlowDefinition,

    -- * Request Lenses
    describeFlowDefinition_flowDefinitionName,

    -- * Destructuring the Response
    DescribeFlowDefinitionResponse (..),
    newDescribeFlowDefinitionResponse,

    -- * Response Lenses
    describeFlowDefinitionResponse_humanLoopActivationConfig,
    describeFlowDefinitionResponse_humanLoopRequestSource,
    describeFlowDefinitionResponse_failureReason,
    describeFlowDefinitionResponse_httpStatus,
    describeFlowDefinitionResponse_flowDefinitionArn,
    describeFlowDefinitionResponse_flowDefinitionName,
    describeFlowDefinitionResponse_flowDefinitionStatus,
    describeFlowDefinitionResponse_creationTime,
    describeFlowDefinitionResponse_humanLoopConfig,
    describeFlowDefinitionResponse_outputConfig,
    describeFlowDefinitionResponse_roleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeFlowDefinition' smart constructor.
data DescribeFlowDefinition = DescribeFlowDefinition'
  { -- | The name of the flow definition.
    flowDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlowDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowDefinitionName', 'describeFlowDefinition_flowDefinitionName' - The name of the flow definition.
newDescribeFlowDefinition ::
  -- | 'flowDefinitionName'
  Prelude.Text ->
  DescribeFlowDefinition
newDescribeFlowDefinition pFlowDefinitionName_ =
  DescribeFlowDefinition'
    { flowDefinitionName =
        pFlowDefinitionName_
    }

-- | The name of the flow definition.
describeFlowDefinition_flowDefinitionName :: Lens.Lens' DescribeFlowDefinition Prelude.Text
describeFlowDefinition_flowDefinitionName = Lens.lens (\DescribeFlowDefinition' {flowDefinitionName} -> flowDefinitionName) (\s@DescribeFlowDefinition' {} a -> s {flowDefinitionName = a} :: DescribeFlowDefinition)

instance Core.AWSRequest DescribeFlowDefinition where
  type
    AWSResponse DescribeFlowDefinition =
      DescribeFlowDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlowDefinitionResponse'
            Prelude.<$> (x Data..?> "HumanLoopActivationConfig")
            Prelude.<*> (x Data..?> "HumanLoopRequestSource")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FlowDefinitionArn")
            Prelude.<*> (x Data..:> "FlowDefinitionName")
            Prelude.<*> (x Data..:> "FlowDefinitionStatus")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "HumanLoopConfig")
            Prelude.<*> (x Data..:> "OutputConfig")
            Prelude.<*> (x Data..:> "RoleArn")
      )

instance Prelude.Hashable DescribeFlowDefinition where
  hashWithSalt _salt DescribeFlowDefinition' {..} =
    _salt `Prelude.hashWithSalt` flowDefinitionName

instance Prelude.NFData DescribeFlowDefinition where
  rnf DescribeFlowDefinition' {..} =
    Prelude.rnf flowDefinitionName

instance Data.ToHeaders DescribeFlowDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeFlowDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFlowDefinition where
  toJSON DescribeFlowDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FlowDefinitionName" Data..= flowDefinitionName)
          ]
      )

instance Data.ToPath DescribeFlowDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFlowDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'
  { -- | An object containing information about what triggers a human review
    -- workflow.
    humanLoopActivationConfig :: Prelude.Maybe HumanLoopActivationConfig,
    -- | Container for configuring the source of human task requests. Used to
    -- specify if Amazon Rekognition or Amazon Textract is used as an
    -- integration source.
    humanLoopRequestSource :: Prelude.Maybe HumanLoopRequestSource,
    -- | The reason your flow definition failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the flow defintion.
    flowDefinitionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionName :: Prelude.Text,
    -- | The status of the flow definition. Valid values are listed below.
    flowDefinitionStatus :: FlowDefinitionStatus,
    -- | The timestamp when the flow definition was created.
    creationTime :: Data.POSIX,
    -- | An object containing information about who works on the task, the
    -- workforce task price, and other task details.
    humanLoopConfig :: HumanLoopConfig,
    -- | An object containing information about the output file.
    outputConfig :: FlowDefinitionOutputConfig,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
    -- Access Management (IAM) execution role for the flow definition.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlowDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopActivationConfig', 'describeFlowDefinitionResponse_humanLoopActivationConfig' - An object containing information about what triggers a human review
-- workflow.
--
-- 'humanLoopRequestSource', 'describeFlowDefinitionResponse_humanLoopRequestSource' - Container for configuring the source of human task requests. Used to
-- specify if Amazon Rekognition or Amazon Textract is used as an
-- integration source.
--
-- 'failureReason', 'describeFlowDefinitionResponse_failureReason' - The reason your flow definition failed.
--
-- 'httpStatus', 'describeFlowDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'flowDefinitionArn', 'describeFlowDefinitionResponse_flowDefinitionArn' - The Amazon Resource Name (ARN) of the flow defintion.
--
-- 'flowDefinitionName', 'describeFlowDefinitionResponse_flowDefinitionName' - The Amazon Resource Name (ARN) of the flow definition.
--
-- 'flowDefinitionStatus', 'describeFlowDefinitionResponse_flowDefinitionStatus' - The status of the flow definition. Valid values are listed below.
--
-- 'creationTime', 'describeFlowDefinitionResponse_creationTime' - The timestamp when the flow definition was created.
--
-- 'humanLoopConfig', 'describeFlowDefinitionResponse_humanLoopConfig' - An object containing information about who works on the task, the
-- workforce task price, and other task details.
--
-- 'outputConfig', 'describeFlowDefinitionResponse_outputConfig' - An object containing information about the output file.
--
-- 'roleArn', 'describeFlowDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) execution role for the flow definition.
newDescribeFlowDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'flowDefinitionArn'
  Prelude.Text ->
  -- | 'flowDefinitionName'
  Prelude.Text ->
  -- | 'flowDefinitionStatus'
  FlowDefinitionStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'humanLoopConfig'
  HumanLoopConfig ->
  -- | 'outputConfig'
  FlowDefinitionOutputConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  DescribeFlowDefinitionResponse
newDescribeFlowDefinitionResponse
  pHttpStatus_
  pFlowDefinitionArn_
  pFlowDefinitionName_
  pFlowDefinitionStatus_
  pCreationTime_
  pHumanLoopConfig_
  pOutputConfig_
  pRoleArn_ =
    DescribeFlowDefinitionResponse'
      { humanLoopActivationConfig =
          Prelude.Nothing,
        humanLoopRequestSource = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        flowDefinitionArn = pFlowDefinitionArn_,
        flowDefinitionName = pFlowDefinitionName_,
        flowDefinitionStatus =
          pFlowDefinitionStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        humanLoopConfig = pHumanLoopConfig_,
        outputConfig = pOutputConfig_,
        roleArn = pRoleArn_
      }

-- | An object containing information about what triggers a human review
-- workflow.
describeFlowDefinitionResponse_humanLoopActivationConfig :: Lens.Lens' DescribeFlowDefinitionResponse (Prelude.Maybe HumanLoopActivationConfig)
describeFlowDefinitionResponse_humanLoopActivationConfig = Lens.lens (\DescribeFlowDefinitionResponse' {humanLoopActivationConfig} -> humanLoopActivationConfig) (\s@DescribeFlowDefinitionResponse' {} a -> s {humanLoopActivationConfig = a} :: DescribeFlowDefinitionResponse)

-- | Container for configuring the source of human task requests. Used to
-- specify if Amazon Rekognition or Amazon Textract is used as an
-- integration source.
describeFlowDefinitionResponse_humanLoopRequestSource :: Lens.Lens' DescribeFlowDefinitionResponse (Prelude.Maybe HumanLoopRequestSource)
describeFlowDefinitionResponse_humanLoopRequestSource = Lens.lens (\DescribeFlowDefinitionResponse' {humanLoopRequestSource} -> humanLoopRequestSource) (\s@DescribeFlowDefinitionResponse' {} a -> s {humanLoopRequestSource = a} :: DescribeFlowDefinitionResponse)

-- | The reason your flow definition failed.
describeFlowDefinitionResponse_failureReason :: Lens.Lens' DescribeFlowDefinitionResponse (Prelude.Maybe Prelude.Text)
describeFlowDefinitionResponse_failureReason = Lens.lens (\DescribeFlowDefinitionResponse' {failureReason} -> failureReason) (\s@DescribeFlowDefinitionResponse' {} a -> s {failureReason = a} :: DescribeFlowDefinitionResponse)

-- | The response's http status code.
describeFlowDefinitionResponse_httpStatus :: Lens.Lens' DescribeFlowDefinitionResponse Prelude.Int
describeFlowDefinitionResponse_httpStatus = Lens.lens (\DescribeFlowDefinitionResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowDefinitionResponse' {} a -> s {httpStatus = a} :: DescribeFlowDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the flow defintion.
describeFlowDefinitionResponse_flowDefinitionArn :: Lens.Lens' DescribeFlowDefinitionResponse Prelude.Text
describeFlowDefinitionResponse_flowDefinitionArn = Lens.lens (\DescribeFlowDefinitionResponse' {flowDefinitionArn} -> flowDefinitionArn) (\s@DescribeFlowDefinitionResponse' {} a -> s {flowDefinitionArn = a} :: DescribeFlowDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the flow definition.
describeFlowDefinitionResponse_flowDefinitionName :: Lens.Lens' DescribeFlowDefinitionResponse Prelude.Text
describeFlowDefinitionResponse_flowDefinitionName = Lens.lens (\DescribeFlowDefinitionResponse' {flowDefinitionName} -> flowDefinitionName) (\s@DescribeFlowDefinitionResponse' {} a -> s {flowDefinitionName = a} :: DescribeFlowDefinitionResponse)

-- | The status of the flow definition. Valid values are listed below.
describeFlowDefinitionResponse_flowDefinitionStatus :: Lens.Lens' DescribeFlowDefinitionResponse FlowDefinitionStatus
describeFlowDefinitionResponse_flowDefinitionStatus = Lens.lens (\DescribeFlowDefinitionResponse' {flowDefinitionStatus} -> flowDefinitionStatus) (\s@DescribeFlowDefinitionResponse' {} a -> s {flowDefinitionStatus = a} :: DescribeFlowDefinitionResponse)

-- | The timestamp when the flow definition was created.
describeFlowDefinitionResponse_creationTime :: Lens.Lens' DescribeFlowDefinitionResponse Prelude.UTCTime
describeFlowDefinitionResponse_creationTime = Lens.lens (\DescribeFlowDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeFlowDefinitionResponse' {} a -> s {creationTime = a} :: DescribeFlowDefinitionResponse) Prelude.. Data._Time

-- | An object containing information about who works on the task, the
-- workforce task price, and other task details.
describeFlowDefinitionResponse_humanLoopConfig :: Lens.Lens' DescribeFlowDefinitionResponse HumanLoopConfig
describeFlowDefinitionResponse_humanLoopConfig = Lens.lens (\DescribeFlowDefinitionResponse' {humanLoopConfig} -> humanLoopConfig) (\s@DescribeFlowDefinitionResponse' {} a -> s {humanLoopConfig = a} :: DescribeFlowDefinitionResponse)

-- | An object containing information about the output file.
describeFlowDefinitionResponse_outputConfig :: Lens.Lens' DescribeFlowDefinitionResponse FlowDefinitionOutputConfig
describeFlowDefinitionResponse_outputConfig = Lens.lens (\DescribeFlowDefinitionResponse' {outputConfig} -> outputConfig) (\s@DescribeFlowDefinitionResponse' {} a -> s {outputConfig = a} :: DescribeFlowDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) execution role for the flow definition.
describeFlowDefinitionResponse_roleArn :: Lens.Lens' DescribeFlowDefinitionResponse Prelude.Text
describeFlowDefinitionResponse_roleArn = Lens.lens (\DescribeFlowDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeFlowDefinitionResponse' {} a -> s {roleArn = a} :: DescribeFlowDefinitionResponse)

instance
  Prelude.NFData
    DescribeFlowDefinitionResponse
  where
  rnf DescribeFlowDefinitionResponse' {..} =
    Prelude.rnf humanLoopActivationConfig
      `Prelude.seq` Prelude.rnf humanLoopRequestSource
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf flowDefinitionArn
      `Prelude.seq` Prelude.rnf flowDefinitionName
      `Prelude.seq` Prelude.rnf flowDefinitionStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf humanLoopConfig
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf roleArn
