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
-- Module      : Network.AWS.SageMaker.DescribeFlowDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified flow definition.
module Network.AWS.SageMaker.DescribeFlowDefinition
  ( -- * Creating a Request
    DescribeFlowDefinition (..),
    newDescribeFlowDefinition,

    -- * Request Lenses
    describeFlowDefinition_flowDefinitionName,

    -- * Destructuring the Response
    DescribeFlowDefinitionResponse (..),
    newDescribeFlowDefinitionResponse,

    -- * Response Lenses
    describeFlowDefinitionResponse_humanLoopRequestSource,
    describeFlowDefinitionResponse_failureReason,
    describeFlowDefinitionResponse_humanLoopActivationConfig,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlowDefinitionResponse'
            Prelude.<$> (x Core..?> "HumanLoopRequestSource")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "HumanLoopActivationConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "FlowDefinitionArn")
            Prelude.<*> (x Core..:> "FlowDefinitionName")
            Prelude.<*> (x Core..:> "FlowDefinitionStatus")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "HumanLoopConfig")
            Prelude.<*> (x Core..:> "OutputConfig")
            Prelude.<*> (x Core..:> "RoleArn")
      )

instance Prelude.Hashable DescribeFlowDefinition

instance Prelude.NFData DescribeFlowDefinition

instance Core.ToHeaders DescribeFlowDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeFlowDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFlowDefinition where
  toJSON DescribeFlowDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FlowDefinitionName" Core..= flowDefinitionName)
          ]
      )

instance Core.ToPath DescribeFlowDefinition where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFlowDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'
  { -- | Container for configuring the source of human task requests. Used to
    -- specify if Amazon Rekognition or Amazon Textract is used as an
    -- integration source.
    humanLoopRequestSource :: Prelude.Maybe HumanLoopRequestSource,
    -- | The reason your flow definition failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | An object containing information about what triggers a human review
    -- workflow.
    humanLoopActivationConfig :: Prelude.Maybe HumanLoopActivationConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the flow defintion.
    flowDefinitionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionName :: Prelude.Text,
    -- | The status of the flow definition. Valid values are listed below.
    flowDefinitionStatus :: FlowDefinitionStatus,
    -- | The timestamp when the flow definition was created.
    creationTime :: Core.POSIX,
    -- | An object containing information about who works on the task, the
    -- workforce task price, and other task details.
    humanLoopConfig :: HumanLoopConfig,
    -- | An object containing information about the output file.
    outputConfig :: FlowDefinitionOutputConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) execution role for the flow definition.
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
-- 'humanLoopRequestSource', 'describeFlowDefinitionResponse_humanLoopRequestSource' - Container for configuring the source of human task requests. Used to
-- specify if Amazon Rekognition or Amazon Textract is used as an
-- integration source.
--
-- 'failureReason', 'describeFlowDefinitionResponse_failureReason' - The reason your flow definition failed.
--
-- 'humanLoopActivationConfig', 'describeFlowDefinitionResponse_humanLoopActivationConfig' - An object containing information about what triggers a human review
-- workflow.
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
-- 'roleArn', 'describeFlowDefinitionResponse_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) execution role for the flow definition.
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
      { humanLoopRequestSource =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        humanLoopActivationConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        flowDefinitionArn = pFlowDefinitionArn_,
        flowDefinitionName = pFlowDefinitionName_,
        flowDefinitionStatus =
          pFlowDefinitionStatus_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        humanLoopConfig = pHumanLoopConfig_,
        outputConfig = pOutputConfig_,
        roleArn = pRoleArn_
      }

-- | Container for configuring the source of human task requests. Used to
-- specify if Amazon Rekognition or Amazon Textract is used as an
-- integration source.
describeFlowDefinitionResponse_humanLoopRequestSource :: Lens.Lens' DescribeFlowDefinitionResponse (Prelude.Maybe HumanLoopRequestSource)
describeFlowDefinitionResponse_humanLoopRequestSource = Lens.lens (\DescribeFlowDefinitionResponse' {humanLoopRequestSource} -> humanLoopRequestSource) (\s@DescribeFlowDefinitionResponse' {} a -> s {humanLoopRequestSource = a} :: DescribeFlowDefinitionResponse)

-- | The reason your flow definition failed.
describeFlowDefinitionResponse_failureReason :: Lens.Lens' DescribeFlowDefinitionResponse (Prelude.Maybe Prelude.Text)
describeFlowDefinitionResponse_failureReason = Lens.lens (\DescribeFlowDefinitionResponse' {failureReason} -> failureReason) (\s@DescribeFlowDefinitionResponse' {} a -> s {failureReason = a} :: DescribeFlowDefinitionResponse)

-- | An object containing information about what triggers a human review
-- workflow.
describeFlowDefinitionResponse_humanLoopActivationConfig :: Lens.Lens' DescribeFlowDefinitionResponse (Prelude.Maybe HumanLoopActivationConfig)
describeFlowDefinitionResponse_humanLoopActivationConfig = Lens.lens (\DescribeFlowDefinitionResponse' {humanLoopActivationConfig} -> humanLoopActivationConfig) (\s@DescribeFlowDefinitionResponse' {} a -> s {humanLoopActivationConfig = a} :: DescribeFlowDefinitionResponse)

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
describeFlowDefinitionResponse_creationTime = Lens.lens (\DescribeFlowDefinitionResponse' {creationTime} -> creationTime) (\s@DescribeFlowDefinitionResponse' {} a -> s {creationTime = a} :: DescribeFlowDefinitionResponse) Prelude.. Core._Time

-- | An object containing information about who works on the task, the
-- workforce task price, and other task details.
describeFlowDefinitionResponse_humanLoopConfig :: Lens.Lens' DescribeFlowDefinitionResponse HumanLoopConfig
describeFlowDefinitionResponse_humanLoopConfig = Lens.lens (\DescribeFlowDefinitionResponse' {humanLoopConfig} -> humanLoopConfig) (\s@DescribeFlowDefinitionResponse' {} a -> s {humanLoopConfig = a} :: DescribeFlowDefinitionResponse)

-- | An object containing information about the output file.
describeFlowDefinitionResponse_outputConfig :: Lens.Lens' DescribeFlowDefinitionResponse FlowDefinitionOutputConfig
describeFlowDefinitionResponse_outputConfig = Lens.lens (\DescribeFlowDefinitionResponse' {outputConfig} -> outputConfig) (\s@DescribeFlowDefinitionResponse' {} a -> s {outputConfig = a} :: DescribeFlowDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) execution role for the flow definition.
describeFlowDefinitionResponse_roleArn :: Lens.Lens' DescribeFlowDefinitionResponse Prelude.Text
describeFlowDefinitionResponse_roleArn = Lens.lens (\DescribeFlowDefinitionResponse' {roleArn} -> roleArn) (\s@DescribeFlowDefinitionResponse' {} a -> s {roleArn = a} :: DescribeFlowDefinitionResponse)

instance
  Prelude.NFData
    DescribeFlowDefinitionResponse
