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
-- Module      : Network.AWS.SageMaker.DescribeModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a model that you created using the @CreateModel@ API.
module Network.AWS.SageMaker.DescribeModel
  ( -- * Creating a Request
    DescribeModel (..),
    newDescribeModel,

    -- * Request Lenses
    describeModel_modelName,

    -- * Destructuring the Response
    DescribeModelResponse (..),
    newDescribeModelResponse,

    -- * Response Lenses
    describeModelResponse_vpcConfig,
    describeModelResponse_primaryContainer,
    describeModelResponse_enableNetworkIsolation,
    describeModelResponse_containers,
    describeModelResponse_inferenceExecutionConfig,
    describeModelResponse_httpStatus,
    describeModelResponse_modelName,
    describeModelResponse_executionRoleArn,
    describeModelResponse_creationTime,
    describeModelResponse_modelArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModel' smart constructor.
data DescribeModel = DescribeModel'
  { -- | The name of the model.
    modelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'describeModel_modelName' - The name of the model.
newDescribeModel ::
  -- | 'modelName'
  Core.Text ->
  DescribeModel
newDescribeModel pModelName_ =
  DescribeModel' {modelName = pModelName_}

-- | The name of the model.
describeModel_modelName :: Lens.Lens' DescribeModel Core.Text
describeModel_modelName = Lens.lens (\DescribeModel' {modelName} -> modelName) (\s@DescribeModel' {} a -> s {modelName = a} :: DescribeModel)

instance Core.AWSRequest DescribeModel where
  type
    AWSResponse DescribeModel =
      DescribeModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelResponse'
            Core.<$> (x Core..?> "VpcConfig")
            Core.<*> (x Core..?> "PrimaryContainer")
            Core.<*> (x Core..?> "EnableNetworkIsolation")
            Core.<*> (x Core..?> "Containers" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "InferenceExecutionConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ModelName")
            Core.<*> (x Core..:> "ExecutionRoleArn")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "ModelArn")
      )

instance Core.Hashable DescribeModel

instance Core.NFData DescribeModel

instance Core.ToHeaders DescribeModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeModel" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeModel where
  toJSON DescribeModel' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ModelName" Core..= modelName)]
      )

instance Core.ToPath DescribeModel where
  toPath = Core.const "/"

instance Core.ToQuery DescribeModel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { -- | A VpcConfig object that specifies the VPC that this model has access to.
    -- For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The location of the primary inference code, associated artifacts, and
    -- custom environment map that the inference code uses when it is deployed
    -- in production.
    primaryContainer :: Core.Maybe ContainerDefinition,
    -- | If @True@, no inbound or outbound network calls can be made to or from
    -- the model container.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | The containers in the inference pipeline.
    containers :: Core.Maybe [ContainerDefinition],
    -- | Specifies details of how containers in a multi-container endpoint are
    -- called.
    inferenceExecutionConfig :: Core.Maybe InferenceExecutionConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Name of the Amazon SageMaker model.
    modelName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the model.
    executionRoleArn :: Core.Text,
    -- | A timestamp that shows when the model was created.
    creationTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'describeModelResponse_vpcConfig' - A VpcConfig object that specifies the VPC that this model has access to.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
--
-- 'primaryContainer', 'describeModelResponse_primaryContainer' - The location of the primary inference code, associated artifacts, and
-- custom environment map that the inference code uses when it is deployed
-- in production.
--
-- 'enableNetworkIsolation', 'describeModelResponse_enableNetworkIsolation' - If @True@, no inbound or outbound network calls can be made to or from
-- the model container.
--
-- 'containers', 'describeModelResponse_containers' - The containers in the inference pipeline.
--
-- 'inferenceExecutionConfig', 'describeModelResponse_inferenceExecutionConfig' - Specifies details of how containers in a multi-container endpoint are
-- called.
--
-- 'httpStatus', 'describeModelResponse_httpStatus' - The response's http status code.
--
-- 'modelName', 'describeModelResponse_modelName' - Name of the Amazon SageMaker model.
--
-- 'executionRoleArn', 'describeModelResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the model.
--
-- 'creationTime', 'describeModelResponse_creationTime' - A timestamp that shows when the model was created.
--
-- 'modelArn', 'describeModelResponse_modelArn' - The Amazon Resource Name (ARN) of the model.
newDescribeModelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'modelName'
  Core.Text ->
  -- | 'executionRoleArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'modelArn'
  Core.Text ->
  DescribeModelResponse
newDescribeModelResponse
  pHttpStatus_
  pModelName_
  pExecutionRoleArn_
  pCreationTime_
  pModelArn_ =
    DescribeModelResponse'
      { vpcConfig = Core.Nothing,
        primaryContainer = Core.Nothing,
        enableNetworkIsolation = Core.Nothing,
        containers = Core.Nothing,
        inferenceExecutionConfig = Core.Nothing,
        httpStatus = pHttpStatus_,
        modelName = pModelName_,
        executionRoleArn = pExecutionRoleArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        modelArn = pModelArn_
      }

-- | A VpcConfig object that specifies the VPC that this model has access to.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
describeModelResponse_vpcConfig :: Lens.Lens' DescribeModelResponse (Core.Maybe VpcConfig)
describeModelResponse_vpcConfig = Lens.lens (\DescribeModelResponse' {vpcConfig} -> vpcConfig) (\s@DescribeModelResponse' {} a -> s {vpcConfig = a} :: DescribeModelResponse)

-- | The location of the primary inference code, associated artifacts, and
-- custom environment map that the inference code uses when it is deployed
-- in production.
describeModelResponse_primaryContainer :: Lens.Lens' DescribeModelResponse (Core.Maybe ContainerDefinition)
describeModelResponse_primaryContainer = Lens.lens (\DescribeModelResponse' {primaryContainer} -> primaryContainer) (\s@DescribeModelResponse' {} a -> s {primaryContainer = a} :: DescribeModelResponse)

-- | If @True@, no inbound or outbound network calls can be made to or from
-- the model container.
describeModelResponse_enableNetworkIsolation :: Lens.Lens' DescribeModelResponse (Core.Maybe Core.Bool)
describeModelResponse_enableNetworkIsolation = Lens.lens (\DescribeModelResponse' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@DescribeModelResponse' {} a -> s {enableNetworkIsolation = a} :: DescribeModelResponse)

-- | The containers in the inference pipeline.
describeModelResponse_containers :: Lens.Lens' DescribeModelResponse (Core.Maybe [ContainerDefinition])
describeModelResponse_containers = Lens.lens (\DescribeModelResponse' {containers} -> containers) (\s@DescribeModelResponse' {} a -> s {containers = a} :: DescribeModelResponse) Core.. Lens.mapping Lens._Coerce

-- | Specifies details of how containers in a multi-container endpoint are
-- called.
describeModelResponse_inferenceExecutionConfig :: Lens.Lens' DescribeModelResponse (Core.Maybe InferenceExecutionConfig)
describeModelResponse_inferenceExecutionConfig = Lens.lens (\DescribeModelResponse' {inferenceExecutionConfig} -> inferenceExecutionConfig) (\s@DescribeModelResponse' {} a -> s {inferenceExecutionConfig = a} :: DescribeModelResponse)

-- | The response's http status code.
describeModelResponse_httpStatus :: Lens.Lens' DescribeModelResponse Core.Int
describeModelResponse_httpStatus = Lens.lens (\DescribeModelResponse' {httpStatus} -> httpStatus) (\s@DescribeModelResponse' {} a -> s {httpStatus = a} :: DescribeModelResponse)

-- | Name of the Amazon SageMaker model.
describeModelResponse_modelName :: Lens.Lens' DescribeModelResponse Core.Text
describeModelResponse_modelName = Lens.lens (\DescribeModelResponse' {modelName} -> modelName) (\s@DescribeModelResponse' {} a -> s {modelName = a} :: DescribeModelResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the model.
describeModelResponse_executionRoleArn :: Lens.Lens' DescribeModelResponse Core.Text
describeModelResponse_executionRoleArn = Lens.lens (\DescribeModelResponse' {executionRoleArn} -> executionRoleArn) (\s@DescribeModelResponse' {} a -> s {executionRoleArn = a} :: DescribeModelResponse)

-- | A timestamp that shows when the model was created.
describeModelResponse_creationTime :: Lens.Lens' DescribeModelResponse Core.UTCTime
describeModelResponse_creationTime = Lens.lens (\DescribeModelResponse' {creationTime} -> creationTime) (\s@DescribeModelResponse' {} a -> s {creationTime = a} :: DescribeModelResponse) Core.. Core._Time

-- | The Amazon Resource Name (ARN) of the model.
describeModelResponse_modelArn :: Lens.Lens' DescribeModelResponse Core.Text
describeModelResponse_modelArn = Lens.lens (\DescribeModelResponse' {modelArn} -> modelArn) (\s@DescribeModelResponse' {} a -> s {modelArn = a} :: DescribeModelResponse)

instance Core.NFData DescribeModelResponse
