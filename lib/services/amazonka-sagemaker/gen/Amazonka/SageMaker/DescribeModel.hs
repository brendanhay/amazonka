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
-- Module      : Amazonka.SageMaker.DescribeModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a model that you created using the @CreateModel@ API.
module Amazonka.SageMaker.DescribeModel
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
    describeModelResponse_enableNetworkIsolation,
    describeModelResponse_containers,
    describeModelResponse_primaryContainer,
    describeModelResponse_inferenceExecutionConfig,
    describeModelResponse_httpStatus,
    describeModelResponse_modelName,
    describeModelResponse_executionRoleArn,
    describeModelResponse_creationTime,
    describeModelResponse_modelArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeModel' smart constructor.
data DescribeModel = DescribeModel'
  { -- | The name of the model.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeModel
newDescribeModel pModelName_ =
  DescribeModel' {modelName = pModelName_}

-- | The name of the model.
describeModel_modelName :: Lens.Lens' DescribeModel Prelude.Text
describeModel_modelName = Lens.lens (\DescribeModel' {modelName} -> modelName) (\s@DescribeModel' {} a -> s {modelName = a} :: DescribeModel)

instance Core.AWSRequest DescribeModel where
  type
    AWSResponse DescribeModel =
      DescribeModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelResponse'
            Prelude.<$> (x Core..?> "VpcConfig")
            Prelude.<*> (x Core..?> "EnableNetworkIsolation")
            Prelude.<*> (x Core..?> "Containers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "PrimaryContainer")
            Prelude.<*> (x Core..?> "InferenceExecutionConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ModelName")
            Prelude.<*> (x Core..:> "ExecutionRoleArn")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "ModelArn")
      )

instance Prelude.Hashable DescribeModel where
  hashWithSalt _salt DescribeModel' {..} =
    _salt `Prelude.hashWithSalt` modelName

instance Prelude.NFData DescribeModel where
  rnf DescribeModel' {..} = Prelude.rnf modelName

instance Core.ToHeaders DescribeModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeModel" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeModel where
  toJSON DescribeModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ModelName" Core..= modelName)]
      )

instance Core.ToPath DescribeModel where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { -- | A VpcConfig object that specifies the VPC that this model has access to.
    -- For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | If @True@, no inbound or outbound network calls can be made to or from
    -- the model container.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | The containers in the inference pipeline.
    containers :: Prelude.Maybe [ContainerDefinition],
    -- | The location of the primary inference code, associated artifacts, and
    -- custom environment map that the inference code uses when it is deployed
    -- in production.
    primaryContainer :: Prelude.Maybe ContainerDefinition,
    -- | Specifies details of how containers in a multi-container endpoint are
    -- called.
    inferenceExecutionConfig :: Prelude.Maybe InferenceExecutionConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Name of the SageMaker model.
    modelName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the model.
    executionRoleArn :: Prelude.Text,
    -- | A timestamp that shows when the model was created.
    creationTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'enableNetworkIsolation', 'describeModelResponse_enableNetworkIsolation' - If @True@, no inbound or outbound network calls can be made to or from
-- the model container.
--
-- 'containers', 'describeModelResponse_containers' - The containers in the inference pipeline.
--
-- 'primaryContainer', 'describeModelResponse_primaryContainer' - The location of the primary inference code, associated artifacts, and
-- custom environment map that the inference code uses when it is deployed
-- in production.
--
-- 'inferenceExecutionConfig', 'describeModelResponse_inferenceExecutionConfig' - Specifies details of how containers in a multi-container endpoint are
-- called.
--
-- 'httpStatus', 'describeModelResponse_httpStatus' - The response's http status code.
--
-- 'modelName', 'describeModelResponse_modelName' - Name of the SageMaker model.
--
-- 'executionRoleArn', 'describeModelResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the model.
--
-- 'creationTime', 'describeModelResponse_creationTime' - A timestamp that shows when the model was created.
--
-- 'modelArn', 'describeModelResponse_modelArn' - The Amazon Resource Name (ARN) of the model.
newDescribeModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelName'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'modelArn'
  Prelude.Text ->
  DescribeModelResponse
newDescribeModelResponse
  pHttpStatus_
  pModelName_
  pExecutionRoleArn_
  pCreationTime_
  pModelArn_ =
    DescribeModelResponse'
      { vpcConfig = Prelude.Nothing,
        enableNetworkIsolation = Prelude.Nothing,
        containers = Prelude.Nothing,
        primaryContainer = Prelude.Nothing,
        inferenceExecutionConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        modelName = pModelName_,
        executionRoleArn = pExecutionRoleArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        modelArn = pModelArn_
      }

-- | A VpcConfig object that specifies the VPC that this model has access to.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/host-vpc.html Protect Endpoints by Using an Amazon Virtual Private Cloud>
describeModelResponse_vpcConfig :: Lens.Lens' DescribeModelResponse (Prelude.Maybe VpcConfig)
describeModelResponse_vpcConfig = Lens.lens (\DescribeModelResponse' {vpcConfig} -> vpcConfig) (\s@DescribeModelResponse' {} a -> s {vpcConfig = a} :: DescribeModelResponse)

-- | If @True@, no inbound or outbound network calls can be made to or from
-- the model container.
describeModelResponse_enableNetworkIsolation :: Lens.Lens' DescribeModelResponse (Prelude.Maybe Prelude.Bool)
describeModelResponse_enableNetworkIsolation = Lens.lens (\DescribeModelResponse' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@DescribeModelResponse' {} a -> s {enableNetworkIsolation = a} :: DescribeModelResponse)

-- | The containers in the inference pipeline.
describeModelResponse_containers :: Lens.Lens' DescribeModelResponse (Prelude.Maybe [ContainerDefinition])
describeModelResponse_containers = Lens.lens (\DescribeModelResponse' {containers} -> containers) (\s@DescribeModelResponse' {} a -> s {containers = a} :: DescribeModelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The location of the primary inference code, associated artifacts, and
-- custom environment map that the inference code uses when it is deployed
-- in production.
describeModelResponse_primaryContainer :: Lens.Lens' DescribeModelResponse (Prelude.Maybe ContainerDefinition)
describeModelResponse_primaryContainer = Lens.lens (\DescribeModelResponse' {primaryContainer} -> primaryContainer) (\s@DescribeModelResponse' {} a -> s {primaryContainer = a} :: DescribeModelResponse)

-- | Specifies details of how containers in a multi-container endpoint are
-- called.
describeModelResponse_inferenceExecutionConfig :: Lens.Lens' DescribeModelResponse (Prelude.Maybe InferenceExecutionConfig)
describeModelResponse_inferenceExecutionConfig = Lens.lens (\DescribeModelResponse' {inferenceExecutionConfig} -> inferenceExecutionConfig) (\s@DescribeModelResponse' {} a -> s {inferenceExecutionConfig = a} :: DescribeModelResponse)

-- | The response's http status code.
describeModelResponse_httpStatus :: Lens.Lens' DescribeModelResponse Prelude.Int
describeModelResponse_httpStatus = Lens.lens (\DescribeModelResponse' {httpStatus} -> httpStatus) (\s@DescribeModelResponse' {} a -> s {httpStatus = a} :: DescribeModelResponse)

-- | Name of the SageMaker model.
describeModelResponse_modelName :: Lens.Lens' DescribeModelResponse Prelude.Text
describeModelResponse_modelName = Lens.lens (\DescribeModelResponse' {modelName} -> modelName) (\s@DescribeModelResponse' {} a -> s {modelName = a} :: DescribeModelResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the model.
describeModelResponse_executionRoleArn :: Lens.Lens' DescribeModelResponse Prelude.Text
describeModelResponse_executionRoleArn = Lens.lens (\DescribeModelResponse' {executionRoleArn} -> executionRoleArn) (\s@DescribeModelResponse' {} a -> s {executionRoleArn = a} :: DescribeModelResponse)

-- | A timestamp that shows when the model was created.
describeModelResponse_creationTime :: Lens.Lens' DescribeModelResponse Prelude.UTCTime
describeModelResponse_creationTime = Lens.lens (\DescribeModelResponse' {creationTime} -> creationTime) (\s@DescribeModelResponse' {} a -> s {creationTime = a} :: DescribeModelResponse) Prelude.. Core._Time

-- | The Amazon Resource Name (ARN) of the model.
describeModelResponse_modelArn :: Lens.Lens' DescribeModelResponse Prelude.Text
describeModelResponse_modelArn = Lens.lens (\DescribeModelResponse' {modelArn} -> modelArn) (\s@DescribeModelResponse' {} a -> s {modelArn = a} :: DescribeModelResponse)

instance Prelude.NFData DescribeModelResponse where
  rnf DescribeModelResponse' {..} =
    Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf containers
      `Prelude.seq` Prelude.rnf primaryContainer
      `Prelude.seq` Prelude.rnf inferenceExecutionConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf modelArn
