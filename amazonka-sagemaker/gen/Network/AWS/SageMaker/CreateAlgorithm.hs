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
-- Module      : Network.AWS.SageMaker.CreateAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a machine learning algorithm that you can use in Amazon SageMaker
-- and list in the AWS Marketplace.
module Network.AWS.SageMaker.CreateAlgorithm
  ( -- * Creating a Request
    CreateAlgorithm (..),
    newCreateAlgorithm,

    -- * Request Lenses
    createAlgorithm_algorithmDescription,
    createAlgorithm_validationSpecification,
    createAlgorithm_certifyForMarketplace,
    createAlgorithm_tags,
    createAlgorithm_inferenceSpecification,
    createAlgorithm_algorithmName,
    createAlgorithm_trainingSpecification,

    -- * Destructuring the Response
    CreateAlgorithmResponse (..),
    newCreateAlgorithmResponse,

    -- * Response Lenses
    createAlgorithmResponse_httpStatus,
    createAlgorithmResponse_algorithmArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateAlgorithm' smart constructor.
data CreateAlgorithm = CreateAlgorithm'
  { -- | A description of the algorithm.
    algorithmDescription :: Core.Maybe Core.Text,
    -- | Specifies configurations for one or more training jobs and that Amazon
    -- SageMaker runs to test the algorithm\'s training code and, optionally,
    -- one or more batch transform jobs that Amazon SageMaker runs to test the
    -- algorithm\'s inference code.
    validationSpecification :: Core.Maybe AlgorithmValidationSpecification,
    -- | Whether to certify the algorithm so that it can be listed in AWS
    -- Marketplace.
    certifyForMarketplace :: Core.Maybe Core.Bool,
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Core.Maybe [Tag],
    -- | Specifies details about inference jobs that the algorithm runs,
    -- including the following:
    --
    -- -   The Amazon ECR paths of containers that contain the inference code
    --     and model artifacts.
    --
    -- -   The instance types that the algorithm supports for transform jobs
    --     and real-time endpoints used for inference.
    --
    -- -   The input and output content formats that the algorithm supports for
    --     inference.
    inferenceSpecification :: Core.Maybe InferenceSpecification,
    -- | The name of the algorithm.
    algorithmName :: Core.Text,
    -- | Specifies details about training jobs run by this algorithm, including
    -- the following:
    --
    -- -   The Amazon ECR path of the container and the version digest of the
    --     algorithm.
    --
    -- -   The hyperparameters that the algorithm supports.
    --
    -- -   The instance types that the algorithm supports for training.
    --
    -- -   Whether the algorithm supports distributed training.
    --
    -- -   The metrics that the algorithm emits to Amazon CloudWatch.
    --
    -- -   Which metrics that the algorithm emits can be used as the objective
    --     metric for hyperparameter tuning jobs.
    --
    -- -   The input channels that the algorithm supports for training data.
    --     For example, an algorithm might support @train@, @validation@, and
    --     @test@ channels.
    trainingSpecification :: TrainingSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAlgorithm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmDescription', 'createAlgorithm_algorithmDescription' - A description of the algorithm.
--
-- 'validationSpecification', 'createAlgorithm_validationSpecification' - Specifies configurations for one or more training jobs and that Amazon
-- SageMaker runs to test the algorithm\'s training code and, optionally,
-- one or more batch transform jobs that Amazon SageMaker runs to test the
-- algorithm\'s inference code.
--
-- 'certifyForMarketplace', 'createAlgorithm_certifyForMarketplace' - Whether to certify the algorithm so that it can be listed in AWS
-- Marketplace.
--
-- 'tags', 'createAlgorithm_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- 'inferenceSpecification', 'createAlgorithm_inferenceSpecification' - Specifies details about inference jobs that the algorithm runs,
-- including the following:
--
-- -   The Amazon ECR paths of containers that contain the inference code
--     and model artifacts.
--
-- -   The instance types that the algorithm supports for transform jobs
--     and real-time endpoints used for inference.
--
-- -   The input and output content formats that the algorithm supports for
--     inference.
--
-- 'algorithmName', 'createAlgorithm_algorithmName' - The name of the algorithm.
--
-- 'trainingSpecification', 'createAlgorithm_trainingSpecification' - Specifies details about training jobs run by this algorithm, including
-- the following:
--
-- -   The Amazon ECR path of the container and the version digest of the
--     algorithm.
--
-- -   The hyperparameters that the algorithm supports.
--
-- -   The instance types that the algorithm supports for training.
--
-- -   Whether the algorithm supports distributed training.
--
-- -   The metrics that the algorithm emits to Amazon CloudWatch.
--
-- -   Which metrics that the algorithm emits can be used as the objective
--     metric for hyperparameter tuning jobs.
--
-- -   The input channels that the algorithm supports for training data.
--     For example, an algorithm might support @train@, @validation@, and
--     @test@ channels.
newCreateAlgorithm ::
  -- | 'algorithmName'
  Core.Text ->
  -- | 'trainingSpecification'
  TrainingSpecification ->
  CreateAlgorithm
newCreateAlgorithm
  pAlgorithmName_
  pTrainingSpecification_ =
    CreateAlgorithm'
      { algorithmDescription =
          Core.Nothing,
        validationSpecification = Core.Nothing,
        certifyForMarketplace = Core.Nothing,
        tags = Core.Nothing,
        inferenceSpecification = Core.Nothing,
        algorithmName = pAlgorithmName_,
        trainingSpecification = pTrainingSpecification_
      }

-- | A description of the algorithm.
createAlgorithm_algorithmDescription :: Lens.Lens' CreateAlgorithm (Core.Maybe Core.Text)
createAlgorithm_algorithmDescription = Lens.lens (\CreateAlgorithm' {algorithmDescription} -> algorithmDescription) (\s@CreateAlgorithm' {} a -> s {algorithmDescription = a} :: CreateAlgorithm)

-- | Specifies configurations for one or more training jobs and that Amazon
-- SageMaker runs to test the algorithm\'s training code and, optionally,
-- one or more batch transform jobs that Amazon SageMaker runs to test the
-- algorithm\'s inference code.
createAlgorithm_validationSpecification :: Lens.Lens' CreateAlgorithm (Core.Maybe AlgorithmValidationSpecification)
createAlgorithm_validationSpecification = Lens.lens (\CreateAlgorithm' {validationSpecification} -> validationSpecification) (\s@CreateAlgorithm' {} a -> s {validationSpecification = a} :: CreateAlgorithm)

-- | Whether to certify the algorithm so that it can be listed in AWS
-- Marketplace.
createAlgorithm_certifyForMarketplace :: Lens.Lens' CreateAlgorithm (Core.Maybe Core.Bool)
createAlgorithm_certifyForMarketplace = Lens.lens (\CreateAlgorithm' {certifyForMarketplace} -> certifyForMarketplace) (\s@CreateAlgorithm' {} a -> s {certifyForMarketplace = a} :: CreateAlgorithm)

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
createAlgorithm_tags :: Lens.Lens' CreateAlgorithm (Core.Maybe [Tag])
createAlgorithm_tags = Lens.lens (\CreateAlgorithm' {tags} -> tags) (\s@CreateAlgorithm' {} a -> s {tags = a} :: CreateAlgorithm) Core.. Lens.mapping Lens._Coerce

-- | Specifies details about inference jobs that the algorithm runs,
-- including the following:
--
-- -   The Amazon ECR paths of containers that contain the inference code
--     and model artifacts.
--
-- -   The instance types that the algorithm supports for transform jobs
--     and real-time endpoints used for inference.
--
-- -   The input and output content formats that the algorithm supports for
--     inference.
createAlgorithm_inferenceSpecification :: Lens.Lens' CreateAlgorithm (Core.Maybe InferenceSpecification)
createAlgorithm_inferenceSpecification = Lens.lens (\CreateAlgorithm' {inferenceSpecification} -> inferenceSpecification) (\s@CreateAlgorithm' {} a -> s {inferenceSpecification = a} :: CreateAlgorithm)

-- | The name of the algorithm.
createAlgorithm_algorithmName :: Lens.Lens' CreateAlgorithm Core.Text
createAlgorithm_algorithmName = Lens.lens (\CreateAlgorithm' {algorithmName} -> algorithmName) (\s@CreateAlgorithm' {} a -> s {algorithmName = a} :: CreateAlgorithm)

-- | Specifies details about training jobs run by this algorithm, including
-- the following:
--
-- -   The Amazon ECR path of the container and the version digest of the
--     algorithm.
--
-- -   The hyperparameters that the algorithm supports.
--
-- -   The instance types that the algorithm supports for training.
--
-- -   Whether the algorithm supports distributed training.
--
-- -   The metrics that the algorithm emits to Amazon CloudWatch.
--
-- -   Which metrics that the algorithm emits can be used as the objective
--     metric for hyperparameter tuning jobs.
--
-- -   The input channels that the algorithm supports for training data.
--     For example, an algorithm might support @train@, @validation@, and
--     @test@ channels.
createAlgorithm_trainingSpecification :: Lens.Lens' CreateAlgorithm TrainingSpecification
createAlgorithm_trainingSpecification = Lens.lens (\CreateAlgorithm' {trainingSpecification} -> trainingSpecification) (\s@CreateAlgorithm' {} a -> s {trainingSpecification = a} :: CreateAlgorithm)

instance Core.AWSRequest CreateAlgorithm where
  type
    AWSResponse CreateAlgorithm =
      CreateAlgorithmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAlgorithmResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "AlgorithmArn")
      )

instance Core.Hashable CreateAlgorithm

instance Core.NFData CreateAlgorithm

instance Core.ToHeaders CreateAlgorithm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateAlgorithm" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateAlgorithm where
  toJSON CreateAlgorithm' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AlgorithmDescription" Core..=)
              Core.<$> algorithmDescription,
            ("ValidationSpecification" Core..=)
              Core.<$> validationSpecification,
            ("CertifyForMarketplace" Core..=)
              Core.<$> certifyForMarketplace,
            ("Tags" Core..=) Core.<$> tags,
            ("InferenceSpecification" Core..=)
              Core.<$> inferenceSpecification,
            Core.Just ("AlgorithmName" Core..= algorithmName),
            Core.Just
              ( "TrainingSpecification"
                  Core..= trainingSpecification
              )
          ]
      )

instance Core.ToPath CreateAlgorithm where
  toPath = Core.const "/"

instance Core.ToQuery CreateAlgorithm where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAlgorithmResponse' smart constructor.
data CreateAlgorithmResponse = CreateAlgorithmResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the new algorithm.
    algorithmArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAlgorithmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAlgorithmResponse_httpStatus' - The response's http status code.
--
-- 'algorithmArn', 'createAlgorithmResponse_algorithmArn' - The Amazon Resource Name (ARN) of the new algorithm.
newCreateAlgorithmResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'algorithmArn'
  Core.Text ->
  CreateAlgorithmResponse
newCreateAlgorithmResponse
  pHttpStatus_
  pAlgorithmArn_ =
    CreateAlgorithmResponse'
      { httpStatus = pHttpStatus_,
        algorithmArn = pAlgorithmArn_
      }

-- | The response's http status code.
createAlgorithmResponse_httpStatus :: Lens.Lens' CreateAlgorithmResponse Core.Int
createAlgorithmResponse_httpStatus = Lens.lens (\CreateAlgorithmResponse' {httpStatus} -> httpStatus) (\s@CreateAlgorithmResponse' {} a -> s {httpStatus = a} :: CreateAlgorithmResponse)

-- | The Amazon Resource Name (ARN) of the new algorithm.
createAlgorithmResponse_algorithmArn :: Lens.Lens' CreateAlgorithmResponse Core.Text
createAlgorithmResponse_algorithmArn = Lens.lens (\CreateAlgorithmResponse' {algorithmArn} -> algorithmArn) (\s@CreateAlgorithmResponse' {} a -> s {algorithmArn = a} :: CreateAlgorithmResponse)

instance Core.NFData CreateAlgorithmResponse
