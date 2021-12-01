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
-- Module      : Amazonka.SageMaker.CreateAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a machine learning algorithm that you can use in Amazon SageMaker
-- and list in the Amazon Web Services Marketplace.
module Amazonka.SageMaker.CreateAlgorithm
  ( -- * Creating a Request
    CreateAlgorithm (..),
    newCreateAlgorithm,

    -- * Request Lenses
    createAlgorithm_validationSpecification,
    createAlgorithm_inferenceSpecification,
    createAlgorithm_algorithmDescription,
    createAlgorithm_certifyForMarketplace,
    createAlgorithm_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateAlgorithm' smart constructor.
data CreateAlgorithm = CreateAlgorithm'
  { -- | Specifies configurations for one or more training jobs and that Amazon
    -- SageMaker runs to test the algorithm\'s training code and, optionally,
    -- one or more batch transform jobs that Amazon SageMaker runs to test the
    -- algorithm\'s inference code.
    validationSpecification :: Prelude.Maybe AlgorithmValidationSpecification,
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
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | A description of the algorithm.
    algorithmDescription :: Prelude.Maybe Prelude.Text,
    -- | Whether to certify the algorithm so that it can be listed in Amazon Web
    -- Services Marketplace.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the algorithm.
    algorithmName :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlgorithm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationSpecification', 'createAlgorithm_validationSpecification' - Specifies configurations for one or more training jobs and that Amazon
-- SageMaker runs to test the algorithm\'s training code and, optionally,
-- one or more batch transform jobs that Amazon SageMaker runs to test the
-- algorithm\'s inference code.
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
-- 'algorithmDescription', 'createAlgorithm_algorithmDescription' - A description of the algorithm.
--
-- 'certifyForMarketplace', 'createAlgorithm_certifyForMarketplace' - Whether to certify the algorithm so that it can be listed in Amazon Web
-- Services Marketplace.
--
-- 'tags', 'createAlgorithm_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
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
  Prelude.Text ->
  -- | 'trainingSpecification'
  TrainingSpecification ->
  CreateAlgorithm
newCreateAlgorithm
  pAlgorithmName_
  pTrainingSpecification_ =
    CreateAlgorithm'
      { validationSpecification =
          Prelude.Nothing,
        inferenceSpecification = Prelude.Nothing,
        algorithmDescription = Prelude.Nothing,
        certifyForMarketplace = Prelude.Nothing,
        tags = Prelude.Nothing,
        algorithmName = pAlgorithmName_,
        trainingSpecification = pTrainingSpecification_
      }

-- | Specifies configurations for one or more training jobs and that Amazon
-- SageMaker runs to test the algorithm\'s training code and, optionally,
-- one or more batch transform jobs that Amazon SageMaker runs to test the
-- algorithm\'s inference code.
createAlgorithm_validationSpecification :: Lens.Lens' CreateAlgorithm (Prelude.Maybe AlgorithmValidationSpecification)
createAlgorithm_validationSpecification = Lens.lens (\CreateAlgorithm' {validationSpecification} -> validationSpecification) (\s@CreateAlgorithm' {} a -> s {validationSpecification = a} :: CreateAlgorithm)

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
createAlgorithm_inferenceSpecification :: Lens.Lens' CreateAlgorithm (Prelude.Maybe InferenceSpecification)
createAlgorithm_inferenceSpecification = Lens.lens (\CreateAlgorithm' {inferenceSpecification} -> inferenceSpecification) (\s@CreateAlgorithm' {} a -> s {inferenceSpecification = a} :: CreateAlgorithm)

-- | A description of the algorithm.
createAlgorithm_algorithmDescription :: Lens.Lens' CreateAlgorithm (Prelude.Maybe Prelude.Text)
createAlgorithm_algorithmDescription = Lens.lens (\CreateAlgorithm' {algorithmDescription} -> algorithmDescription) (\s@CreateAlgorithm' {} a -> s {algorithmDescription = a} :: CreateAlgorithm)

-- | Whether to certify the algorithm so that it can be listed in Amazon Web
-- Services Marketplace.
createAlgorithm_certifyForMarketplace :: Lens.Lens' CreateAlgorithm (Prelude.Maybe Prelude.Bool)
createAlgorithm_certifyForMarketplace = Lens.lens (\CreateAlgorithm' {certifyForMarketplace} -> certifyForMarketplace) (\s@CreateAlgorithm' {} a -> s {certifyForMarketplace = a} :: CreateAlgorithm)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createAlgorithm_tags :: Lens.Lens' CreateAlgorithm (Prelude.Maybe [Tag])
createAlgorithm_tags = Lens.lens (\CreateAlgorithm' {tags} -> tags) (\s@CreateAlgorithm' {} a -> s {tags = a} :: CreateAlgorithm) Prelude.. Lens.mapping Lens.coerced

-- | The name of the algorithm.
createAlgorithm_algorithmName :: Lens.Lens' CreateAlgorithm Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AlgorithmArn")
      )

instance Prelude.Hashable CreateAlgorithm where
  hashWithSalt salt' CreateAlgorithm' {..} =
    salt' `Prelude.hashWithSalt` trainingSpecification
      `Prelude.hashWithSalt` algorithmName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` certifyForMarketplace
      `Prelude.hashWithSalt` algorithmDescription
      `Prelude.hashWithSalt` inferenceSpecification
      `Prelude.hashWithSalt` validationSpecification

instance Prelude.NFData CreateAlgorithm where
  rnf CreateAlgorithm' {..} =
    Prelude.rnf validationSpecification
      `Prelude.seq` Prelude.rnf trainingSpecification
      `Prelude.seq` Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf certifyForMarketplace
      `Prelude.seq` Prelude.rnf algorithmDescription
      `Prelude.seq` Prelude.rnf inferenceSpecification

instance Core.ToHeaders CreateAlgorithm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateAlgorithm" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAlgorithm where
  toJSON CreateAlgorithm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ValidationSpecification" Core..=)
              Prelude.<$> validationSpecification,
            ("InferenceSpecification" Core..=)
              Prelude.<$> inferenceSpecification,
            ("AlgorithmDescription" Core..=)
              Prelude.<$> algorithmDescription,
            ("CertifyForMarketplace" Core..=)
              Prelude.<$> certifyForMarketplace,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("AlgorithmName" Core..= algorithmName),
            Prelude.Just
              ( "TrainingSpecification"
                  Core..= trainingSpecification
              )
          ]
      )

instance Core.ToPath CreateAlgorithm where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAlgorithm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAlgorithmResponse' smart constructor.
data CreateAlgorithmResponse = CreateAlgorithmResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the new algorithm.
    algorithmArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'algorithmArn'
  Prelude.Text ->
  CreateAlgorithmResponse
newCreateAlgorithmResponse
  pHttpStatus_
  pAlgorithmArn_ =
    CreateAlgorithmResponse'
      { httpStatus = pHttpStatus_,
        algorithmArn = pAlgorithmArn_
      }

-- | The response's http status code.
createAlgorithmResponse_httpStatus :: Lens.Lens' CreateAlgorithmResponse Prelude.Int
createAlgorithmResponse_httpStatus = Lens.lens (\CreateAlgorithmResponse' {httpStatus} -> httpStatus) (\s@CreateAlgorithmResponse' {} a -> s {httpStatus = a} :: CreateAlgorithmResponse)

-- | The Amazon Resource Name (ARN) of the new algorithm.
createAlgorithmResponse_algorithmArn :: Lens.Lens' CreateAlgorithmResponse Prelude.Text
createAlgorithmResponse_algorithmArn = Lens.lens (\CreateAlgorithmResponse' {algorithmArn} -> algorithmArn) (\s@CreateAlgorithmResponse' {} a -> s {algorithmArn = a} :: CreateAlgorithmResponse)

instance Prelude.NFData CreateAlgorithmResponse where
  rnf CreateAlgorithmResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf algorithmArn
