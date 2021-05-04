{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.DescribeAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified algorithm that is in your
-- account.
module Network.AWS.SageMaker.DescribeAlgorithm
  ( -- * Creating a Request
    DescribeAlgorithm (..),
    newDescribeAlgorithm,

    -- * Request Lenses
    describeAlgorithm_algorithmName,

    -- * Destructuring the Response
    DescribeAlgorithmResponse (..),
    newDescribeAlgorithmResponse,

    -- * Response Lenses
    describeAlgorithmResponse_algorithmDescription,
    describeAlgorithmResponse_validationSpecification,
    describeAlgorithmResponse_certifyForMarketplace,
    describeAlgorithmResponse_productId,
    describeAlgorithmResponse_inferenceSpecification,
    describeAlgorithmResponse_httpStatus,
    describeAlgorithmResponse_algorithmName,
    describeAlgorithmResponse_algorithmArn,
    describeAlgorithmResponse_creationTime,
    describeAlgorithmResponse_trainingSpecification,
    describeAlgorithmResponse_algorithmStatus,
    describeAlgorithmResponse_algorithmStatusDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeAlgorithm' smart constructor.
data DescribeAlgorithm = DescribeAlgorithm'
  { -- | The name of the algorithm to describe.
    algorithmName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlgorithm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'describeAlgorithm_algorithmName' - The name of the algorithm to describe.
newDescribeAlgorithm ::
  -- | 'algorithmName'
  Prelude.Text ->
  DescribeAlgorithm
newDescribeAlgorithm pAlgorithmName_ =
  DescribeAlgorithm' {algorithmName = pAlgorithmName_}

-- | The name of the algorithm to describe.
describeAlgorithm_algorithmName :: Lens.Lens' DescribeAlgorithm Prelude.Text
describeAlgorithm_algorithmName = Lens.lens (\DescribeAlgorithm' {algorithmName} -> algorithmName) (\s@DescribeAlgorithm' {} a -> s {algorithmName = a} :: DescribeAlgorithm)

instance Prelude.AWSRequest DescribeAlgorithm where
  type Rs DescribeAlgorithm = DescribeAlgorithmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAlgorithmResponse'
            Prelude.<$> (x Prelude..?> "AlgorithmDescription")
            Prelude.<*> (x Prelude..?> "ValidationSpecification")
            Prelude.<*> (x Prelude..?> "CertifyForMarketplace")
            Prelude.<*> (x Prelude..?> "ProductId")
            Prelude.<*> (x Prelude..?> "InferenceSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "AlgorithmName")
            Prelude.<*> (x Prelude..:> "AlgorithmArn")
            Prelude.<*> (x Prelude..:> "CreationTime")
            Prelude.<*> (x Prelude..:> "TrainingSpecification")
            Prelude.<*> (x Prelude..:> "AlgorithmStatus")
            Prelude.<*> (x Prelude..:> "AlgorithmStatusDetails")
      )

instance Prelude.Hashable DescribeAlgorithm

instance Prelude.NFData DescribeAlgorithm

instance Prelude.ToHeaders DescribeAlgorithm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeAlgorithm" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeAlgorithm where
  toJSON DescribeAlgorithm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AlgorithmName" Prelude..= algorithmName)
          ]
      )

instance Prelude.ToPath DescribeAlgorithm where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAlgorithm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'
  { -- | A brief summary about the algorithm.
    algorithmDescription :: Prelude.Maybe Prelude.Text,
    -- | Details about configurations for one or more training jobs that Amazon
    -- SageMaker runs to test the algorithm.
    validationSpecification :: Prelude.Maybe AlgorithmValidationSpecification,
    -- | Whether the algorithm is certified to be listed in AWS Marketplace.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | The product identifier of the algorithm.
    productId :: Prelude.Maybe Prelude.Text,
    -- | Details about inference jobs that the algorithm runs.
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the algorithm being described.
    algorithmName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the algorithm.
    algorithmArn :: Prelude.Text,
    -- | A timestamp specifying when the algorithm was created.
    creationTime :: Prelude.POSIX,
    -- | Details about training jobs run by this algorithm.
    trainingSpecification :: TrainingSpecification,
    -- | The current status of the algorithm.
    algorithmStatus :: AlgorithmStatus,
    -- | Details about the current status of the algorithm.
    algorithmStatusDetails :: AlgorithmStatusDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlgorithmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmDescription', 'describeAlgorithmResponse_algorithmDescription' - A brief summary about the algorithm.
--
-- 'validationSpecification', 'describeAlgorithmResponse_validationSpecification' - Details about configurations for one or more training jobs that Amazon
-- SageMaker runs to test the algorithm.
--
-- 'certifyForMarketplace', 'describeAlgorithmResponse_certifyForMarketplace' - Whether the algorithm is certified to be listed in AWS Marketplace.
--
-- 'productId', 'describeAlgorithmResponse_productId' - The product identifier of the algorithm.
--
-- 'inferenceSpecification', 'describeAlgorithmResponse_inferenceSpecification' - Details about inference jobs that the algorithm runs.
--
-- 'httpStatus', 'describeAlgorithmResponse_httpStatus' - The response's http status code.
--
-- 'algorithmName', 'describeAlgorithmResponse_algorithmName' - The name of the algorithm being described.
--
-- 'algorithmArn', 'describeAlgorithmResponse_algorithmArn' - The Amazon Resource Name (ARN) of the algorithm.
--
-- 'creationTime', 'describeAlgorithmResponse_creationTime' - A timestamp specifying when the algorithm was created.
--
-- 'trainingSpecification', 'describeAlgorithmResponse_trainingSpecification' - Details about training jobs run by this algorithm.
--
-- 'algorithmStatus', 'describeAlgorithmResponse_algorithmStatus' - The current status of the algorithm.
--
-- 'algorithmStatusDetails', 'describeAlgorithmResponse_algorithmStatusDetails' - Details about the current status of the algorithm.
newDescribeAlgorithmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'algorithmName'
  Prelude.Text ->
  -- | 'algorithmArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'trainingSpecification'
  TrainingSpecification ->
  -- | 'algorithmStatus'
  AlgorithmStatus ->
  -- | 'algorithmStatusDetails'
  AlgorithmStatusDetails ->
  DescribeAlgorithmResponse
newDescribeAlgorithmResponse
  pHttpStatus_
  pAlgorithmName_
  pAlgorithmArn_
  pCreationTime_
  pTrainingSpecification_
  pAlgorithmStatus_
  pAlgorithmStatusDetails_ =
    DescribeAlgorithmResponse'
      { algorithmDescription =
          Prelude.Nothing,
        validationSpecification = Prelude.Nothing,
        certifyForMarketplace = Prelude.Nothing,
        productId = Prelude.Nothing,
        inferenceSpecification = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        algorithmName = pAlgorithmName_,
        algorithmArn = pAlgorithmArn_,
        creationTime =
          Prelude._Time Lens.# pCreationTime_,
        trainingSpecification = pTrainingSpecification_,
        algorithmStatus = pAlgorithmStatus_,
        algorithmStatusDetails =
          pAlgorithmStatusDetails_
      }

-- | A brief summary about the algorithm.
describeAlgorithmResponse_algorithmDescription :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe Prelude.Text)
describeAlgorithmResponse_algorithmDescription = Lens.lens (\DescribeAlgorithmResponse' {algorithmDescription} -> algorithmDescription) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmDescription = a} :: DescribeAlgorithmResponse)

-- | Details about configurations for one or more training jobs that Amazon
-- SageMaker runs to test the algorithm.
describeAlgorithmResponse_validationSpecification :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe AlgorithmValidationSpecification)
describeAlgorithmResponse_validationSpecification = Lens.lens (\DescribeAlgorithmResponse' {validationSpecification} -> validationSpecification) (\s@DescribeAlgorithmResponse' {} a -> s {validationSpecification = a} :: DescribeAlgorithmResponse)

-- | Whether the algorithm is certified to be listed in AWS Marketplace.
describeAlgorithmResponse_certifyForMarketplace :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe Prelude.Bool)
describeAlgorithmResponse_certifyForMarketplace = Lens.lens (\DescribeAlgorithmResponse' {certifyForMarketplace} -> certifyForMarketplace) (\s@DescribeAlgorithmResponse' {} a -> s {certifyForMarketplace = a} :: DescribeAlgorithmResponse)

-- | The product identifier of the algorithm.
describeAlgorithmResponse_productId :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe Prelude.Text)
describeAlgorithmResponse_productId = Lens.lens (\DescribeAlgorithmResponse' {productId} -> productId) (\s@DescribeAlgorithmResponse' {} a -> s {productId = a} :: DescribeAlgorithmResponse)

-- | Details about inference jobs that the algorithm runs.
describeAlgorithmResponse_inferenceSpecification :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe InferenceSpecification)
describeAlgorithmResponse_inferenceSpecification = Lens.lens (\DescribeAlgorithmResponse' {inferenceSpecification} -> inferenceSpecification) (\s@DescribeAlgorithmResponse' {} a -> s {inferenceSpecification = a} :: DescribeAlgorithmResponse)

-- | The response's http status code.
describeAlgorithmResponse_httpStatus :: Lens.Lens' DescribeAlgorithmResponse Prelude.Int
describeAlgorithmResponse_httpStatus = Lens.lens (\DescribeAlgorithmResponse' {httpStatus} -> httpStatus) (\s@DescribeAlgorithmResponse' {} a -> s {httpStatus = a} :: DescribeAlgorithmResponse)

-- | The name of the algorithm being described.
describeAlgorithmResponse_algorithmName :: Lens.Lens' DescribeAlgorithmResponse Prelude.Text
describeAlgorithmResponse_algorithmName = Lens.lens (\DescribeAlgorithmResponse' {algorithmName} -> algorithmName) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmName = a} :: DescribeAlgorithmResponse)

-- | The Amazon Resource Name (ARN) of the algorithm.
describeAlgorithmResponse_algorithmArn :: Lens.Lens' DescribeAlgorithmResponse Prelude.Text
describeAlgorithmResponse_algorithmArn = Lens.lens (\DescribeAlgorithmResponse' {algorithmArn} -> algorithmArn) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmArn = a} :: DescribeAlgorithmResponse)

-- | A timestamp specifying when the algorithm was created.
describeAlgorithmResponse_creationTime :: Lens.Lens' DescribeAlgorithmResponse Prelude.UTCTime
describeAlgorithmResponse_creationTime = Lens.lens (\DescribeAlgorithmResponse' {creationTime} -> creationTime) (\s@DescribeAlgorithmResponse' {} a -> s {creationTime = a} :: DescribeAlgorithmResponse) Prelude.. Prelude._Time

-- | Details about training jobs run by this algorithm.
describeAlgorithmResponse_trainingSpecification :: Lens.Lens' DescribeAlgorithmResponse TrainingSpecification
describeAlgorithmResponse_trainingSpecification = Lens.lens (\DescribeAlgorithmResponse' {trainingSpecification} -> trainingSpecification) (\s@DescribeAlgorithmResponse' {} a -> s {trainingSpecification = a} :: DescribeAlgorithmResponse)

-- | The current status of the algorithm.
describeAlgorithmResponse_algorithmStatus :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatus
describeAlgorithmResponse_algorithmStatus = Lens.lens (\DescribeAlgorithmResponse' {algorithmStatus} -> algorithmStatus) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmStatus = a} :: DescribeAlgorithmResponse)

-- | Details about the current status of the algorithm.
describeAlgorithmResponse_algorithmStatusDetails :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
describeAlgorithmResponse_algorithmStatusDetails = Lens.lens (\DescribeAlgorithmResponse' {algorithmStatusDetails} -> algorithmStatusDetails) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmStatusDetails = a} :: DescribeAlgorithmResponse)

instance Prelude.NFData DescribeAlgorithmResponse
