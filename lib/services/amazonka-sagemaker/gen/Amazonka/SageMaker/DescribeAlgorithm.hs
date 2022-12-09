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
-- Module      : Amazonka.SageMaker.DescribeAlgorithm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified algorithm that is in your
-- account.
module Amazonka.SageMaker.DescribeAlgorithm
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
    describeAlgorithmResponse_certifyForMarketplace,
    describeAlgorithmResponse_inferenceSpecification,
    describeAlgorithmResponse_productId,
    describeAlgorithmResponse_validationSpecification,
    describeAlgorithmResponse_httpStatus,
    describeAlgorithmResponse_algorithmName,
    describeAlgorithmResponse_algorithmArn,
    describeAlgorithmResponse_creationTime,
    describeAlgorithmResponse_trainingSpecification,
    describeAlgorithmResponse_algorithmStatus,
    describeAlgorithmResponse_algorithmStatusDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeAlgorithm' smart constructor.
data DescribeAlgorithm = DescribeAlgorithm'
  { -- | The name of the algorithm to describe.
    algorithmName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeAlgorithm where
  type
    AWSResponse DescribeAlgorithm =
      DescribeAlgorithmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAlgorithmResponse'
            Prelude.<$> (x Data..?> "AlgorithmDescription")
            Prelude.<*> (x Data..?> "CertifyForMarketplace")
            Prelude.<*> (x Data..?> "InferenceSpecification")
            Prelude.<*> (x Data..?> "ProductId")
            Prelude.<*> (x Data..?> "ValidationSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AlgorithmName")
            Prelude.<*> (x Data..:> "AlgorithmArn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "TrainingSpecification")
            Prelude.<*> (x Data..:> "AlgorithmStatus")
            Prelude.<*> (x Data..:> "AlgorithmStatusDetails")
      )

instance Prelude.Hashable DescribeAlgorithm where
  hashWithSalt _salt DescribeAlgorithm' {..} =
    _salt `Prelude.hashWithSalt` algorithmName

instance Prelude.NFData DescribeAlgorithm where
  rnf DescribeAlgorithm' {..} =
    Prelude.rnf algorithmName

instance Data.ToHeaders DescribeAlgorithm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeAlgorithm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAlgorithm where
  toJSON DescribeAlgorithm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AlgorithmName" Data..= algorithmName)
          ]
      )

instance Data.ToPath DescribeAlgorithm where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAlgorithm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'
  { -- | A brief summary about the algorithm.
    algorithmDescription :: Prelude.Maybe Prelude.Text,
    -- | Whether the algorithm is certified to be listed in Amazon Web Services
    -- Marketplace.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | Details about inference jobs that the algorithm runs.
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | The product identifier of the algorithm.
    productId :: Prelude.Maybe Prelude.Text,
    -- | Details about configurations for one or more training jobs that
    -- SageMaker runs to test the algorithm.
    validationSpecification :: Prelude.Maybe AlgorithmValidationSpecification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the algorithm being described.
    algorithmName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the algorithm.
    algorithmArn :: Prelude.Text,
    -- | A timestamp specifying when the algorithm was created.
    creationTime :: Data.POSIX,
    -- | Details about training jobs run by this algorithm.
    trainingSpecification :: TrainingSpecification,
    -- | The current status of the algorithm.
    algorithmStatus :: AlgorithmStatus,
    -- | Details about the current status of the algorithm.
    algorithmStatusDetails :: AlgorithmStatusDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'certifyForMarketplace', 'describeAlgorithmResponse_certifyForMarketplace' - Whether the algorithm is certified to be listed in Amazon Web Services
-- Marketplace.
--
-- 'inferenceSpecification', 'describeAlgorithmResponse_inferenceSpecification' - Details about inference jobs that the algorithm runs.
--
-- 'productId', 'describeAlgorithmResponse_productId' - The product identifier of the algorithm.
--
-- 'validationSpecification', 'describeAlgorithmResponse_validationSpecification' - Details about configurations for one or more training jobs that
-- SageMaker runs to test the algorithm.
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
        certifyForMarketplace = Prelude.Nothing,
        inferenceSpecification = Prelude.Nothing,
        productId = Prelude.Nothing,
        validationSpecification = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        algorithmName = pAlgorithmName_,
        algorithmArn = pAlgorithmArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        trainingSpecification = pTrainingSpecification_,
        algorithmStatus = pAlgorithmStatus_,
        algorithmStatusDetails =
          pAlgorithmStatusDetails_
      }

-- | A brief summary about the algorithm.
describeAlgorithmResponse_algorithmDescription :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe Prelude.Text)
describeAlgorithmResponse_algorithmDescription = Lens.lens (\DescribeAlgorithmResponse' {algorithmDescription} -> algorithmDescription) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmDescription = a} :: DescribeAlgorithmResponse)

-- | Whether the algorithm is certified to be listed in Amazon Web Services
-- Marketplace.
describeAlgorithmResponse_certifyForMarketplace :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe Prelude.Bool)
describeAlgorithmResponse_certifyForMarketplace = Lens.lens (\DescribeAlgorithmResponse' {certifyForMarketplace} -> certifyForMarketplace) (\s@DescribeAlgorithmResponse' {} a -> s {certifyForMarketplace = a} :: DescribeAlgorithmResponse)

-- | Details about inference jobs that the algorithm runs.
describeAlgorithmResponse_inferenceSpecification :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe InferenceSpecification)
describeAlgorithmResponse_inferenceSpecification = Lens.lens (\DescribeAlgorithmResponse' {inferenceSpecification} -> inferenceSpecification) (\s@DescribeAlgorithmResponse' {} a -> s {inferenceSpecification = a} :: DescribeAlgorithmResponse)

-- | The product identifier of the algorithm.
describeAlgorithmResponse_productId :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe Prelude.Text)
describeAlgorithmResponse_productId = Lens.lens (\DescribeAlgorithmResponse' {productId} -> productId) (\s@DescribeAlgorithmResponse' {} a -> s {productId = a} :: DescribeAlgorithmResponse)

-- | Details about configurations for one or more training jobs that
-- SageMaker runs to test the algorithm.
describeAlgorithmResponse_validationSpecification :: Lens.Lens' DescribeAlgorithmResponse (Prelude.Maybe AlgorithmValidationSpecification)
describeAlgorithmResponse_validationSpecification = Lens.lens (\DescribeAlgorithmResponse' {validationSpecification} -> validationSpecification) (\s@DescribeAlgorithmResponse' {} a -> s {validationSpecification = a} :: DescribeAlgorithmResponse)

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
describeAlgorithmResponse_creationTime = Lens.lens (\DescribeAlgorithmResponse' {creationTime} -> creationTime) (\s@DescribeAlgorithmResponse' {} a -> s {creationTime = a} :: DescribeAlgorithmResponse) Prelude.. Data._Time

-- | Details about training jobs run by this algorithm.
describeAlgorithmResponse_trainingSpecification :: Lens.Lens' DescribeAlgorithmResponse TrainingSpecification
describeAlgorithmResponse_trainingSpecification = Lens.lens (\DescribeAlgorithmResponse' {trainingSpecification} -> trainingSpecification) (\s@DescribeAlgorithmResponse' {} a -> s {trainingSpecification = a} :: DescribeAlgorithmResponse)

-- | The current status of the algorithm.
describeAlgorithmResponse_algorithmStatus :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatus
describeAlgorithmResponse_algorithmStatus = Lens.lens (\DescribeAlgorithmResponse' {algorithmStatus} -> algorithmStatus) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmStatus = a} :: DescribeAlgorithmResponse)

-- | Details about the current status of the algorithm.
describeAlgorithmResponse_algorithmStatusDetails :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
describeAlgorithmResponse_algorithmStatusDetails = Lens.lens (\DescribeAlgorithmResponse' {algorithmStatusDetails} -> algorithmStatusDetails) (\s@DescribeAlgorithmResponse' {} a -> s {algorithmStatusDetails = a} :: DescribeAlgorithmResponse)

instance Prelude.NFData DescribeAlgorithmResponse where
  rnf DescribeAlgorithmResponse' {..} =
    Prelude.rnf algorithmDescription
      `Prelude.seq` Prelude.rnf certifyForMarketplace
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf validationSpecification
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf algorithmArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf trainingSpecification
      `Prelude.seq` Prelude.rnf algorithmStatus
      `Prelude.seq` Prelude.rnf algorithmStatusDetails
