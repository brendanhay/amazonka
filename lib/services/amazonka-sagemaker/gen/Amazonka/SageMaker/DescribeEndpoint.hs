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
-- Module      : Amazonka.SageMaker.DescribeEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint.
module Amazonka.SageMaker.DescribeEndpoint
  ( -- * Creating a Request
    DescribeEndpoint (..),
    newDescribeEndpoint,

    -- * Request Lenses
    describeEndpoint_endpointName,

    -- * Destructuring the Response
    DescribeEndpointResponse (..),
    newDescribeEndpointResponse,

    -- * Response Lenses
    describeEndpointResponse_asyncInferenceConfig,
    describeEndpointResponse_dataCaptureConfig,
    describeEndpointResponse_explainerConfig,
    describeEndpointResponse_failureReason,
    describeEndpointResponse_lastDeploymentConfig,
    describeEndpointResponse_pendingDeploymentSummary,
    describeEndpointResponse_productionVariants,
    describeEndpointResponse_shadowProductionVariants,
    describeEndpointResponse_httpStatus,
    describeEndpointResponse_endpointName,
    describeEndpointResponse_endpointArn,
    describeEndpointResponse_endpointConfigName,
    describeEndpointResponse_endpointStatus,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The name of the endpoint.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'describeEndpoint_endpointName' - The name of the endpoint.
newDescribeEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  DescribeEndpoint
newDescribeEndpoint pEndpointName_ =
  DescribeEndpoint' {endpointName = pEndpointName_}

-- | The name of the endpoint.
describeEndpoint_endpointName :: Lens.Lens' DescribeEndpoint Prelude.Text
describeEndpoint_endpointName = Lens.lens (\DescribeEndpoint' {endpointName} -> endpointName) (\s@DescribeEndpoint' {} a -> s {endpointName = a} :: DescribeEndpoint)

instance Core.AWSRequest DescribeEndpoint where
  type
    AWSResponse DescribeEndpoint =
      DescribeEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Prelude.<$> (x Data..?> "AsyncInferenceConfig")
            Prelude.<*> (x Data..?> "DataCaptureConfig")
            Prelude.<*> (x Data..?> "ExplainerConfig")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LastDeploymentConfig")
            Prelude.<*> (x Data..?> "PendingDeploymentSummary")
            Prelude.<*> (x Data..?> "ProductionVariants")
            Prelude.<*> (x Data..?> "ShadowProductionVariants")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EndpointName")
            Prelude.<*> (x Data..:> "EndpointArn")
            Prelude.<*> (x Data..:> "EndpointConfigName")
            Prelude.<*> (x Data..:> "EndpointStatus")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
      )

instance Prelude.Hashable DescribeEndpoint where
  hashWithSalt _salt DescribeEndpoint' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData DescribeEndpoint where
  rnf DescribeEndpoint' {..} = Prelude.rnf endpointName

instance Data.ToHeaders DescribeEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("EndpointName" Data..= endpointName)]
      )

instance Data.ToPath DescribeEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | Returns the description of an endpoint configuration created using the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>
    -- API.
    asyncInferenceConfig :: Prelude.Maybe AsyncInferenceConfig,
    dataCaptureConfig :: Prelude.Maybe DataCaptureConfigSummary,
    -- | The configuration parameters for an explainer.
    explainerConfig :: Prelude.Maybe ExplainerConfig,
    -- | If the status of the endpoint is @Failed@, the reason why it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The most recent deployment configuration for the endpoint.
    lastDeploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | Returns the summary of an in-progress deployment. This field is only
    -- returned when the endpoint is creating or updating with a new endpoint
    -- configuration.
    pendingDeploymentSummary :: Prelude.Maybe PendingDeploymentSummary,
    -- | An array of ProductionVariantSummary objects, one for each model hosted
    -- behind this endpoint.
    productionVariants :: Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary),
    -- | An array of ProductionVariantSummary objects, one for each model that
    -- you want to host at this endpoint in shadow mode with production traffic
    -- replicated from the model specified on @ProductionVariants@.
    shadowProductionVariants :: Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Name of the endpoint.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text,
    -- | The name of the endpoint configuration associated with this endpoint.
    endpointConfigName :: Prelude.Text,
    -- | The status of the endpoint.
    --
    -- -   @OutOfService@: Endpoint is not available to take incoming requests.
    --
    -- -   @Creating@: CreateEndpoint is executing.
    --
    -- -   @Updating@: UpdateEndpoint or UpdateEndpointWeightsAndCapacities is
    --     executing.
    --
    -- -   @SystemUpdating@: Endpoint is undergoing maintenance and cannot be
    --     updated or deleted or re-scaled until it has completed. This
    --     maintenance operation does not change any customer-specified values
    --     such as VPC config, KMS encryption, model, instance type, or
    --     instance count.
    --
    -- -   @RollingBack@: Endpoint fails to scale up or down or change its
    --     variant weight and is in the process of rolling back to its previous
    --     configuration. Once the rollback completes, endpoint returns to an
    --     @InService@ status. This transitional status only applies to an
    --     endpoint that has autoscaling enabled and is undergoing variant
    --     weight or capacity changes as part of an
    --     UpdateEndpointWeightsAndCapacities call or when the
    --     UpdateEndpointWeightsAndCapacities operation is called explicitly.
    --
    -- -   @InService@: Endpoint is available to process incoming requests.
    --
    -- -   @Deleting@: DeleteEndpoint is executing.
    --
    -- -   @Failed@: Endpoint could not be created, updated, or re-scaled. Use
    --     DescribeEndpointOutput$FailureReason for information about the
    --     failure. DeleteEndpoint is the only operation that can be performed
    --     on a failed endpoint.
    endpointStatus :: EndpointStatus,
    -- | A timestamp that shows when the endpoint was created.
    creationTime :: Data.POSIX,
    -- | A timestamp that shows when the endpoint was last modified.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asyncInferenceConfig', 'describeEndpointResponse_asyncInferenceConfig' - Returns the description of an endpoint configuration created using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>
-- API.
--
-- 'dataCaptureConfig', 'describeEndpointResponse_dataCaptureConfig' - Undocumented member.
--
-- 'explainerConfig', 'describeEndpointResponse_explainerConfig' - The configuration parameters for an explainer.
--
-- 'failureReason', 'describeEndpointResponse_failureReason' - If the status of the endpoint is @Failed@, the reason why it failed.
--
-- 'lastDeploymentConfig', 'describeEndpointResponse_lastDeploymentConfig' - The most recent deployment configuration for the endpoint.
--
-- 'pendingDeploymentSummary', 'describeEndpointResponse_pendingDeploymentSummary' - Returns the summary of an in-progress deployment. This field is only
-- returned when the endpoint is creating or updating with a new endpoint
-- configuration.
--
-- 'productionVariants', 'describeEndpointResponse_productionVariants' - An array of ProductionVariantSummary objects, one for each model hosted
-- behind this endpoint.
--
-- 'shadowProductionVariants', 'describeEndpointResponse_shadowProductionVariants' - An array of ProductionVariantSummary objects, one for each model that
-- you want to host at this endpoint in shadow mode with production traffic
-- replicated from the model specified on @ProductionVariants@.
--
-- 'httpStatus', 'describeEndpointResponse_httpStatus' - The response's http status code.
--
-- 'endpointName', 'describeEndpointResponse_endpointName' - Name of the endpoint.
--
-- 'endpointArn', 'describeEndpointResponse_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
--
-- 'endpointConfigName', 'describeEndpointResponse_endpointConfigName' - The name of the endpoint configuration associated with this endpoint.
--
-- 'endpointStatus', 'describeEndpointResponse_endpointStatus' - The status of the endpoint.
--
-- -   @OutOfService@: Endpoint is not available to take incoming requests.
--
-- -   @Creating@: CreateEndpoint is executing.
--
-- -   @Updating@: UpdateEndpoint or UpdateEndpointWeightsAndCapacities is
--     executing.
--
-- -   @SystemUpdating@: Endpoint is undergoing maintenance and cannot be
--     updated or deleted or re-scaled until it has completed. This
--     maintenance operation does not change any customer-specified values
--     such as VPC config, KMS encryption, model, instance type, or
--     instance count.
--
-- -   @RollingBack@: Endpoint fails to scale up or down or change its
--     variant weight and is in the process of rolling back to its previous
--     configuration. Once the rollback completes, endpoint returns to an
--     @InService@ status. This transitional status only applies to an
--     endpoint that has autoscaling enabled and is undergoing variant
--     weight or capacity changes as part of an
--     UpdateEndpointWeightsAndCapacities call or when the
--     UpdateEndpointWeightsAndCapacities operation is called explicitly.
--
-- -   @InService@: Endpoint is available to process incoming requests.
--
-- -   @Deleting@: DeleteEndpoint is executing.
--
-- -   @Failed@: Endpoint could not be created, updated, or re-scaled. Use
--     DescribeEndpointOutput$FailureReason for information about the
--     failure. DeleteEndpoint is the only operation that can be performed
--     on a failed endpoint.
--
-- 'creationTime', 'describeEndpointResponse_creationTime' - A timestamp that shows when the endpoint was created.
--
-- 'lastModifiedTime', 'describeEndpointResponse_lastModifiedTime' - A timestamp that shows when the endpoint was last modified.
newDescribeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'endpointArn'
  Prelude.Text ->
  -- | 'endpointConfigName'
  Prelude.Text ->
  -- | 'endpointStatus'
  EndpointStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  DescribeEndpointResponse
newDescribeEndpointResponse
  pHttpStatus_
  pEndpointName_
  pEndpointArn_
  pEndpointConfigName_
  pEndpointStatus_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeEndpointResponse'
      { asyncInferenceConfig =
          Prelude.Nothing,
        dataCaptureConfig = Prelude.Nothing,
        explainerConfig = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        lastDeploymentConfig = Prelude.Nothing,
        pendingDeploymentSummary = Prelude.Nothing,
        productionVariants = Prelude.Nothing,
        shadowProductionVariants = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        endpointName = pEndpointName_,
        endpointArn = pEndpointArn_,
        endpointConfigName = pEndpointConfigName_,
        endpointStatus = pEndpointStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | Returns the description of an endpoint configuration created using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpointConfig.html CreateEndpointConfig>
-- API.
describeEndpointResponse_asyncInferenceConfig :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe AsyncInferenceConfig)
describeEndpointResponse_asyncInferenceConfig = Lens.lens (\DescribeEndpointResponse' {asyncInferenceConfig} -> asyncInferenceConfig) (\s@DescribeEndpointResponse' {} a -> s {asyncInferenceConfig = a} :: DescribeEndpointResponse)

-- | Undocumented member.
describeEndpointResponse_dataCaptureConfig :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe DataCaptureConfigSummary)
describeEndpointResponse_dataCaptureConfig = Lens.lens (\DescribeEndpointResponse' {dataCaptureConfig} -> dataCaptureConfig) (\s@DescribeEndpointResponse' {} a -> s {dataCaptureConfig = a} :: DescribeEndpointResponse)

-- | The configuration parameters for an explainer.
describeEndpointResponse_explainerConfig :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe ExplainerConfig)
describeEndpointResponse_explainerConfig = Lens.lens (\DescribeEndpointResponse' {explainerConfig} -> explainerConfig) (\s@DescribeEndpointResponse' {} a -> s {explainerConfig = a} :: DescribeEndpointResponse)

-- | If the status of the endpoint is @Failed@, the reason why it failed.
describeEndpointResponse_failureReason :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_failureReason = Lens.lens (\DescribeEndpointResponse' {failureReason} -> failureReason) (\s@DescribeEndpointResponse' {} a -> s {failureReason = a} :: DescribeEndpointResponse)

-- | The most recent deployment configuration for the endpoint.
describeEndpointResponse_lastDeploymentConfig :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe DeploymentConfig)
describeEndpointResponse_lastDeploymentConfig = Lens.lens (\DescribeEndpointResponse' {lastDeploymentConfig} -> lastDeploymentConfig) (\s@DescribeEndpointResponse' {} a -> s {lastDeploymentConfig = a} :: DescribeEndpointResponse)

-- | Returns the summary of an in-progress deployment. This field is only
-- returned when the endpoint is creating or updating with a new endpoint
-- configuration.
describeEndpointResponse_pendingDeploymentSummary :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe PendingDeploymentSummary)
describeEndpointResponse_pendingDeploymentSummary = Lens.lens (\DescribeEndpointResponse' {pendingDeploymentSummary} -> pendingDeploymentSummary) (\s@DescribeEndpointResponse' {} a -> s {pendingDeploymentSummary = a} :: DescribeEndpointResponse)

-- | An array of ProductionVariantSummary objects, one for each model hosted
-- behind this endpoint.
describeEndpointResponse_productionVariants :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary))
describeEndpointResponse_productionVariants = Lens.lens (\DescribeEndpointResponse' {productionVariants} -> productionVariants) (\s@DescribeEndpointResponse' {} a -> s {productionVariants = a} :: DescribeEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of ProductionVariantSummary objects, one for each model that
-- you want to host at this endpoint in shadow mode with production traffic
-- replicated from the model specified on @ProductionVariants@.
describeEndpointResponse_shadowProductionVariants :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary))
describeEndpointResponse_shadowProductionVariants = Lens.lens (\DescribeEndpointResponse' {shadowProductionVariants} -> shadowProductionVariants) (\s@DescribeEndpointResponse' {} a -> s {shadowProductionVariants = a} :: DescribeEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Prelude.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

-- | Name of the endpoint.
describeEndpointResponse_endpointName :: Lens.Lens' DescribeEndpointResponse Prelude.Text
describeEndpointResponse_endpointName = Lens.lens (\DescribeEndpointResponse' {endpointName} -> endpointName) (\s@DescribeEndpointResponse' {} a -> s {endpointName = a} :: DescribeEndpointResponse)

-- | The Amazon Resource Name (ARN) of the endpoint.
describeEndpointResponse_endpointArn :: Lens.Lens' DescribeEndpointResponse Prelude.Text
describeEndpointResponse_endpointArn = Lens.lens (\DescribeEndpointResponse' {endpointArn} -> endpointArn) (\s@DescribeEndpointResponse' {} a -> s {endpointArn = a} :: DescribeEndpointResponse)

-- | The name of the endpoint configuration associated with this endpoint.
describeEndpointResponse_endpointConfigName :: Lens.Lens' DescribeEndpointResponse Prelude.Text
describeEndpointResponse_endpointConfigName = Lens.lens (\DescribeEndpointResponse' {endpointConfigName} -> endpointConfigName) (\s@DescribeEndpointResponse' {} a -> s {endpointConfigName = a} :: DescribeEndpointResponse)

-- | The status of the endpoint.
--
-- -   @OutOfService@: Endpoint is not available to take incoming requests.
--
-- -   @Creating@: CreateEndpoint is executing.
--
-- -   @Updating@: UpdateEndpoint or UpdateEndpointWeightsAndCapacities is
--     executing.
--
-- -   @SystemUpdating@: Endpoint is undergoing maintenance and cannot be
--     updated or deleted or re-scaled until it has completed. This
--     maintenance operation does not change any customer-specified values
--     such as VPC config, KMS encryption, model, instance type, or
--     instance count.
--
-- -   @RollingBack@: Endpoint fails to scale up or down or change its
--     variant weight and is in the process of rolling back to its previous
--     configuration. Once the rollback completes, endpoint returns to an
--     @InService@ status. This transitional status only applies to an
--     endpoint that has autoscaling enabled and is undergoing variant
--     weight or capacity changes as part of an
--     UpdateEndpointWeightsAndCapacities call or when the
--     UpdateEndpointWeightsAndCapacities operation is called explicitly.
--
-- -   @InService@: Endpoint is available to process incoming requests.
--
-- -   @Deleting@: DeleteEndpoint is executing.
--
-- -   @Failed@: Endpoint could not be created, updated, or re-scaled. Use
--     DescribeEndpointOutput$FailureReason for information about the
--     failure. DeleteEndpoint is the only operation that can be performed
--     on a failed endpoint.
describeEndpointResponse_endpointStatus :: Lens.Lens' DescribeEndpointResponse EndpointStatus
describeEndpointResponse_endpointStatus = Lens.lens (\DescribeEndpointResponse' {endpointStatus} -> endpointStatus) (\s@DescribeEndpointResponse' {} a -> s {endpointStatus = a} :: DescribeEndpointResponse)

-- | A timestamp that shows when the endpoint was created.
describeEndpointResponse_creationTime :: Lens.Lens' DescribeEndpointResponse Prelude.UTCTime
describeEndpointResponse_creationTime = Lens.lens (\DescribeEndpointResponse' {creationTime} -> creationTime) (\s@DescribeEndpointResponse' {} a -> s {creationTime = a} :: DescribeEndpointResponse) Prelude.. Data._Time

-- | A timestamp that shows when the endpoint was last modified.
describeEndpointResponse_lastModifiedTime :: Lens.Lens' DescribeEndpointResponse Prelude.UTCTime
describeEndpointResponse_lastModifiedTime = Lens.lens (\DescribeEndpointResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeEndpointResponse' {} a -> s {lastModifiedTime = a} :: DescribeEndpointResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeEndpointResponse where
  rnf DescribeEndpointResponse' {..} =
    Prelude.rnf asyncInferenceConfig
      `Prelude.seq` Prelude.rnf dataCaptureConfig
      `Prelude.seq` Prelude.rnf explainerConfig
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastDeploymentConfig
      `Prelude.seq` Prelude.rnf pendingDeploymentSummary
      `Prelude.seq` Prelude.rnf productionVariants
      `Prelude.seq` Prelude.rnf shadowProductionVariants
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf endpointConfigName
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
