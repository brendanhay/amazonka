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
-- Module      : Network.AWS.SageMaker.DescribeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint.
module Network.AWS.SageMaker.DescribeEndpoint
  ( -- * Creating a Request
    DescribeEndpoint (..),
    newDescribeEndpoint,

    -- * Request Lenses
    describeEndpoint_endpointName,

    -- * Destructuring the Response
    DescribeEndpointResponse (..),
    newDescribeEndpointResponse,

    -- * Response Lenses
    describeEndpointResponse_productionVariants,
    describeEndpointResponse_lastDeploymentConfig,
    describeEndpointResponse_failureReason,
    describeEndpointResponse_dataCaptureConfig,
    describeEndpointResponse_httpStatus,
    describeEndpointResponse_endpointName,
    describeEndpointResponse_endpointArn,
    describeEndpointResponse_endpointConfigName,
    describeEndpointResponse_endpointStatus,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_lastModifiedTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The name of the endpoint.
    endpointName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeEndpoint
newDescribeEndpoint pEndpointName_ =
  DescribeEndpoint' {endpointName = pEndpointName_}

-- | The name of the endpoint.
describeEndpoint_endpointName :: Lens.Lens' DescribeEndpoint Core.Text
describeEndpoint_endpointName = Lens.lens (\DescribeEndpoint' {endpointName} -> endpointName) (\s@DescribeEndpoint' {} a -> s {endpointName = a} :: DescribeEndpoint)

instance Core.AWSRequest DescribeEndpoint where
  type
    AWSResponse DescribeEndpoint =
      DescribeEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Core.<$> (x Core..?> "ProductionVariants")
            Core.<*> (x Core..?> "LastDeploymentConfig")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "DataCaptureConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "EndpointName")
            Core.<*> (x Core..:> "EndpointArn")
            Core.<*> (x Core..:> "EndpointConfigName")
            Core.<*> (x Core..:> "EndpointStatus")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "LastModifiedTime")
      )

instance Core.Hashable DescribeEndpoint

instance Core.NFData DescribeEndpoint

instance Core.ToHeaders DescribeEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeEndpoint" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EndpointName" Core..= endpointName)]
      )

instance Core.ToPath DescribeEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | An array of ProductionVariantSummary objects, one for each model hosted
    -- behind this endpoint.
    productionVariants :: Core.Maybe (Core.NonEmpty ProductionVariantSummary),
    -- | The most recent deployment configuration for the endpoint.
    lastDeploymentConfig :: Core.Maybe DeploymentConfig,
    -- | If the status of the endpoint is @Failed@, the reason why it failed.
    failureReason :: Core.Maybe Core.Text,
    dataCaptureConfig :: Core.Maybe DataCaptureConfigSummary,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Name of the endpoint.
    endpointName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Core.Text,
    -- | The name of the endpoint configuration associated with this endpoint.
    endpointConfigName :: Core.Text,
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
    creationTime :: Core.POSIX,
    -- | A timestamp that shows when the endpoint was last modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productionVariants', 'describeEndpointResponse_productionVariants' - An array of ProductionVariantSummary objects, one for each model hosted
-- behind this endpoint.
--
-- 'lastDeploymentConfig', 'describeEndpointResponse_lastDeploymentConfig' - The most recent deployment configuration for the endpoint.
--
-- 'failureReason', 'describeEndpointResponse_failureReason' - If the status of the endpoint is @Failed@, the reason why it failed.
--
-- 'dataCaptureConfig', 'describeEndpointResponse_dataCaptureConfig' - Undocumented member.
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
  Core.Int ->
  -- | 'endpointName'
  Core.Text ->
  -- | 'endpointArn'
  Core.Text ->
  -- | 'endpointConfigName'
  Core.Text ->
  -- | 'endpointStatus'
  EndpointStatus ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
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
      { productionVariants =
          Core.Nothing,
        lastDeploymentConfig = Core.Nothing,
        failureReason = Core.Nothing,
        dataCaptureConfig = Core.Nothing,
        httpStatus = pHttpStatus_,
        endpointName = pEndpointName_,
        endpointArn = pEndpointArn_,
        endpointConfigName = pEndpointConfigName_,
        endpointStatus = pEndpointStatus_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | An array of ProductionVariantSummary objects, one for each model hosted
-- behind this endpoint.
describeEndpointResponse_productionVariants :: Lens.Lens' DescribeEndpointResponse (Core.Maybe (Core.NonEmpty ProductionVariantSummary))
describeEndpointResponse_productionVariants = Lens.lens (\DescribeEndpointResponse' {productionVariants} -> productionVariants) (\s@DescribeEndpointResponse' {} a -> s {productionVariants = a} :: DescribeEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | The most recent deployment configuration for the endpoint.
describeEndpointResponse_lastDeploymentConfig :: Lens.Lens' DescribeEndpointResponse (Core.Maybe DeploymentConfig)
describeEndpointResponse_lastDeploymentConfig = Lens.lens (\DescribeEndpointResponse' {lastDeploymentConfig} -> lastDeploymentConfig) (\s@DescribeEndpointResponse' {} a -> s {lastDeploymentConfig = a} :: DescribeEndpointResponse)

-- | If the status of the endpoint is @Failed@, the reason why it failed.
describeEndpointResponse_failureReason :: Lens.Lens' DescribeEndpointResponse (Core.Maybe Core.Text)
describeEndpointResponse_failureReason = Lens.lens (\DescribeEndpointResponse' {failureReason} -> failureReason) (\s@DescribeEndpointResponse' {} a -> s {failureReason = a} :: DescribeEndpointResponse)

-- | Undocumented member.
describeEndpointResponse_dataCaptureConfig :: Lens.Lens' DescribeEndpointResponse (Core.Maybe DataCaptureConfigSummary)
describeEndpointResponse_dataCaptureConfig = Lens.lens (\DescribeEndpointResponse' {dataCaptureConfig} -> dataCaptureConfig) (\s@DescribeEndpointResponse' {} a -> s {dataCaptureConfig = a} :: DescribeEndpointResponse)

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Core.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

-- | Name of the endpoint.
describeEndpointResponse_endpointName :: Lens.Lens' DescribeEndpointResponse Core.Text
describeEndpointResponse_endpointName = Lens.lens (\DescribeEndpointResponse' {endpointName} -> endpointName) (\s@DescribeEndpointResponse' {} a -> s {endpointName = a} :: DescribeEndpointResponse)

-- | The Amazon Resource Name (ARN) of the endpoint.
describeEndpointResponse_endpointArn :: Lens.Lens' DescribeEndpointResponse Core.Text
describeEndpointResponse_endpointArn = Lens.lens (\DescribeEndpointResponse' {endpointArn} -> endpointArn) (\s@DescribeEndpointResponse' {} a -> s {endpointArn = a} :: DescribeEndpointResponse)

-- | The name of the endpoint configuration associated with this endpoint.
describeEndpointResponse_endpointConfigName :: Lens.Lens' DescribeEndpointResponse Core.Text
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
describeEndpointResponse_creationTime :: Lens.Lens' DescribeEndpointResponse Core.UTCTime
describeEndpointResponse_creationTime = Lens.lens (\DescribeEndpointResponse' {creationTime} -> creationTime) (\s@DescribeEndpointResponse' {} a -> s {creationTime = a} :: DescribeEndpointResponse) Core.. Core._Time

-- | A timestamp that shows when the endpoint was last modified.
describeEndpointResponse_lastModifiedTime :: Lens.Lens' DescribeEndpointResponse Core.UTCTime
describeEndpointResponse_lastModifiedTime = Lens.lens (\DescribeEndpointResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeEndpointResponse' {} a -> s {lastModifiedTime = a} :: DescribeEndpointResponse) Core.. Core._Time

instance Core.NFData DescribeEndpointResponse
