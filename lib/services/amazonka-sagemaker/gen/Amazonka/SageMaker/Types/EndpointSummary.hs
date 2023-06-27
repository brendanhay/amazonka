{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.EndpointSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointStatus

-- | Provides summary information for an endpoint.
--
-- /See:/ 'newEndpointSummary' smart constructor.
data EndpointSummary = EndpointSummary'
  { -- | The name of the endpoint.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text,
    -- | A timestamp that shows when the endpoint was created.
    creationTime :: Data.POSIX,
    -- | A timestamp that shows when the endpoint was last modified.
    lastModifiedTime :: Data.POSIX,
    -- | The status of the endpoint.
    --
    -- -   @OutOfService@: Endpoint is not available to take incoming requests.
    --
    -- -   @Creating@:
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpoint.html CreateEndpoint>
    --     is executing.
    --
    -- -   @Updating@:
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpoint.html UpdateEndpoint>
    --     or
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
    --     is executing.
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
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
    --     call or when the
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
    --     operation is called explicitly.
    --
    -- -   @InService@: Endpoint is available to process incoming requests.
    --
    -- -   @Deleting@:
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteEndpoint.html DeleteEndpoint>
    --     is executing.
    --
    -- -   @Failed@: Endpoint could not be created, updated, or re-scaled. Use
    --     @DescribeEndpointOutput$FailureReason@ for information about the
    --     failure.
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteEndpoint.html DeleteEndpoint>
    --     is the only operation that can be performed on a failed endpoint.
    --
    -- To get a list of endpoints with a specified status, use the
    -- @StatusEquals@ filter with a call to
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListEndpoints.html ListEndpoints>.
    endpointStatus :: EndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'endpointSummary_endpointName' - The name of the endpoint.
--
-- 'endpointArn', 'endpointSummary_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
--
-- 'creationTime', 'endpointSummary_creationTime' - A timestamp that shows when the endpoint was created.
--
-- 'lastModifiedTime', 'endpointSummary_lastModifiedTime' - A timestamp that shows when the endpoint was last modified.
--
-- 'endpointStatus', 'endpointSummary_endpointStatus' - The status of the endpoint.
--
-- -   @OutOfService@: Endpoint is not available to take incoming requests.
--
-- -   @Creating@:
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpoint.html CreateEndpoint>
--     is executing.
--
-- -   @Updating@:
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpoint.html UpdateEndpoint>
--     or
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
--     is executing.
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
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
--     call or when the
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
--     operation is called explicitly.
--
-- -   @InService@: Endpoint is available to process incoming requests.
--
-- -   @Deleting@:
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteEndpoint.html DeleteEndpoint>
--     is executing.
--
-- -   @Failed@: Endpoint could not be created, updated, or re-scaled. Use
--     @DescribeEndpointOutput$FailureReason@ for information about the
--     failure.
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteEndpoint.html DeleteEndpoint>
--     is the only operation that can be performed on a failed endpoint.
--
-- To get a list of endpoints with a specified status, use the
-- @StatusEquals@ filter with a call to
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListEndpoints.html ListEndpoints>.
newEndpointSummary ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'endpointArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'endpointStatus'
  EndpointStatus ->
  EndpointSummary
newEndpointSummary
  pEndpointName_
  pEndpointArn_
  pCreationTime_
  pLastModifiedTime_
  pEndpointStatus_ =
    EndpointSummary'
      { endpointName = pEndpointName_,
        endpointArn = pEndpointArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        endpointStatus = pEndpointStatus_
      }

-- | The name of the endpoint.
endpointSummary_endpointName :: Lens.Lens' EndpointSummary Prelude.Text
endpointSummary_endpointName = Lens.lens (\EndpointSummary' {endpointName} -> endpointName) (\s@EndpointSummary' {} a -> s {endpointName = a} :: EndpointSummary)

-- | The Amazon Resource Name (ARN) of the endpoint.
endpointSummary_endpointArn :: Lens.Lens' EndpointSummary Prelude.Text
endpointSummary_endpointArn = Lens.lens (\EndpointSummary' {endpointArn} -> endpointArn) (\s@EndpointSummary' {} a -> s {endpointArn = a} :: EndpointSummary)

-- | A timestamp that shows when the endpoint was created.
endpointSummary_creationTime :: Lens.Lens' EndpointSummary Prelude.UTCTime
endpointSummary_creationTime = Lens.lens (\EndpointSummary' {creationTime} -> creationTime) (\s@EndpointSummary' {} a -> s {creationTime = a} :: EndpointSummary) Prelude.. Data._Time

-- | A timestamp that shows when the endpoint was last modified.
endpointSummary_lastModifiedTime :: Lens.Lens' EndpointSummary Prelude.UTCTime
endpointSummary_lastModifiedTime = Lens.lens (\EndpointSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EndpointSummary' {} a -> s {lastModifiedTime = a} :: EndpointSummary) Prelude.. Data._Time

-- | The status of the endpoint.
--
-- -   @OutOfService@: Endpoint is not available to take incoming requests.
--
-- -   @Creating@:
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateEndpoint.html CreateEndpoint>
--     is executing.
--
-- -   @Updating@:
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpoint.html UpdateEndpoint>
--     or
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
--     is executing.
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
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
--     call or when the
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateEndpointWeightsAndCapacities.html UpdateEndpointWeightsAndCapacities>
--     operation is called explicitly.
--
-- -   @InService@: Endpoint is available to process incoming requests.
--
-- -   @Deleting@:
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteEndpoint.html DeleteEndpoint>
--     is executing.
--
-- -   @Failed@: Endpoint could not be created, updated, or re-scaled. Use
--     @DescribeEndpointOutput$FailureReason@ for information about the
--     failure.
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteEndpoint.html DeleteEndpoint>
--     is the only operation that can be performed on a failed endpoint.
--
-- To get a list of endpoints with a specified status, use the
-- @StatusEquals@ filter with a call to
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListEndpoints.html ListEndpoints>.
endpointSummary_endpointStatus :: Lens.Lens' EndpointSummary EndpointStatus
endpointSummary_endpointStatus = Lens.lens (\EndpointSummary' {endpointStatus} -> endpointStatus) (\s@EndpointSummary' {} a -> s {endpointStatus = a} :: EndpointSummary)

instance Data.FromJSON EndpointSummary where
  parseJSON =
    Data.withObject
      "EndpointSummary"
      ( \x ->
          EndpointSummary'
            Prelude.<$> (x Data..: "EndpointName")
            Prelude.<*> (x Data..: "EndpointArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "EndpointStatus")
      )

instance Prelude.Hashable EndpointSummary where
  hashWithSalt _salt EndpointSummary' {..} =
    _salt
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` endpointStatus

instance Prelude.NFData EndpointSummary where
  rnf EndpointSummary' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf endpointStatus
