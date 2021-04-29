{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.EndpointSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.EndpointStatus

-- | Provides summary information for an endpoint.
--
-- /See:/ 'newEndpointSummary' smart constructor.
data EndpointSummary = EndpointSummary'
  { -- | The name of the endpoint.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text,
    -- | A timestamp that shows when the endpoint was created.
    creationTime :: Prelude.POSIX,
    -- | A timestamp that shows when the endpoint was last modified.
    lastModifiedTime :: Prelude.POSIX,
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
    --
    -- To get a list of endpoints with a specified status, use the
    -- ListEndpointsInput$StatusEquals filter.
    endpointStatus :: EndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- To get a list of endpoints with a specified status, use the
-- ListEndpointsInput$StatusEquals filter.
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
        creationTime = Prelude._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
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
endpointSummary_creationTime = Lens.lens (\EndpointSummary' {creationTime} -> creationTime) (\s@EndpointSummary' {} a -> s {creationTime = a} :: EndpointSummary) Prelude.. Prelude._Time

-- | A timestamp that shows when the endpoint was last modified.
endpointSummary_lastModifiedTime :: Lens.Lens' EndpointSummary Prelude.UTCTime
endpointSummary_lastModifiedTime = Lens.lens (\EndpointSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EndpointSummary' {} a -> s {lastModifiedTime = a} :: EndpointSummary) Prelude.. Prelude._Time

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
--
-- To get a list of endpoints with a specified status, use the
-- ListEndpointsInput$StatusEquals filter.
endpointSummary_endpointStatus :: Lens.Lens' EndpointSummary EndpointStatus
endpointSummary_endpointStatus = Lens.lens (\EndpointSummary' {endpointStatus} -> endpointStatus) (\s@EndpointSummary' {} a -> s {endpointStatus = a} :: EndpointSummary)

instance Prelude.FromJSON EndpointSummary where
  parseJSON =
    Prelude.withObject
      "EndpointSummary"
      ( \x ->
          EndpointSummary'
            Prelude.<$> (x Prelude..: "EndpointName")
            Prelude.<*> (x Prelude..: "EndpointArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "LastModifiedTime")
            Prelude.<*> (x Prelude..: "EndpointStatus")
      )

instance Prelude.Hashable EndpointSummary

instance Prelude.NFData EndpointSummary
