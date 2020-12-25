{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSummary
  ( EndpointSummary (..),

    -- * Smart constructor
    mkEndpointSummary,

    -- * Lenses
    esEndpointName,
    esEndpointArn,
    esCreationTime,
    esLastModifiedTime,
    esEndpointStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EndpointArn as Types
import qualified Network.AWS.SageMaker.Types.EndpointName as Types
import qualified Network.AWS.SageMaker.Types.EndpointStatus as Types

-- | Provides summary information for an endpoint.
--
-- /See:/ 'mkEndpointSummary' smart constructor.
data EndpointSummary = EndpointSummary'
  { -- | The name of the endpoint.
    endpointName :: Types.EndpointName,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Types.EndpointArn,
    -- | A timestamp that shows when the endpoint was created.
    creationTime :: Core.NominalDiffTime,
    -- | A timestamp that shows when the endpoint was last modified.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | The status of the endpoint.
    --
    --
    --     * @OutOfService@ : Endpoint is not available to take incoming requests.
    --
    --
    --     * @Creating@ : 'CreateEndpoint' is executing.
    --
    --
    --     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.
    --
    --
    --     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.
    --
    --
    --     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.
    --
    --
    --     * @InService@ : Endpoint is available to process incoming requests.
    --
    --
    --     * @Deleting@ : 'DeleteEndpoint' is executing.
    --
    --
    --     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint.
    --
    --
    -- To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
    endpointStatus :: Types.EndpointStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EndpointSummary' value with any optional fields omitted.
mkEndpointSummary ::
  -- | 'endpointName'
  Types.EndpointName ->
  -- | 'endpointArn'
  Types.EndpointArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'endpointStatus'
  Types.EndpointStatus ->
  EndpointSummary
mkEndpointSummary
  endpointName
  endpointArn
  creationTime
  lastModifiedTime
  endpointStatus =
    EndpointSummary'
      { endpointName,
        endpointArn,
        creationTime,
        lastModifiedTime,
        endpointStatus
      }

-- | The name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointName :: Lens.Lens' EndpointSummary Types.EndpointName
esEndpointName = Lens.field @"endpointName"
{-# DEPRECATED esEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointArn :: Lens.Lens' EndpointSummary Types.EndpointArn
esEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED esEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | A timestamp that shows when the endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCreationTime :: Lens.Lens' EndpointSummary Core.NominalDiffTime
esCreationTime = Lens.field @"creationTime"
{-# DEPRECATED esCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that shows when the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLastModifiedTime :: Lens.Lens' EndpointSummary Core.NominalDiffTime
esLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED esLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the endpoint.
--
--
--     * @OutOfService@ : Endpoint is not available to take incoming requests.
--
--
--     * @Creating@ : 'CreateEndpoint' is executing.
--
--
--     * @Updating@ : 'UpdateEndpoint' or 'UpdateEndpointWeightsAndCapacities' is executing.
--
--
--     * @SystemUpdating@ : Endpoint is undergoing maintenance and cannot be updated or deleted or re-scaled until it has completed. This maintenance operation does not change any customer-specified values such as VPC config, KMS encryption, model, instance type, or instance count.
--
--
--     * @RollingBack@ : Endpoint fails to scale up or down or change its variant weight and is in the process of rolling back to its previous configuration. Once the rollback completes, endpoint returns to an @InService@ status. This transitional status only applies to an endpoint that has autoscaling enabled and is undergoing variant weight or capacity changes as part of an 'UpdateEndpointWeightsAndCapacities' call or when the 'UpdateEndpointWeightsAndCapacities' operation is called explicitly.
--
--
--     * @InService@ : Endpoint is available to process incoming requests.
--
--
--     * @Deleting@ : 'DeleteEndpoint' is executing.
--
--
--     * @Failed@ : Endpoint could not be created, updated, or re-scaled. Use 'DescribeEndpointOutput$FailureReason' for information about the failure. 'DeleteEndpoint' is the only operation that can be performed on a failed endpoint.
--
--
-- To get a list of endpoints with a specified status, use the 'ListEndpointsInput$StatusEquals' filter.
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esEndpointStatus :: Lens.Lens' EndpointSummary Types.EndpointStatus
esEndpointStatus = Lens.field @"endpointStatus"
{-# DEPRECATED esEndpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead." #-}

instance Core.FromJSON EndpointSummary where
  parseJSON =
    Core.withObject "EndpointSummary" Core.$
      \x ->
        EndpointSummary'
          Core.<$> (x Core..: "EndpointName")
          Core.<*> (x Core..: "EndpointArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "LastModifiedTime")
          Core.<*> (x Core..: "EndpointStatus")
