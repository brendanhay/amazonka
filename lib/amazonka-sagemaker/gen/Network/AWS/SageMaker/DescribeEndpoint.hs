{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint.
module Network.AWS.SageMaker.DescribeEndpoint
    (
    -- * Creating a request
      DescribeEndpoint (..)
    , mkDescribeEndpoint
    -- ** Request lenses
    , dEndpointName

    -- * Destructuring the response
    , DescribeEndpointResponse (..)
    , mkDescribeEndpointResponse
    -- ** Response lenses
    , dergrsEndpointName
    , dergrsEndpointArn
    , dergrsEndpointConfigName
    , dergrsEndpointStatus
    , dergrsCreationTime
    , dergrsLastModifiedTime
    , dergrsDataCaptureConfig
    , dergrsFailureReason
    , dergrsProductionVariants
    , dergrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { endpointName :: Types.EndpointName
    -- ^ The name of the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpoint' value with any optional fields omitted.
mkDescribeEndpoint
    :: Types.EndpointName -- ^ 'endpointName'
    -> DescribeEndpoint
mkDescribeEndpoint endpointName = DescribeEndpoint'{endpointName}

-- | The name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndpointName :: Lens.Lens' DescribeEndpoint Types.EndpointName
dEndpointName = Lens.field @"endpointName"
{-# INLINEABLE dEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

instance Core.ToQuery DescribeEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEndpoint where
        toHeaders DescribeEndpoint{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeEndpoint") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEndpoint where
        toJSON DescribeEndpoint{..}
          = Core.object
              (Core.catMaybes [Core.Just ("EndpointName" Core..= endpointName)])

instance Core.AWSRequest DescribeEndpoint where
        type Rs DescribeEndpoint = DescribeEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEndpointResponse' Core.<$>
                   (x Core..: "EndpointName") Core.<*> x Core..: "EndpointArn"
                     Core.<*> x Core..: "EndpointConfigName"
                     Core.<*> x Core..: "EndpointStatus"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "LastModifiedTime"
                     Core.<*> x Core..:? "DataCaptureConfig"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "ProductionVariants"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { endpointName :: Types.EndpointName
    -- ^ Name of the endpoint.
  , endpointArn :: Types.EndpointArn
    -- ^ The Amazon Resource Name (ARN) of the endpoint.
  , endpointConfigName :: Types.EndpointConfigName
    -- ^ The name of the endpoint configuration associated with this endpoint.
  , endpointStatus :: Types.EndpointStatus
    -- ^ The status of the endpoint.
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
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the endpoint was created.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the endpoint was last modified.
  , dataCaptureConfig :: Core.Maybe Types.DataCaptureConfigSummary
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ If the status of the endpoint is @Failed@ , the reason why it failed. 
  , productionVariants :: Core.Maybe (Core.NonEmpty Types.ProductionVariantSummary)
    -- ^ An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEndpointResponse' value with any optional fields omitted.
mkDescribeEndpointResponse
    :: Types.EndpointName -- ^ 'endpointName'
    -> Types.EndpointArn -- ^ 'endpointArn'
    -> Types.EndpointConfigName -- ^ 'endpointConfigName'
    -> Types.EndpointStatus -- ^ 'endpointStatus'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeEndpointResponse
mkDescribeEndpointResponse endpointName endpointArn
  endpointConfigName endpointStatus creationTime lastModifiedTime
  responseStatus
  = DescribeEndpointResponse'{endpointName, endpointArn,
                              endpointConfigName, endpointStatus, creationTime, lastModifiedTime,
                              dataCaptureConfig = Core.Nothing, failureReason = Core.Nothing,
                              productionVariants = Core.Nothing, responseStatus}

-- | Name of the endpoint.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsEndpointName :: Lens.Lens' DescribeEndpointResponse Types.EndpointName
dergrsEndpointName = Lens.field @"endpointName"
{-# INLINEABLE dergrsEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsEndpointArn :: Lens.Lens' DescribeEndpointResponse Types.EndpointArn
dergrsEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE dergrsEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The name of the endpoint configuration associated with this endpoint.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsEndpointConfigName :: Lens.Lens' DescribeEndpointResponse Types.EndpointConfigName
dergrsEndpointConfigName = Lens.field @"endpointConfigName"
{-# INLINEABLE dergrsEndpointConfigName #-}
{-# DEPRECATED endpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead"  #-}

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
--
-- /Note:/ Consider using 'endpointStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsEndpointStatus :: Lens.Lens' DescribeEndpointResponse Types.EndpointStatus
dergrsEndpointStatus = Lens.field @"endpointStatus"
{-# INLINEABLE dergrsEndpointStatus #-}
{-# DEPRECATED endpointStatus "Use generic-lens or generic-optics with 'endpointStatus' instead"  #-}

-- | A timestamp that shows when the endpoint was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsCreationTime :: Lens.Lens' DescribeEndpointResponse Core.NominalDiffTime
dergrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dergrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A timestamp that shows when the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsLastModifiedTime :: Lens.Lens' DescribeEndpointResponse Core.NominalDiffTime
dergrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dergrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsDataCaptureConfig :: Lens.Lens' DescribeEndpointResponse (Core.Maybe Types.DataCaptureConfigSummary)
dergrsDataCaptureConfig = Lens.field @"dataCaptureConfig"
{-# INLINEABLE dergrsDataCaptureConfig #-}
{-# DEPRECATED dataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead"  #-}

-- | If the status of the endpoint is @Failed@ , the reason why it failed. 
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsFailureReason :: Lens.Lens' DescribeEndpointResponse (Core.Maybe Types.FailureReason)
dergrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dergrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | An array of 'ProductionVariantSummary' objects, one for each model hosted behind this endpoint. 
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsProductionVariants :: Lens.Lens' DescribeEndpointResponse (Core.Maybe (Core.NonEmpty Types.ProductionVariantSummary))
dergrsProductionVariants = Lens.field @"productionVariants"
{-# INLINEABLE dergrsProductionVariants #-}
{-# DEPRECATED productionVariants "Use generic-lens or generic-optics with 'productionVariants' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dergrsResponseStatus :: Lens.Lens' DescribeEndpointResponse Core.Int
dergrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dergrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
