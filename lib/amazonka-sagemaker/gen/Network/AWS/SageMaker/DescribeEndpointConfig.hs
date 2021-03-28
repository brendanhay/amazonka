{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint configuration created using the @CreateEndpointConfig@ API.
module Network.AWS.SageMaker.DescribeEndpointConfig
    (
    -- * Creating a request
      DescribeEndpointConfig (..)
    , mkDescribeEndpointConfig
    -- ** Request lenses
    , decEndpointConfigName

    -- * Destructuring the response
    , DescribeEndpointConfigResponse (..)
    , mkDescribeEndpointConfigResponse
    -- ** Response lenses
    , decrrsEndpointConfigName
    , decrrsEndpointConfigArn
    , decrrsProductionVariants
    , decrrsCreationTime
    , decrrsDataCaptureConfig
    , decrrsKmsKeyId
    , decrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeEndpointConfig' smart constructor.
newtype DescribeEndpointConfig = DescribeEndpointConfig'
  { endpointConfigName :: Types.EndpointConfigName
    -- ^ The name of the endpoint configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointConfig' value with any optional fields omitted.
mkDescribeEndpointConfig
    :: Types.EndpointConfigName -- ^ 'endpointConfigName'
    -> DescribeEndpointConfig
mkDescribeEndpointConfig endpointConfigName
  = DescribeEndpointConfig'{endpointConfigName}

-- | The name of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEndpointConfigName :: Lens.Lens' DescribeEndpointConfig Types.EndpointConfigName
decEndpointConfigName = Lens.field @"endpointConfigName"
{-# INLINEABLE decEndpointConfigName #-}
{-# DEPRECATED endpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead"  #-}

instance Core.ToQuery DescribeEndpointConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEndpointConfig where
        toHeaders DescribeEndpointConfig{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeEndpointConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEndpointConfig where
        toJSON DescribeEndpointConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointConfigName" Core..= endpointConfigName)])

instance Core.AWSRequest DescribeEndpointConfig where
        type Rs DescribeEndpointConfig = DescribeEndpointConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEndpointConfigResponse' Core.<$>
                   (x Core..: "EndpointConfigName") Core.<*>
                     x Core..: "EndpointConfigArn"
                     Core.<*> x Core..: "ProductionVariants"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..:? "DataCaptureConfig"
                     Core.<*> x Core..:? "KmsKeyId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEndpointConfigResponse' smart constructor.
data DescribeEndpointConfigResponse = DescribeEndpointConfigResponse'
  { endpointConfigName :: Types.EndpointConfigName
    -- ^ Name of the Amazon SageMaker endpoint configuration.
  , endpointConfigArn :: Types.EndpointConfigArn
    -- ^ The Amazon Resource Name (ARN) of the endpoint configuration.
  , productionVariants :: Core.NonEmpty Types.ProductionVariant
    -- ^ An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the endpoint configuration was created.
  , dataCaptureConfig :: Core.Maybe Types.DataCaptureConfig
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEndpointConfigResponse' value with any optional fields omitted.
mkDescribeEndpointConfigResponse
    :: Types.EndpointConfigName -- ^ 'endpointConfigName'
    -> Types.EndpointConfigArn -- ^ 'endpointConfigArn'
    -> Core.NonEmpty Types.ProductionVariant -- ^ 'productionVariants'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeEndpointConfigResponse
mkDescribeEndpointConfigResponse endpointConfigName
  endpointConfigArn productionVariants creationTime responseStatus
  = DescribeEndpointConfigResponse'{endpointConfigName,
                                    endpointConfigArn, productionVariants, creationTime,
                                    dataCaptureConfig = Core.Nothing, kmsKeyId = Core.Nothing,
                                    responseStatus}

-- | Name of the Amazon SageMaker endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEndpointConfigName :: Lens.Lens' DescribeEndpointConfigResponse Types.EndpointConfigName
decrrsEndpointConfigName = Lens.field @"endpointConfigName"
{-# INLINEABLE decrrsEndpointConfigName #-}
{-# DEPRECATED endpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEndpointConfigArn :: Lens.Lens' DescribeEndpointConfigResponse Types.EndpointConfigArn
decrrsEndpointConfigArn = Lens.field @"endpointConfigArn"
{-# INLINEABLE decrrsEndpointConfigArn #-}
{-# DEPRECATED endpointConfigArn "Use generic-lens or generic-optics with 'endpointConfigArn' instead"  #-}

-- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsProductionVariants :: Lens.Lens' DescribeEndpointConfigResponse (Core.NonEmpty Types.ProductionVariant)
decrrsProductionVariants = Lens.field @"productionVariants"
{-# INLINEABLE decrrsProductionVariants #-}
{-# DEPRECATED productionVariants "Use generic-lens or generic-optics with 'productionVariants' instead"  #-}

-- | A timestamp that shows when the endpoint configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsCreationTime :: Lens.Lens' DescribeEndpointConfigResponse Core.NominalDiffTime
decrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE decrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsDataCaptureConfig :: Lens.Lens' DescribeEndpointConfigResponse (Core.Maybe Types.DataCaptureConfig)
decrrsDataCaptureConfig = Lens.field @"dataCaptureConfig"
{-# INLINEABLE decrrsDataCaptureConfig #-}
{-# DEPRECATED dataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead"  #-}

-- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsKmsKeyId :: Lens.Lens' DescribeEndpointConfigResponse (Core.Maybe Types.KmsKeyId)
decrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE decrrsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEndpointConfigResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
