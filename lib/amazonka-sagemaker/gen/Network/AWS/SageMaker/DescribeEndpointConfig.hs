{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeEndpointConfig (..),
    mkDescribeEndpointConfig,

    -- ** Request lenses
    decEndpointConfigName,

    -- * Destructuring the response
    DescribeEndpointConfigResponse (..),
    mkDescribeEndpointConfigResponse,

    -- ** Response lenses
    decrrsEndpointConfigName,
    decrrsEndpointConfigArn,
    decrrsProductionVariants,
    decrrsCreationTime,
    decrrsDataCaptureConfig,
    decrrsKmsKeyId,
    decrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeEndpointConfig' smart constructor.
newtype DescribeEndpointConfig = DescribeEndpointConfig'
  { -- | The name of the endpoint configuration.
    endpointConfigName :: Types.EndpointConfigName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointConfig' value with any optional fields omitted.
mkDescribeEndpointConfig ::
  -- | 'endpointConfigName'
  Types.EndpointConfigName ->
  DescribeEndpointConfig
mkDescribeEndpointConfig endpointConfigName =
  DescribeEndpointConfig' {endpointConfigName}

-- | The name of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEndpointConfigName :: Lens.Lens' DescribeEndpointConfig Types.EndpointConfigName
decEndpointConfigName = Lens.field @"endpointConfigName"
{-# DEPRECATED decEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

instance Core.FromJSON DescribeEndpointConfig where
  toJSON DescribeEndpointConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EndpointConfigName" Core..= endpointConfigName)]
      )

instance Core.AWSRequest DescribeEndpointConfig where
  type Rs DescribeEndpointConfig = DescribeEndpointConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeEndpointConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointConfigResponse'
            Core.<$> (x Core..: "EndpointConfigName")
            Core.<*> (x Core..: "EndpointConfigArn")
            Core.<*> (x Core..: "ProductionVariants")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..:? "DataCaptureConfig")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEndpointConfigResponse' smart constructor.
data DescribeEndpointConfigResponse = DescribeEndpointConfigResponse'
  { -- | Name of the Amazon SageMaker endpoint configuration.
    endpointConfigName :: Types.EndpointConfigName,
    -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigArn :: Types.EndpointConfigArn,
    -- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
    productionVariants :: Core.NonEmpty Types.ProductionVariant,
    -- | A timestamp that shows when the endpoint configuration was created.
    creationTime :: Core.NominalDiffTime,
    dataCaptureConfig :: Core.Maybe Types.DataCaptureConfig,
    -- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEndpointConfigResponse' value with any optional fields omitted.
mkDescribeEndpointConfigResponse ::
  -- | 'endpointConfigName'
  Types.EndpointConfigName ->
  -- | 'endpointConfigArn'
  Types.EndpointConfigArn ->
  -- | 'productionVariants'
  Core.NonEmpty Types.ProductionVariant ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeEndpointConfigResponse
mkDescribeEndpointConfigResponse
  endpointConfigName
  endpointConfigArn
  productionVariants
  creationTime
  responseStatus =
    DescribeEndpointConfigResponse'
      { endpointConfigName,
        endpointConfigArn,
        productionVariants,
        creationTime,
        dataCaptureConfig = Core.Nothing,
        kmsKeyId = Core.Nothing,
        responseStatus
      }

-- | Name of the Amazon SageMaker endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEndpointConfigName :: Lens.Lens' DescribeEndpointConfigResponse Types.EndpointConfigName
decrrsEndpointConfigName = Lens.field @"endpointConfigName"
{-# DEPRECATED decrrsEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsEndpointConfigArn :: Lens.Lens' DescribeEndpointConfigResponse Types.EndpointConfigArn
decrrsEndpointConfigArn = Lens.field @"endpointConfigArn"
{-# DEPRECATED decrrsEndpointConfigArn "Use generic-lens or generic-optics with 'endpointConfigArn' instead." #-}

-- | An array of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsProductionVariants :: Lens.Lens' DescribeEndpointConfigResponse (Core.NonEmpty Types.ProductionVariant)
decrrsProductionVariants = Lens.field @"productionVariants"
{-# DEPRECATED decrrsProductionVariants "Use generic-lens or generic-optics with 'productionVariants' instead." #-}

-- | A timestamp that shows when the endpoint configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsCreationTime :: Lens.Lens' DescribeEndpointConfigResponse Core.NominalDiffTime
decrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED decrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsDataCaptureConfig :: Lens.Lens' DescribeEndpointConfigResponse (Core.Maybe Types.DataCaptureConfig)
decrrsDataCaptureConfig = Lens.field @"dataCaptureConfig"
{-# DEPRECATED decrrsDataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead." #-}

-- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on the ML storage volume attached to the instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsKmsKeyId :: Lens.Lens' DescribeEndpointConfigResponse (Core.Maybe Types.KmsKeyId)
decrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED decrrsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrrsResponseStatus :: Lens.Lens' DescribeEndpointConfigResponse Core.Int
decrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
