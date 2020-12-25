{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint configuration that Amazon SageMaker hosting services uses to deploy models. In the configuration, you identify one or more models, created using the @CreateModel@ API, to deploy and the resources that you want Amazon SageMaker to provision. Then you call the 'CreateEndpoint' API.
--
-- In the request, you define a @ProductionVariant@ , for each model that you want to deploy. Each @ProductionVariant@ parameter also describes the resources that you want Amazon SageMaker to provision. This includes the number and type of ML compute instances to deploy.
-- If you are hosting multiple models, you also assign a @VariantWeight@ to specify how much traffic you want to allocate to each model. For example, suppose that you want to host two models, A and B, and you assign traffic weight 2 for model A and 1 for model B. Amazon SageMaker distributes two-thirds of the traffic to Model A, and one-third to model B.
-- For an example that calls this method when deploying a model to Amazon SageMaker hosting services, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).>
module Network.AWS.SageMaker.CreateEndpointConfig
  ( -- * Creating a request
    CreateEndpointConfig (..),
    mkCreateEndpointConfig,

    -- ** Request lenses
    cecEndpointConfigName,
    cecProductionVariants,
    cecDataCaptureConfig,
    cecKmsKeyId,
    cecTags,

    -- * Destructuring the response
    CreateEndpointConfigResponse (..),
    mkCreateEndpointConfigResponse,

    -- ** Response lenses
    cecrrsEndpointConfigArn,
    cecrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateEndpointConfig' smart constructor.
data CreateEndpointConfig = CreateEndpointConfig'
  { -- | The name of the endpoint configuration. You specify this name in a 'CreateEndpoint' request.
    endpointConfigName :: Types.EndpointConfigName,
    -- | An list of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
    productionVariants :: Core.NonEmpty Types.ProductionVariant,
    dataCaptureConfig :: Core.Maybe Types.DataCaptureConfig,
    -- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance that hosts the endpoint.
    --
    -- The KmsKeyId can be any of the following formats:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Alias name: @alias/ExampleAlias@
    --
    --
    --     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@
    --
    --
    -- The KMS key policy must grant permission to the IAM role that you specify in your @CreateEndpoint@ , @UpdateEndpoint@ requests. For more information, refer to the AWS Key Management Service section<https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS >
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | A list of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointConfig' value with any optional fields omitted.
mkCreateEndpointConfig ::
  -- | 'endpointConfigName'
  Types.EndpointConfigName ->
  -- | 'productionVariants'
  Core.NonEmpty Types.ProductionVariant ->
  CreateEndpointConfig
mkCreateEndpointConfig endpointConfigName productionVariants =
  CreateEndpointConfig'
    { endpointConfigName,
      productionVariants,
      dataCaptureConfig = Core.Nothing,
      kmsKeyId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the endpoint configuration. You specify this name in a 'CreateEndpoint' request.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecEndpointConfigName :: Lens.Lens' CreateEndpointConfig Types.EndpointConfigName
cecEndpointConfigName = Lens.field @"endpointConfigName"
{-# DEPRECATED cecEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | An list of @ProductionVariant@ objects, one for each model that you want to host at this endpoint.
--
-- /Note:/ Consider using 'productionVariants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecProductionVariants :: Lens.Lens' CreateEndpointConfig (Core.NonEmpty Types.ProductionVariant)
cecProductionVariants = Lens.field @"productionVariants"
{-# DEPRECATED cecProductionVariants "Use generic-lens or generic-optics with 'productionVariants' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dataCaptureConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecDataCaptureConfig :: Lens.Lens' CreateEndpointConfig (Core.Maybe Types.DataCaptureConfig)
cecDataCaptureConfig = Lens.field @"dataCaptureConfig"
{-# DEPRECATED cecDataCaptureConfig "Use generic-lens or generic-optics with 'dataCaptureConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to the ML compute instance that hosts the endpoint.
--
-- The KmsKeyId can be any of the following formats:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Alias name: @alias/ExampleAlias@
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@
--
--
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateEndpoint@ , @UpdateEndpoint@ requests. For more information, refer to the AWS Key Management Service section<https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS >
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecKmsKeyId :: Lens.Lens' CreateEndpointConfig (Core.Maybe Types.KmsKeyId)
cecKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cecKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecTags :: Lens.Lens' CreateEndpointConfig (Core.Maybe [Types.Tag])
cecTags = Lens.field @"tags"
{-# DEPRECATED cecTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateEndpointConfig where
  toJSON CreateEndpointConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointConfigName" Core..= endpointConfigName),
            Core.Just ("ProductionVariants" Core..= productionVariants),
            ("DataCaptureConfig" Core..=) Core.<$> dataCaptureConfig,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateEndpointConfig where
  type Rs CreateEndpointConfig = CreateEndpointConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateEndpointConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointConfigResponse'
            Core.<$> (x Core..: "EndpointConfigArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEndpointConfigResponse' smart constructor.
data CreateEndpointConfigResponse = CreateEndpointConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigArn :: Types.EndpointConfigArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointConfigResponse' value with any optional fields omitted.
mkCreateEndpointConfigResponse ::
  -- | 'endpointConfigArn'
  Types.EndpointConfigArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateEndpointConfigResponse
mkCreateEndpointConfigResponse endpointConfigArn responseStatus =
  CreateEndpointConfigResponse' {endpointConfigArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecrrsEndpointConfigArn :: Lens.Lens' CreateEndpointConfigResponse Types.EndpointConfigArn
cecrrsEndpointConfigArn = Lens.field @"endpointConfigArn"
{-# DEPRECATED cecrrsEndpointConfigArn "Use generic-lens or generic-optics with 'endpointConfigArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cecrrsResponseStatus :: Lens.Lens' CreateEndpointConfigResponse Core.Int
cecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
