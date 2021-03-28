{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the endpoint configuration specified in the request. Amazon SageMaker uses the endpoint to provision resources and deploy models. You create the endpoint configuration with the 'CreateEndpointConfig' API. 
--
-- Use this API to deploy models using Amazon SageMaker hosting services. 
-- For an example that calls this method when deploying a model to Amazon SageMaker hosting services, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ex1-deploy-model.html#ex1-deploy-model-boto Deploy the Model to Amazon SageMaker Hosting Services (AWS SDK for Python (Boto 3)).> 
-- The endpoint name must be unique within an AWS Region in your AWS account. 
-- When it receives the request, Amazon SageMaker creates the endpoint, launches the resources (ML compute instances), and deploys the model(s) on them. 
-- When Amazon SageMaker receives the request, it sets the endpoint status to @Creating@ . After it creates the endpoint, it sets the status to @InService@ . Amazon SageMaker can then process incoming requests for inferences. To check the status of an endpoint, use the 'DescribeEndpoint' API.
-- If any of the models hosted at this endpoint get model data from an Amazon S3 location, Amazon SageMaker uses AWS Security Token Service to download model artifacts from the S3 path you provided. AWS STS is activated in your IAM user account by default. If you previously deactivated AWS STS for a region, you need to reactivate AWS STS for that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
module Network.AWS.SageMaker.CreateEndpoint
    (
    -- * Creating a request
      CreateEndpoint (..)
    , mkCreateEndpoint
    -- ** Request lenses
    , ceEndpointName
    , ceEndpointConfigName
    , ceTags

    -- * Destructuring the response
    , CreateEndpointResponse (..)
    , mkCreateEndpointResponse
    -- ** Response lenses
    , cerfrsEndpointArn
    , cerfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { endpointName :: Types.EndpointName
    -- ^ The name of the endpoint.The name must be unique within an AWS Region in your AWS account. The name is case-insensitive in @CreateEndpoint@ , but the case is preserved and must be matched in .
  , endpointConfigName :: Types.EndpointConfigName
    -- ^ The name of an endpoint configuration. For more information, see 'CreateEndpointConfig' . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpoint' value with any optional fields omitted.
mkCreateEndpoint
    :: Types.EndpointName -- ^ 'endpointName'
    -> Types.EndpointConfigName -- ^ 'endpointConfigName'
    -> CreateEndpoint
mkCreateEndpoint endpointName endpointConfigName
  = CreateEndpoint'{endpointName, endpointConfigName,
                    tags = Core.Nothing}

-- | The name of the endpoint.The name must be unique within an AWS Region in your AWS account. The name is case-insensitive in @CreateEndpoint@ , but the case is preserved and must be matched in .
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointName :: Lens.Lens' CreateEndpoint Types.EndpointName
ceEndpointName = Lens.field @"endpointName"
{-# INLINEABLE ceEndpointName #-}
{-# DEPRECATED endpointName "Use generic-lens or generic-optics with 'endpointName' instead"  #-}

-- | The name of an endpoint configuration. For more information, see 'CreateEndpointConfig' . 
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEndpointConfigName :: Lens.Lens' CreateEndpoint Types.EndpointConfigName
ceEndpointConfigName = Lens.field @"endpointConfigName"
{-# INLINEABLE ceEndpointConfigName #-}
{-# DEPRECATED endpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead"  #-}

-- | An array of key-value pairs. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ . 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTags :: Lens.Lens' CreateEndpoint (Core.Maybe [Types.Tag])
ceTags = Lens.field @"tags"
{-# INLINEABLE ceTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEndpoint where
        toHeaders CreateEndpoint{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateEndpoint") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateEndpoint where
        toJSON CreateEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointName" Core..= endpointName),
                  Core.Just ("EndpointConfigName" Core..= endpointConfigName),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateEndpoint where
        type Rs CreateEndpoint = CreateEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateEndpointResponse' Core.<$>
                   (x Core..: "EndpointArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { endpointArn :: Types.EndpointArn
    -- ^ The Amazon Resource Name (ARN) of the endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndpointResponse' value with any optional fields omitted.
mkCreateEndpointResponse
    :: Types.EndpointArn -- ^ 'endpointArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateEndpointResponse
mkCreateEndpointResponse endpointArn responseStatus
  = CreateEndpointResponse'{endpointArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerfrsEndpointArn :: Lens.Lens' CreateEndpointResponse Types.EndpointArn
cerfrsEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE cerfrsEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerfrsResponseStatus :: Lens.Lens' CreateEndpointResponse Core.Int
cerfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cerfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
