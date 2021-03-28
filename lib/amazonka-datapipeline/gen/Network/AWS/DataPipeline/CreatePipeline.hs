{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.CreatePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty pipeline. Use 'PutPipelineDefinition' to populate the pipeline.
module Network.AWS.DataPipeline.CreatePipeline
    (
    -- * Creating a request
      CreatePipeline (..)
    , mkCreatePipeline
    -- ** Request lenses
    , cpName
    , cpUniqueId
    , cpDescription
    , cpTags

    -- * Destructuring the response
    , CreatePipelineResponse (..)
    , mkCreatePipelineResponse
    -- ** Response lenses
    , cprrsPipelineId
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreatePipeline.
--
-- /See:/ 'mkCreatePipeline' smart constructor.
data CreatePipeline = CreatePipeline'
  { name :: Types.Id
    -- ^ The name for the pipeline. You can use the same name for multiple pipelines associated with your AWS account, because AWS Data Pipeline assigns each pipeline a unique pipeline identifier.
  , uniqueId :: Types.Id
    -- ^ A unique identifier. This identifier is not the same as the pipeline identifier assigned by AWS Data Pipeline. You are responsible for defining the format and ensuring the uniqueness of this identifier. You use this parameter to ensure idempotency during repeated calls to @CreatePipeline@ . For example, if the first call to @CreatePipeline@ does not succeed, you can pass in the same unique identifier and pipeline name combination on a subsequent call to @CreatePipeline@ . @CreatePipeline@ ensures that if a pipeline already exists with the same name and unique identifier, a new pipeline is not created. Instead, you'll receive the pipeline identifier from the previous attempt. The uniqueness of the name and unique identifier combination is scoped to the AWS account or IAM user credentials.
  , description :: Core.Maybe Core.Text
    -- ^ The description for the pipeline.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags to associate with the pipeline at creation. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipeline' value with any optional fields omitted.
mkCreatePipeline
    :: Types.Id -- ^ 'name'
    -> Types.Id -- ^ 'uniqueId'
    -> CreatePipeline
mkCreatePipeline name uniqueId
  = CreatePipeline'{name, uniqueId, description = Core.Nothing,
                    tags = Core.Nothing}

-- | The name for the pipeline. You can use the same name for multiple pipelines associated with your AWS account, because AWS Data Pipeline assigns each pipeline a unique pipeline identifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePipeline Types.Id
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique identifier. This identifier is not the same as the pipeline identifier assigned by AWS Data Pipeline. You are responsible for defining the format and ensuring the uniqueness of this identifier. You use this parameter to ensure idempotency during repeated calls to @CreatePipeline@ . For example, if the first call to @CreatePipeline@ does not succeed, you can pass in the same unique identifier and pipeline name combination on a subsequent call to @CreatePipeline@ . @CreatePipeline@ ensures that if a pipeline already exists with the same name and unique identifier, a new pipeline is not created. Instead, you'll receive the pipeline identifier from the previous attempt. The uniqueness of the name and unique identifier combination is scoped to the AWS account or IAM user credentials.
--
-- /Note:/ Consider using 'uniqueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUniqueId :: Lens.Lens' CreatePipeline Types.Id
cpUniqueId = Lens.field @"uniqueId"
{-# INLINEABLE cpUniqueId #-}
{-# DEPRECATED uniqueId "Use generic-lens or generic-optics with 'uniqueId' instead"  #-}

-- | The description for the pipeline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePipeline (Core.Maybe Core.Text)
cpDescription = Lens.field @"description"
{-# INLINEABLE cpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of tags to associate with the pipeline at creation. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePipeline (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePipeline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePipeline where
        toHeaders CreatePipeline{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.CreatePipeline") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePipeline where
        toJSON CreatePipeline{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("uniqueId" Core..= uniqueId),
                  ("description" Core..=) Core.<$> description,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePipeline where
        type Rs CreatePipeline = CreatePipelineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' Core.<$>
                   (x Core..: "pipelineId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreatePipeline.
--
-- /See:/ 'mkCreatePipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { pipelineId :: Types.Id
    -- ^ The ID that AWS Data Pipeline assigns the newly created pipeline. For example, @df-06372391ZG65EXAMPLE@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePipelineResponse' value with any optional fields omitted.
mkCreatePipelineResponse
    :: Types.Id -- ^ 'pipelineId'
    -> Core.Int -- ^ 'responseStatus'
    -> CreatePipelineResponse
mkCreatePipelineResponse pipelineId responseStatus
  = CreatePipelineResponse'{pipelineId, responseStatus}

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. For example, @df-06372391ZG65EXAMPLE@ .
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPipelineId :: Lens.Lens' CreatePipelineResponse Types.Id
cprrsPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE cprrsPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePipelineResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
