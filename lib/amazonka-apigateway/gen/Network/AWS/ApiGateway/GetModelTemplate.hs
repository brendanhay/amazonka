{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetModelTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a sample mapping template that can be used to transform a payload into the structure of a model.
module Network.AWS.ApiGateway.GetModelTemplate
    (
    -- * Creating a request
      GetModelTemplate (..)
    , mkGetModelTemplate
    -- ** Request lenses
    , gmtRestApiId
    , gmtModelName

    -- * Destructuring the response
    , GetModelTemplateResponse (..)
    , mkGetModelTemplateResponse
    -- ** Response lenses
    , gmtrrsValue
    , gmtrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to generate a sample mapping template used to transform the payload.
--
-- /See:/ 'mkGetModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , modelName :: Core.Text
    -- ^ [Required] The name of the model for which to generate a template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetModelTemplate' value with any optional fields omitted.
mkGetModelTemplate
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'modelName'
    -> GetModelTemplate
mkGetModelTemplate restApiId modelName
  = GetModelTemplate'{restApiId, modelName}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtRestApiId :: Lens.Lens' GetModelTemplate Core.Text
gmtRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gmtRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the model for which to generate a template.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtModelName :: Lens.Lens' GetModelTemplate Core.Text
gmtModelName = Lens.field @"modelName"
{-# INLINEABLE gmtModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

instance Core.ToQuery GetModelTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetModelTemplate where
        toHeaders GetModelTemplate{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetModelTemplate where
        type Rs GetModelTemplate = GetModelTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/models/"
                             Core.<> Core.toText modelName
                             Core.<> "/default_template",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetModelTemplateResponse' Core.<$>
                   (x Core..:? "value") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents a mapping template used to transform a payload.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings Mapping Templates> 
--
-- /See:/ 'mkGetModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { value :: Core.Maybe Core.Text
    -- ^ The Apache <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)> template content used for the template resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetModelTemplateResponse' value with any optional fields omitted.
mkGetModelTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetModelTemplateResponse
mkGetModelTemplateResponse responseStatus
  = GetModelTemplateResponse'{value = Core.Nothing, responseStatus}

-- | The Apache <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)> template content used for the template resource.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtrrsValue :: Lens.Lens' GetModelTemplateResponse (Core.Maybe Core.Text)
gmtrrsValue = Lens.field @"value"
{-# INLINEABLE gmtrrsValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtrrsResponseStatus :: Lens.Lens' GetModelTemplateResponse Core.Int
gmtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
