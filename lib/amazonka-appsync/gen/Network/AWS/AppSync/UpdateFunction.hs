{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Function@ object.
module Network.AWS.AppSync.UpdateFunction
    (
    -- * Creating a request
      UpdateFunction (..)
    , mkUpdateFunction
    -- ** Request lenses
    , ufApiId
    , ufName
    , ufFunctionId
    , ufDataSourceName
    , ufFunctionVersion
    , ufDescription
    , ufRequestMappingTemplate
    , ufResponseMappingTemplate

    -- * Destructuring the response
    , UpdateFunctionResponse (..)
    , mkUpdateFunctionResponse
    -- ** Response lenses
    , ufrrsFunctionConfiguration
    , ufrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { apiId :: Core.Text
    -- ^ The GraphQL API ID.
  , name :: Types.ResourceName
    -- ^ The @Function@ name.
  , functionId :: Types.ResourceName
    -- ^ The function ID.
  , dataSourceName :: Types.ResourceName
    -- ^ The @Function@ @DataSource@ name.
  , functionVersion :: Core.Text
    -- ^ The @version@ of the request mapping template. Currently the supported value is 2018-05-29. 
  , description :: Core.Maybe Core.Text
    -- ^ The @Function@ description.
  , requestMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
  , responseMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The @Function@ request mapping template. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunction' value with any optional fields omitted.
mkUpdateFunction
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'name'
    -> Types.ResourceName -- ^ 'functionId'
    -> Types.ResourceName -- ^ 'dataSourceName'
    -> Core.Text -- ^ 'functionVersion'
    -> UpdateFunction
mkUpdateFunction apiId name functionId dataSourceName
  functionVersion
  = UpdateFunction'{apiId, name, functionId, dataSourceName,
                    functionVersion, description = Core.Nothing,
                    requestMappingTemplate = Core.Nothing,
                    responseMappingTemplate = Core.Nothing}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufApiId :: Lens.Lens' UpdateFunction Core.Text
ufApiId = Lens.field @"apiId"
{-# INLINEABLE ufApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The @Function@ name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFunction Types.ResourceName
ufName = Lens.field @"name"
{-# INLINEABLE ufName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The function ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionId :: Lens.Lens' UpdateFunction Types.ResourceName
ufFunctionId = Lens.field @"functionId"
{-# INLINEABLE ufFunctionId #-}
{-# DEPRECATED functionId "Use generic-lens or generic-optics with 'functionId' instead"  #-}

-- | The @Function@ @DataSource@ name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDataSourceName :: Lens.Lens' UpdateFunction Types.ResourceName
ufDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE ufDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29. 
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionVersion :: Lens.Lens' UpdateFunction Core.Text
ufFunctionVersion = Lens.field @"functionVersion"
{-# INLINEABLE ufFunctionVersion #-}
{-# DEPRECATED functionVersion "Use generic-lens or generic-optics with 'functionVersion' instead"  #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFunction (Core.Maybe Core.Text)
ufDescription = Lens.field @"description"
{-# INLINEABLE ufDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufRequestMappingTemplate :: Lens.Lens' UpdateFunction (Core.Maybe Types.MappingTemplate)
ufRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# INLINEABLE ufRequestMappingTemplate #-}
{-# DEPRECATED requestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead"  #-}

-- | The @Function@ request mapping template. 
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufResponseMappingTemplate :: Lens.Lens' UpdateFunction (Core.Maybe Types.MappingTemplate)
ufResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# INLINEABLE ufResponseMappingTemplate #-}
{-# DEPRECATED responseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead"  #-}

instance Core.ToQuery UpdateFunction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFunction where
        toHeaders UpdateFunction{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateFunction where
        toJSON UpdateFunction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("dataSourceName" Core..= dataSourceName),
                  Core.Just ("functionVersion" Core..= functionVersion),
                  ("description" Core..=) Core.<$> description,
                  ("requestMappingTemplate" Core..=) Core.<$> requestMappingTemplate,
                  ("responseMappingTemplate" Core..=) Core.<$>
                    responseMappingTemplate])

instance Core.AWSRequest UpdateFunction where
        type Rs UpdateFunction = UpdateFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/functions/" Core.<>
                             Core.toText functionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateFunctionResponse' Core.<$>
                   (x Core..:? "functionConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
  { functionConfiguration :: Core.Maybe Types.FunctionConfiguration
    -- ^ The @Function@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionResponse' value with any optional fields omitted.
mkUpdateFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateFunctionResponse
mkUpdateFunctionResponse responseStatus
  = UpdateFunctionResponse'{functionConfiguration = Core.Nothing,
                            responseStatus}

-- | The @Function@ object.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsFunctionConfiguration :: Lens.Lens' UpdateFunctionResponse (Core.Maybe Types.FunctionConfiguration)
ufrrsFunctionConfiguration = Lens.field @"functionConfiguration"
{-# INLINEABLE ufrrsFunctionConfiguration #-}
{-# DEPRECATED functionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsResponseStatus :: Lens.Lens' UpdateFunctionResponse Core.Int
ufrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ufrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
