{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Function@ object.
--
-- A function is a reusable entity. Multiple functions can be used to compose the resolver logic.
module Network.AWS.AppSync.CreateFunction
  ( -- * Creating a request
    CreateFunction (..),
    mkCreateFunction,

    -- ** Request lenses
    cfApiId,
    cfName,
    cfDataSourceName,
    cfFunctionVersion,
    cfDescription,
    cfRequestMappingTemplate,
    cfResponseMappingTemplate,

    -- * Destructuring the response
    CreateFunctionResponse (..),
    mkCreateFunctionResponse,

    -- ** Response lenses
    cfrrsFunctionConfiguration,
    cfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | The GraphQL API ID.
    apiId :: Types.ApiId,
    -- | The @Function@ name. The function name does not have to be unique.
    name :: Types.Name,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Types.DataSourceName,
    -- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
    functionVersion :: Types.FunctionVersion,
    -- | The @Function@ description.
    description :: Core.Maybe Types.Description,
    -- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The @Function@ response mapping template.
    responseMappingTemplate :: Core.Maybe Types.MappingTemplate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFunction' value with any optional fields omitted.
mkCreateFunction ::
  -- | 'apiId'
  Types.ApiId ->
  -- | 'name'
  Types.Name ->
  -- | 'dataSourceName'
  Types.DataSourceName ->
  -- | 'functionVersion'
  Types.FunctionVersion ->
  CreateFunction
mkCreateFunction apiId name dataSourceName functionVersion =
  CreateFunction'
    { apiId,
      name,
      dataSourceName,
      functionVersion,
      description = Core.Nothing,
      requestMappingTemplate = Core.Nothing,
      responseMappingTemplate = Core.Nothing
    }

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfApiId :: Lens.Lens' CreateFunction Types.ApiId
cfApiId = Lens.field @"apiId"
{-# DEPRECATED cfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ name. The function name does not have to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFunction Types.Name
cfName = Lens.field @"name"
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @Function@ @DataSource@ name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDataSourceName :: Lens.Lens' CreateFunction Types.DataSourceName
cfDataSourceName = Lens.field @"dataSourceName"
{-# DEPRECATED cfDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFunctionVersion :: Lens.Lens' CreateFunction Types.FunctionVersion
cfFunctionVersion = Lens.field @"functionVersion"
{-# DEPRECATED cfFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFunction (Core.Maybe Types.Description)
cfDescription = Lens.field @"description"
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRequestMappingTemplate :: Lens.Lens' CreateFunction (Core.Maybe Types.MappingTemplate)
cfRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# DEPRECATED cfRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The @Function@ response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfResponseMappingTemplate :: Lens.Lens' CreateFunction (Core.Maybe Types.MappingTemplate)
cfResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# DEPRECATED cfResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

instance Core.FromJSON CreateFunction where
  toJSON CreateFunction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("dataSourceName" Core..= dataSourceName),
            Core.Just ("functionVersion" Core..= functionVersion),
            ("description" Core..=) Core.<$> description,
            ("requestMappingTemplate" Core..=) Core.<$> requestMappingTemplate,
            ("responseMappingTemplate" Core..=)
              Core.<$> responseMappingTemplate
          ]
      )

instance Core.AWSRequest CreateFunction where
  type Rs CreateFunction = CreateFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/functions")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionResponse'
            Core.<$> (x Core..:? "functionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFunctionResponse' smart constructor.
data CreateFunctionResponse = CreateFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Core.Maybe Types.FunctionConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFunctionResponse' value with any optional fields omitted.
mkCreateFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFunctionResponse
mkCreateFunctionResponse responseStatus =
  CreateFunctionResponse'
    { functionConfiguration = Core.Nothing,
      responseStatus
    }

-- | The @Function@ object.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsFunctionConfiguration :: Lens.Lens' CreateFunctionResponse (Core.Maybe Types.FunctionConfiguration)
cfrrsFunctionConfiguration = Lens.field @"functionConfiguration"
{-# DEPRECATED cfrrsFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFunctionResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
