{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateFunction (..),
    mkUpdateFunction,

    -- ** Request lenses
    ufApiId,
    ufName,
    ufFunctionId,
    ufDataSourceName,
    ufFunctionVersion,
    ufDescription,
    ufRequestMappingTemplate,
    ufResponseMappingTemplate,

    -- * Destructuring the response
    UpdateFunctionResponse (..),
    mkUpdateFunctionResponse,

    -- ** Response lenses
    ufrrsFunctionConfiguration,
    ufrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { -- | The GraphQL API ID.
    apiId :: Types.String,
    -- | The @Function@ name.
    name :: Types.ResourceName,
    -- | The function ID.
    functionId :: Types.ResourceName,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Types.ResourceName,
    -- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
    functionVersion :: Types.String,
    -- | The @Function@ description.
    description :: Core.Maybe Types.String,
    -- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The @Function@ request mapping template.
    responseMappingTemplate :: Core.Maybe Types.MappingTemplate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunction' value with any optional fields omitted.
mkUpdateFunction ::
  -- | 'apiId'
  Types.String ->
  -- | 'name'
  Types.ResourceName ->
  -- | 'functionId'
  Types.ResourceName ->
  -- | 'dataSourceName'
  Types.ResourceName ->
  -- | 'functionVersion'
  Types.String ->
  UpdateFunction
mkUpdateFunction
  apiId
  name
  functionId
  dataSourceName
  functionVersion =
    UpdateFunction'
      { apiId,
        name,
        functionId,
        dataSourceName,
        functionVersion,
        description = Core.Nothing,
        requestMappingTemplate = Core.Nothing,
        responseMappingTemplate = Core.Nothing
      }

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufApiId :: Lens.Lens' UpdateFunction Types.String
ufApiId = Lens.field @"apiId"
{-# DEPRECATED ufApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFunction Types.ResourceName
ufName = Lens.field @"name"
{-# DEPRECATED ufName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The function ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionId :: Lens.Lens' UpdateFunction Types.ResourceName
ufFunctionId = Lens.field @"functionId"
{-# DEPRECATED ufFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

-- | The @Function@ @DataSource@ name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDataSourceName :: Lens.Lens' UpdateFunction Types.ResourceName
ufDataSourceName = Lens.field @"dataSourceName"
{-# DEPRECATED ufDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFunctionVersion :: Lens.Lens' UpdateFunction Types.String
ufFunctionVersion = Lens.field @"functionVersion"
{-# DEPRECATED ufFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFunction (Core.Maybe Types.String)
ufDescription = Lens.field @"description"
{-# DEPRECATED ufDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufRequestMappingTemplate :: Lens.Lens' UpdateFunction (Core.Maybe Types.MappingTemplate)
ufRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# DEPRECATED ufRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The @Function@ request mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufResponseMappingTemplate :: Lens.Lens' UpdateFunction (Core.Maybe Types.MappingTemplate)
ufResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# DEPRECATED ufResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

instance Core.FromJSON UpdateFunction where
  toJSON UpdateFunction {..} =
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

instance Core.AWSRequest UpdateFunction where
  type Rs UpdateFunction = UpdateFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/functions/")
                Core.<> (Core.toText functionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFunctionResponse'
            Core.<$> (x Core..:? "functionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Core.Maybe Types.FunctionConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionResponse' value with any optional fields omitted.
mkUpdateFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFunctionResponse
mkUpdateFunctionResponse responseStatus =
  UpdateFunctionResponse'
    { functionConfiguration = Core.Nothing,
      responseStatus
    }

-- | The @Function@ object.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsFunctionConfiguration :: Lens.Lens' UpdateFunctionResponse (Core.Maybe Types.FunctionConfiguration)
ufrrsFunctionConfiguration = Lens.field @"functionConfiguration"
{-# DEPRECATED ufrrsFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsResponseStatus :: Lens.Lens' UpdateFunctionResponse Core.Int
ufrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
