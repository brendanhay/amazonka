{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Type@ object.
module Network.AWS.AppSync.GetType
  ( -- * Creating a request
    GetType (..),
    mkGetType,

    -- ** Request lenses
    gtApiId,
    gtTypeName,
    gtFormat,

    -- * Destructuring the response
    GetTypeResponse (..),
    mkGetTypeResponse,

    -- ** Response lenses
    gtrrsType,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetType' smart constructor.
data GetType = GetType'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The type name.
    typeName :: Types.ResourceName,
    -- | The type format: SDL or JSON.
    format :: Types.TypeDefinitionFormat
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetType' value with any optional fields omitted.
mkGetType ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  -- | 'format'
  Types.TypeDefinitionFormat ->
  GetType
mkGetType apiId typeName format = GetType' {apiId, typeName, format}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtApiId :: Lens.Lens' GetType Types.String
gtApiId = Lens.field @"apiId"
{-# DEPRECATED gtApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTypeName :: Lens.Lens' GetType Types.ResourceName
gtTypeName = Lens.field @"typeName"
{-# DEPRECATED gtTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtFormat :: Lens.Lens' GetType Types.TypeDefinitionFormat
gtFormat = Lens.field @"format"
{-# DEPRECATED gtFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Core.AWSRequest GetType where
  type Rs GetType = GetTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
            ),
        Core._rqQuery = Core.toQueryValue "format" format,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTypeResponse'
            Core.<$> (x Core..:? "type") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTypeResponse' smart constructor.
data GetTypeResponse = GetTypeResponse'
  { -- | The @Type@ object.
    type' :: Core.Maybe Types.Type,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTypeResponse' value with any optional fields omitted.
mkGetTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTypeResponse
mkGetTypeResponse responseStatus =
  GetTypeResponse' {type' = Core.Nothing, responseStatus}

-- | The @Type@ object.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsType :: Lens.Lens' GetTypeResponse (Core.Maybe Types.Type)
gtrrsType = Lens.field @"type'"
{-# DEPRECATED gtrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTypeResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
