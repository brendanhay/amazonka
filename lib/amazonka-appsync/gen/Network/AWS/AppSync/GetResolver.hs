{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Resolver@ object.
module Network.AWS.AppSync.GetResolver
  ( -- * Creating a request
    GetResolver (..),
    mkGetResolver,

    -- ** Request lenses
    grApiId,
    grTypeName,
    grFieldName,

    -- * Destructuring the response
    GetResolverResponse (..),
    mkGetResolverResponse,

    -- ** Response lenses
    grrrsResolver,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResolver' smart constructor.
data GetResolver = GetResolver'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The resolver type name.
    typeName :: Types.ResourceName,
    -- | The resolver field name.
    fieldName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResolver' value with any optional fields omitted.
mkGetResolver ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  -- | 'fieldName'
  Types.ResourceName ->
  GetResolver
mkGetResolver apiId typeName fieldName =
  GetResolver' {apiId, typeName, fieldName}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grApiId :: Lens.Lens' GetResolver Types.String
grApiId = Lens.field @"apiId"
{-# DEPRECATED grApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The resolver type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grTypeName :: Lens.Lens' GetResolver Types.ResourceName
grTypeName = Lens.field @"typeName"
{-# DEPRECATED grTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grFieldName :: Lens.Lens' GetResolver Types.ResourceName
grFieldName = Lens.field @"fieldName"
{-# DEPRECATED grFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Core.AWSRequest GetResolver where
  type Rs GetResolver = GetResolverResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
                Core.<> ("/resolvers/")
                Core.<> (Core.toText fieldName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverResponse'
            Core.<$> (x Core..:? "resolver") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetResolverResponse' smart constructor.
data GetResolverResponse = GetResolverResponse'
  { -- | The @Resolver@ object.
    resolver :: Core.Maybe Types.Resolver,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResolverResponse' value with any optional fields omitted.
mkGetResolverResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResolverResponse
mkGetResolverResponse responseStatus =
  GetResolverResponse' {resolver = Core.Nothing, responseStatus}

-- | The @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResolver :: Lens.Lens' GetResolverResponse (Core.Maybe Types.Resolver)
grrrsResolver = Lens.field @"resolver"
{-# DEPRECATED grrrsResolver "Use generic-lens or generic-optics with 'resolver' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetResolverResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
