{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Resolver@ object.
module Network.AWS.AppSync.DeleteResolver
  ( -- * Creating a request
    DeleteResolver (..),
    mkDeleteResolver,

    -- ** Request lenses
    drApiId,
    drTypeName,
    drFieldName,

    -- * Destructuring the response
    DeleteResolverResponse (..),
    mkDeleteResolverResponse,

    -- ** Response lenses
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResolver' smart constructor.
data DeleteResolver = DeleteResolver'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The name of the resolver type.
    typeName :: Types.ResourceName,
    -- | The resolver field name.
    fieldName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResolver' value with any optional fields omitted.
mkDeleteResolver ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  -- | 'fieldName'
  Types.ResourceName ->
  DeleteResolver
mkDeleteResolver apiId typeName fieldName =
  DeleteResolver' {apiId, typeName, fieldName}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drApiId :: Lens.Lens' DeleteResolver Types.String
drApiId = Lens.field @"apiId"
{-# DEPRECATED drApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the resolver type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drTypeName :: Lens.Lens' DeleteResolver Types.ResourceName
drTypeName = Lens.field @"typeName"
{-# DEPRECATED drTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drFieldName :: Lens.Lens' DeleteResolver Types.ResourceName
drFieldName = Lens.field @"fieldName"
{-# DEPRECATED drFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Core.AWSRequest DeleteResolver where
  type Rs DeleteResolver = DeleteResolverResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
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
    Response.receiveEmpty
      ( \s h x ->
          DeleteResolverResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteResolverResponse' smart constructor.
newtype DeleteResolverResponse = DeleteResolverResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResolverResponse' value with any optional fields omitted.
mkDeleteResolverResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteResolverResponse
mkDeleteResolverResponse responseStatus =
  DeleteResolverResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteResolverResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
