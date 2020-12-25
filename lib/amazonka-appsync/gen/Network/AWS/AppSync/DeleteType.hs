{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Type@ object.
module Network.AWS.AppSync.DeleteType
  ( -- * Creating a request
    DeleteType (..),
    mkDeleteType,

    -- ** Request lenses
    dtApiId,
    dtTypeName,

    -- * Destructuring the response
    DeleteTypeResponse (..),
    mkDeleteTypeResponse,

    -- ** Response lenses
    dtrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteType' smart constructor.
data DeleteType = DeleteType'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The type name.
    typeName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteType' value with any optional fields omitted.
mkDeleteType ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  DeleteType
mkDeleteType apiId typeName = DeleteType' {apiId, typeName}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtApiId :: Lens.Lens' DeleteType Types.String
dtApiId = Lens.field @"apiId"
{-# DEPRECATED dtApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTypeName :: Lens.Lens' DeleteType Types.ResourceName
dtTypeName = Lens.field @"typeName"
{-# DEPRECATED dtTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Core.AWSRequest DeleteType where
  type Rs DeleteType = DeleteTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTypeResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTypeResponse' smart constructor.
newtype DeleteTypeResponse = DeleteTypeResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTypeResponse' value with any optional fields omitted.
mkDeleteTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTypeResponse
mkDeleteTypeResponse responseStatus =
  DeleteTypeResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DeleteTypeResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
