{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Function@ .
module Network.AWS.AppSync.DeleteFunction
  ( -- * Creating a request
    DeleteFunction (..),
    mkDeleteFunction,

    -- ** Request lenses
    dfApiId,
    dfFunctionId,

    -- * Destructuring the response
    DeleteFunctionResponse (..),
    mkDeleteFunctionResponse,

    -- ** Response lenses
    dfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { -- | The GraphQL API ID.
    apiId :: Types.String,
    -- | The @Function@ ID.
    functionId :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunction' value with any optional fields omitted.
mkDeleteFunction ::
  -- | 'apiId'
  Types.String ->
  -- | 'functionId'
  Types.ResourceName ->
  DeleteFunction
mkDeleteFunction apiId functionId =
  DeleteFunction' {apiId, functionId}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfApiId :: Lens.Lens' DeleteFunction Types.String
dfApiId = Lens.field @"apiId"
{-# DEPRECATED dfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFunctionId :: Lens.Lens' DeleteFunction Types.ResourceName
dfFunctionId = Lens.field @"functionId"
{-# DEPRECATED dfFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

instance Core.AWSRequest DeleteFunction where
  type Rs DeleteFunction = DeleteFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/functions/")
                Core.<> (Core.toText functionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFunctionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFunctionResponse' smart constructor.
newtype DeleteFunctionResponse = DeleteFunctionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionResponse' value with any optional fields omitted.
mkDeleteFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFunctionResponse
mkDeleteFunctionResponse responseStatus =
  DeleteFunctionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFunctionResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
