{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a @Function@ .
module Network.AWS.AppSync.GetFunction
  ( -- * Creating a request
    GetFunction (..),
    mkGetFunction,

    -- ** Request lenses
    gfApiId,
    gfFunctionId,

    -- * Destructuring the response
    GetFunctionResponse (..),
    mkGetFunctionResponse,

    -- ** Response lenses
    gfrrsFunctionConfiguration,
    gfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFunction' smart constructor.
data GetFunction = GetFunction'
  { -- | The GraphQL API ID.
    apiId :: Types.ApiId,
    -- | The @Function@ ID.
    functionId :: Types.FunctionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunction' value with any optional fields omitted.
mkGetFunction ::
  -- | 'apiId'
  Types.ApiId ->
  -- | 'functionId'
  Types.FunctionId ->
  GetFunction
mkGetFunction apiId functionId = GetFunction' {apiId, functionId}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfApiId :: Lens.Lens' GetFunction Types.ApiId
gfApiId = Lens.field @"apiId"
{-# DEPRECATED gfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The @Function@ ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFunctionId :: Lens.Lens' GetFunction Types.FunctionId
gfFunctionId = Lens.field @"functionId"
{-# DEPRECATED gfFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

instance Core.AWSRequest GetFunction where
  type Rs GetFunction = GetFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
    Response.receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            Core.<$> (x Core..:? "functionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Core.Maybe Types.FunctionConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionResponse' value with any optional fields omitted.
mkGetFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFunctionResponse
mkGetFunctionResponse responseStatus =
  GetFunctionResponse'
    { functionConfiguration = Core.Nothing,
      responseStatus
    }

-- | The @Function@ object.
--
-- /Note:/ Consider using 'functionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsFunctionConfiguration :: Lens.Lens' GetFunctionResponse (Core.Maybe Types.FunctionConfiguration)
gfrrsFunctionConfiguration = Lens.field @"functionConfiguration"
{-# DEPRECATED gfrrsFunctionConfiguration "Use generic-lens or generic-optics with 'functionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFunctionResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
