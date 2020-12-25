{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetParameterHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history of all changes to a parameter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetParameterHistory
  ( -- * Creating a request
    GetParameterHistory (..),
    mkGetParameterHistory,

    -- ** Request lenses
    gphName,
    gphMaxResults,
    gphNextToken,
    gphWithDecryption,

    -- * Destructuring the response
    GetParameterHistoryResponse (..),
    mkGetParameterHistoryResponse,

    -- ** Response lenses
    gphrrsNextToken,
    gphrrsParameters,
    gphrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetParameterHistory' smart constructor.
data GetParameterHistory = GetParameterHistory'
  { -- | The name of the parameter for which you want to review history.
    name :: Types.PSParameterName,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
    withDecryption :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetParameterHistory' value with any optional fields omitted.
mkGetParameterHistory ::
  -- | 'name'
  Types.PSParameterName ->
  GetParameterHistory
mkGetParameterHistory name =
  GetParameterHistory'
    { name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      withDecryption = Core.Nothing
    }

-- | The name of the parameter for which you want to review history.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphName :: Lens.Lens' GetParameterHistory Types.PSParameterName
gphName = Lens.field @"name"
{-# DEPRECATED gphName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphMaxResults :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Natural)
gphMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gphMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphNextToken :: Lens.Lens' GetParameterHistory (Core.Maybe Types.NextToken)
gphNextToken = Lens.field @"nextToken"
{-# DEPRECATED gphNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Return decrypted values for secure string parameters. This flag is ignored for String and StringList parameter types.
--
-- /Note:/ Consider using 'withDecryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphWithDecryption :: Lens.Lens' GetParameterHistory (Core.Maybe Core.Bool)
gphWithDecryption = Lens.field @"withDecryption"
{-# DEPRECATED gphWithDecryption "Use generic-lens or generic-optics with 'withDecryption' instead." #-}

instance Core.FromJSON GetParameterHistory where
  toJSON GetParameterHistory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("WithDecryption" Core..=) Core.<$> withDecryption
          ]
      )

instance Core.AWSRequest GetParameterHistory where
  type Rs GetParameterHistory = GetParameterHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetParameterHistory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterHistoryResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Parameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetParameterHistory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetParameterHistoryResponse' smart constructor.
data GetParameterHistoryResponse = GetParameterHistoryResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of parameters returned by the request.
    parameters :: Core.Maybe [Types.ParameterHistory],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetParameterHistoryResponse' value with any optional fields omitted.
mkGetParameterHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetParameterHistoryResponse
mkGetParameterHistoryResponse responseStatus =
  GetParameterHistoryResponse'
    { nextToken = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrrsNextToken :: Lens.Lens' GetParameterHistoryResponse (Core.Maybe Types.NextToken)
gphrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gphrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters returned by the request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrrsParameters :: Lens.Lens' GetParameterHistoryResponse (Core.Maybe [Types.ParameterHistory])
gphrrsParameters = Lens.field @"parameters"
{-# DEPRECATED gphrrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gphrrsResponseStatus :: Lens.Lens' GetParameterHistoryResponse Core.Int
gphrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gphrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
