{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListSecurityKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all security keys associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityKeys
  ( -- * Creating a request
    ListSecurityKeys (..),
    mkListSecurityKeys,

    -- ** Request lenses
    lskInstanceId,
    lskMaxResults,
    lskNextToken,

    -- * Destructuring the response
    ListSecurityKeysResponse (..),
    mkListSecurityKeysResponse,

    -- ** Response lenses
    lskrrsNextToken,
    lskrrsSecurityKeys,
    lskrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSecurityKeys' value with any optional fields omitted.
mkListSecurityKeys ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListSecurityKeys
mkListSecurityKeys instanceId =
  ListSecurityKeys'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskInstanceId :: Lens.Lens' ListSecurityKeys Types.InstanceId
lskInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lskInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskMaxResults :: Lens.Lens' ListSecurityKeys (Core.Maybe Core.Natural)
lskMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lskMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskNextToken :: Lens.Lens' ListSecurityKeys (Core.Maybe Types.NextToken)
lskNextToken = Lens.field @"nextToken"
{-# DEPRECATED lskNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListSecurityKeys where
  type Rs ListSecurityKeys = ListSecurityKeysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/security-keys")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityKeysResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SecurityKeys")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSecurityKeys where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"securityKeys" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSecurityKeysResponse' smart constructor.
data ListSecurityKeysResponse = ListSecurityKeysResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The security keys.
    securityKeys :: Core.Maybe [Types.SecurityKey],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSecurityKeysResponse' value with any optional fields omitted.
mkListSecurityKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSecurityKeysResponse
mkListSecurityKeysResponse responseStatus =
  ListSecurityKeysResponse'
    { nextToken = Core.Nothing,
      securityKeys = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrrsNextToken :: Lens.Lens' ListSecurityKeysResponse (Core.Maybe Types.NextToken)
lskrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lskrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The security keys.
--
-- /Note:/ Consider using 'securityKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrrsSecurityKeys :: Lens.Lens' ListSecurityKeysResponse (Core.Maybe [Types.SecurityKey])
lskrrsSecurityKeys = Lens.field @"securityKeys"
{-# DEPRECATED lskrrsSecurityKeys "Use generic-lens or generic-optics with 'securityKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrrsResponseStatus :: Lens.Lens' ListSecurityKeysResponse Core.Int
lskrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lskrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
