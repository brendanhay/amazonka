{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListProtections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all 'Protection' objects for the account.
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListProtections
  ( -- * Creating a request
    ListProtections (..),
    mkListProtections,

    -- ** Request lenses
    lpMaxResults,
    lpNextToken,

    -- * Destructuring the response
    ListProtectionsResponse (..),
    mkListProtectionsResponse,

    -- ** Response lenses
    lprrsNextToken,
    lprrsProtections,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkListProtections' smart constructor.
data ListProtections = ListProtections'
  { -- | The maximum number of 'Protection' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProtections' value with any optional fields omitted.
mkListProtections ::
  ListProtections
mkListProtections =
  ListProtections'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of 'Protection' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListProtections (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @ListProtectionsRequest.NextToken@ value from a previous call to @ListProtections@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListProtections (Core.Maybe Types.Token)
lpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListProtections where
  toJSON ListProtections {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListProtections where
  type Rs ListProtections = ListProtectionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSShield_20160616.ListProtections")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Protections")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProtections where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"protections" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListProtectionsResponse' smart constructor.
data ListProtectionsResponse = ListProtectionsResponse'
  { -- | If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
    --
    -- Shield Advanced might return the list of 'Protection' objects in batches smaller than the number specified by MaxResults. If there are more 'Protection' objects to return, Shield Advanced will always also return a @NextToken@ .
    nextToken :: Core.Maybe Types.Token,
    -- | The array of enabled 'Protection' objects.
    protections :: Core.Maybe [Types.Protection],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProtectionsResponse' value with any optional fields omitted.
mkListProtectionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProtectionsResponse
mkListProtectionsResponse responseStatus =
  ListProtectionsResponse'
    { nextToken = Core.Nothing,
      protections = Core.Nothing,
      responseStatus
    }

-- | If you specify a value for @MaxResults@ and you have more Protections than the value of MaxResults, AWS Shield Advanced returns a NextToken value in the response that allows you to list another group of Protections. For the second and subsequent ListProtections requests, specify the value of NextToken from the previous response to get information about another batch of Protections.
--
-- Shield Advanced might return the list of 'Protection' objects in batches smaller than the number specified by MaxResults. If there are more 'Protection' objects to return, Shield Advanced will always also return a @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListProtectionsResponse (Core.Maybe Types.Token)
lprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The array of enabled 'Protection' objects.
--
-- /Note:/ Consider using 'protections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsProtections :: Lens.Lens' ListProtectionsResponse (Core.Maybe [Types.Protection])
lprrsProtections = Lens.field @"protections"
{-# DEPRECATED lprrsProtections "Use generic-lens or generic-optics with 'protections' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListProtectionsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
