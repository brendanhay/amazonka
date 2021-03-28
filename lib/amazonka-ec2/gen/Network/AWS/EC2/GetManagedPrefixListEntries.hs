{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetManagedPrefixListEntries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the entries for a specified managed prefix list.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListEntries
    (
    -- * Creating a request
      GetManagedPrefixListEntries (..)
    , mkGetManagedPrefixListEntries
    -- ** Request lenses
    , gmplePrefixListId
    , gmpleDryRun
    , gmpleMaxResults
    , gmpleNextToken
    , gmpleTargetVersion

    -- * Destructuring the response
    , GetManagedPrefixListEntriesResponse (..)
    , mkGetManagedPrefixListEntriesResponse
    -- ** Response lenses
    , gmplerrsEntries
    , gmplerrsNextToken
    , gmplerrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetManagedPrefixListEntries' smart constructor.
data GetManagedPrefixListEntries = GetManagedPrefixListEntries'
  { prefixListId :: Types.PrefixListResourceId
    -- ^ The ID of the prefix list.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next page of results.
  , targetVersion :: Core.Maybe Core.Integer
    -- ^ The version of the prefix list for which to return the entries. The default is the current version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetManagedPrefixListEntries' value with any optional fields omitted.
mkGetManagedPrefixListEntries
    :: Types.PrefixListResourceId -- ^ 'prefixListId'
    -> GetManagedPrefixListEntries
mkGetManagedPrefixListEntries prefixListId
  = GetManagedPrefixListEntries'{prefixListId, dryRun = Core.Nothing,
                                 maxResults = Core.Nothing, nextToken = Core.Nothing,
                                 targetVersion = Core.Nothing}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplePrefixListId :: Lens.Lens' GetManagedPrefixListEntries Types.PrefixListResourceId
gmplePrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE gmplePrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleDryRun :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Bool)
gmpleDryRun = Lens.field @"dryRun"
{-# INLINEABLE gmpleDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleMaxResults :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Natural)
gmpleMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gmpleMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleNextToken :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Types.NextToken)
gmpleNextToken = Lens.field @"nextToken"
{-# INLINEABLE gmpleNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The version of the prefix list for which to return the entries. The default is the current version.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmpleTargetVersion :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Integer)
gmpleTargetVersion = Lens.field @"targetVersion"
{-# INLINEABLE gmpleTargetVersion #-}
{-# DEPRECATED targetVersion "Use generic-lens or generic-optics with 'targetVersion' instead"  #-}

instance Core.ToQuery GetManagedPrefixListEntries where
        toQuery GetManagedPrefixListEntries{..}
          = Core.toQueryPair "Action"
              ("GetManagedPrefixListEntries" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrefixListId" prefixListId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetVersion")
                targetVersion

instance Core.ToHeaders GetManagedPrefixListEntries where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetManagedPrefixListEntries where
        type Rs GetManagedPrefixListEntries =
             GetManagedPrefixListEntriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetManagedPrefixListEntriesResponse' Core.<$>
                   (x Core..@? "entrySet" Core..<@> Core.parseXMLList "item") Core.<*>
                     x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetManagedPrefixListEntries where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"entries" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetManagedPrefixListEntriesResponse' smart constructor.
data GetManagedPrefixListEntriesResponse = GetManagedPrefixListEntriesResponse'
  { entries :: Core.Maybe [Types.PrefixListEntry]
    -- ^ Information about the prefix list entries.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetManagedPrefixListEntriesResponse' value with any optional fields omitted.
mkGetManagedPrefixListEntriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetManagedPrefixListEntriesResponse
mkGetManagedPrefixListEntriesResponse responseStatus
  = GetManagedPrefixListEntriesResponse'{entries = Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | Information about the prefix list entries.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplerrsEntries :: Lens.Lens' GetManagedPrefixListEntriesResponse (Core.Maybe [Types.PrefixListEntry])
gmplerrsEntries = Lens.field @"entries"
{-# INLINEABLE gmplerrsEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplerrsNextToken :: Lens.Lens' GetManagedPrefixListEntriesResponse (Core.Maybe Types.NextToken)
gmplerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gmplerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmplerrsResponseStatus :: Lens.Lens' GetManagedPrefixListEntriesResponse Core.Int
gmplerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmplerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
