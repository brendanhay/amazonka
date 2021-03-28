{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DescribeBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about backups of AWS CloudHSM clusters.
--
-- This is a paginated operation, which means that each response might contain only a subset of all the backups. When the response contains only a subset of backups, it includes a @NextToken@ value. Use this value in a subsequent @DescribeBackups@ request to get more backups. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more backups to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.DescribeBackups
    (
    -- * Creating a request
      DescribeBackups (..)
    , mkDescribeBackups
    -- ** Request lenses
    , dbFilters
    , dbMaxResults
    , dbNextToken
    , dbSortAscending

    -- * Destructuring the response
    , DescribeBackupsResponse (..)
    , mkDescribeBackupsResponse
    -- ** Response lenses
    , dbrfrsBackups
    , dbrfrsNextToken
    , dbrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { filters :: Core.Maybe (Core.HashMap Types.Field [Core.Text])
    -- ^ One or more filters to limit the items returned in the response.
--
-- Use the @backupIds@ filter to return only the specified backups. Specify backups by their backup identifier (ID).
-- Use the @sourceBackupIds@ filter to return only the backups created from a source backup. The @sourceBackupID@ of a source backup is returned by the 'CopyBackupToRegion' operation.
-- Use the @clusterIds@ filter to return only the backups for the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @states@ filter to return only backups that match the specified state.
-- Use the @neverExpires@ filter to return backups filtered by the value in the @neverExpires@ parameter. @True@ returns all backups exempt from the backup retention policy. @False@ returns all backups with a backup retention policy defined at the cluster.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of backups to return in the response. When there are more backups than the number you specify, the response contains a @NextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @NextToken@ value that you received in the previous response. Use this value to get more backups.
  , sortAscending :: Core.Maybe Core.Bool
    -- ^ Designates whether or not to sort the return backups by ascending chronological order of generation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBackups' value with any optional fields omitted.
mkDescribeBackups
    :: DescribeBackups
mkDescribeBackups
  = DescribeBackups'{filters = Core.Nothing,
                     maxResults = Core.Nothing, nextToken = Core.Nothing,
                     sortAscending = Core.Nothing}

-- | One or more filters to limit the items returned in the response.
--
-- Use the @backupIds@ filter to return only the specified backups. Specify backups by their backup identifier (ID).
-- Use the @sourceBackupIds@ filter to return only the backups created from a source backup. The @sourceBackupID@ of a source backup is returned by the 'CopyBackupToRegion' operation.
-- Use the @clusterIds@ filter to return only the backups for the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @states@ filter to return only backups that match the specified state.
-- Use the @neverExpires@ filter to return backups filtered by the value in the @neverExpires@ parameter. @True@ returns all backups exempt from the backup retention policy. @False@ returns all backups with a backup retention policy defined at the cluster.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbFilters :: Lens.Lens' DescribeBackups (Core.Maybe (Core.HashMap Types.Field [Core.Text]))
dbFilters = Lens.field @"filters"
{-# INLINEABLE dbFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of backups to return in the response. When there are more backups than the number you specify, the response contains a @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbMaxResults :: Lens.Lens' DescribeBackups (Core.Maybe Core.Natural)
dbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @NextToken@ value that you received in the previous response. Use this value to get more backups.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbNextToken :: Lens.Lens' DescribeBackups (Core.Maybe Types.NextToken)
dbNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Designates whether or not to sort the return backups by ascending chronological order of generation.
--
-- /Note:/ Consider using 'sortAscending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSortAscending :: Lens.Lens' DescribeBackups (Core.Maybe Core.Bool)
dbSortAscending = Lens.field @"sortAscending"
{-# INLINEABLE dbSortAscending #-}
{-# DEPRECATED sortAscending "Use generic-lens or generic-optics with 'sortAscending' instead"  #-}

instance Core.ToQuery DescribeBackups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBackups where
        toHeaders DescribeBackups{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.DescribeBackups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBackups where
        toJSON DescribeBackups{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortAscending" Core..=) Core.<$> sortAscending])

instance Core.AWSRequest DescribeBackups where
        type Rs DescribeBackups = DescribeBackupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBackupsResponse' Core.<$>
                   (x Core..:? "Backups") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeBackups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"backups" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { backups :: Core.Maybe [Types.Backup]
    -- ^ A list of backups.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An opaque string that indicates that the response contains only a subset of backups. Use this value in a subsequent @DescribeBackups@ request to get more backups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBackupsResponse' value with any optional fields omitted.
mkDescribeBackupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBackupsResponse
mkDescribeBackupsResponse responseStatus
  = DescribeBackupsResponse'{backups = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | A list of backups.
--
-- /Note:/ Consider using 'backups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrfrsBackups :: Lens.Lens' DescribeBackupsResponse (Core.Maybe [Types.Backup])
dbrfrsBackups = Lens.field @"backups"
{-# INLINEABLE dbrfrsBackups #-}
{-# DEPRECATED backups "Use generic-lens or generic-optics with 'backups' instead"  #-}

-- | An opaque string that indicates that the response contains only a subset of backups. Use this value in a subsequent @DescribeBackups@ request to get more backups.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrfrsNextToken :: Lens.Lens' DescribeBackupsResponse (Core.Maybe Types.NextToken)
dbrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrfrsResponseStatus :: Lens.Lens' DescribeBackupsResponse Core.Int
dbrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
