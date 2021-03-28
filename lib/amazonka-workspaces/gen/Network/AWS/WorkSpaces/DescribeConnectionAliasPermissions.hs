{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of a connection alias has granted to another AWS account for the specified connection alias. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
    (
    -- * Creating a request
      DescribeConnectionAliasPermissions (..)
    , mkDescribeConnectionAliasPermissions
    -- ** Request lenses
    , dcapAliasId
    , dcapMaxResults
    , dcapNextToken

    -- * Destructuring the response
    , DescribeConnectionAliasPermissionsResponse (..)
    , mkDescribeConnectionAliasPermissionsResponse
    -- ** Response lenses
    , dcaprrsAliasId
    , dcaprrsConnectionAliasPermissions
    , dcaprrsNextToken
    , dcaprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeConnectionAliasPermissions' smart constructor.
data DescribeConnectionAliasPermissions = DescribeConnectionAliasPermissions'
  { aliasId :: Types.AliasId
    -- ^ The identifier of the connection alias.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnectionAliasPermissions' value with any optional fields omitted.
mkDescribeConnectionAliasPermissions
    :: Types.AliasId -- ^ 'aliasId'
    -> DescribeConnectionAliasPermissions
mkDescribeConnectionAliasPermissions aliasId
  = DescribeConnectionAliasPermissions'{aliasId,
                                        maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcapAliasId :: Lens.Lens' DescribeConnectionAliasPermissions Types.AliasId
dcapAliasId = Lens.field @"aliasId"
{-# INLINEABLE dcapAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcapMaxResults :: Lens.Lens' DescribeConnectionAliasPermissions (Core.Maybe Core.Natural)
dcapMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcapMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcapNextToken :: Lens.Lens' DescribeConnectionAliasPermissions (Core.Maybe Types.NextToken)
dcapNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcapNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeConnectionAliasPermissions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConnectionAliasPermissions where
        toHeaders DescribeConnectionAliasPermissions{..}
          = Core.pure
              ("X-Amz-Target",
               "WorkspacesService.DescribeConnectionAliasPermissions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConnectionAliasPermissions where
        toJSON DescribeConnectionAliasPermissions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AliasId" Core..= aliasId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeConnectionAliasPermissions where
        type Rs DescribeConnectionAliasPermissions =
             DescribeConnectionAliasPermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConnectionAliasPermissionsResponse' Core.<$>
                   (x Core..:? "AliasId") Core.<*>
                     x Core..:? "ConnectionAliasPermissions"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeConnectionAliasPermissionsResponse' smart constructor.
data DescribeConnectionAliasPermissionsResponse = DescribeConnectionAliasPermissionsResponse'
  { aliasId :: Core.Maybe Types.AliasId
    -- ^ The identifier of the connection alias.
  , connectionAliasPermissions :: Core.Maybe (Core.NonEmpty Types.ConnectionAliasPermission)
    -- ^ The permissions associated with a connection alias.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next set of results, or null if no more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnectionAliasPermissionsResponse' value with any optional fields omitted.
mkDescribeConnectionAliasPermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConnectionAliasPermissionsResponse
mkDescribeConnectionAliasPermissionsResponse responseStatus
  = DescribeConnectionAliasPermissionsResponse'{aliasId =
                                                  Core.Nothing,
                                                connectionAliasPermissions = Core.Nothing,
                                                nextToken = Core.Nothing, responseStatus}

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprrsAliasId :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Core.Maybe Types.AliasId)
dcaprrsAliasId = Lens.field @"aliasId"
{-# INLINEABLE dcaprrsAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | The permissions associated with a connection alias.
--
-- /Note:/ Consider using 'connectionAliasPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprrsConnectionAliasPermissions :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Core.Maybe (Core.NonEmpty Types.ConnectionAliasPermission))
dcaprrsConnectionAliasPermissions = Lens.field @"connectionAliasPermissions"
{-# INLINEABLE dcaprrsConnectionAliasPermissions #-}
{-# DEPRECATED connectionAliasPermissions "Use generic-lens or generic-optics with 'connectionAliasPermissions' instead"  #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprrsNextToken :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Core.Maybe Types.NextToken)
dcaprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcaprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaprrsResponseStatus :: Lens.Lens' DescribeConnectionAliasPermissionsResponse Core.Int
dcaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
