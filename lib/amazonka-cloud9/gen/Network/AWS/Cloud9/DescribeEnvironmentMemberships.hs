{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentMemberships
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about environment members for an AWS Cloud9 development environment.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.DescribeEnvironmentMemberships
    (
    -- * Creating a request
      DescribeEnvironmentMemberships (..)
    , mkDescribeEnvironmentMemberships
    -- ** Request lenses
    , dEnvironmentId
    , dMaxResults
    , dNextToken
    , dPermissions
    , dUserArn

    -- * Destructuring the response
    , DescribeEnvironmentMembershipsResponse (..)
    , mkDescribeEnvironmentMembershipsResponse
    -- ** Response lenses
    , demrfrsMemberships
    , demrfrsNextToken
    , demrfrsResponseStatus
    ) where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEnvironmentMemberships' smart constructor.
data DescribeEnvironmentMemberships = DescribeEnvironmentMemberships'
  { environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ The ID of the environment to get environment member information about.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of environment members to get information about.
  , nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
  , permissions :: Core.Maybe [Types.Permissions]
    -- ^ The type of environment member permissions to get information about. Available values include:
--
--
--     * @owner@ : Owns the environment.
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
-- If no value is specified, information about all environment members are returned.
  , userArn :: Core.Maybe Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of an individual environment member to get information about. If no value is specified, information about all environment members are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentMemberships' value with any optional fields omitted.
mkDescribeEnvironmentMemberships
    :: DescribeEnvironmentMemberships
mkDescribeEnvironmentMemberships
  = DescribeEnvironmentMemberships'{environmentId = Core.Nothing,
                                    maxResults = Core.Nothing, nextToken = Core.Nothing,
                                    permissions = Core.Nothing, userArn = Core.Nothing}

-- | The ID of the environment to get environment member information about.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnvironmentId :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Types.EnvironmentId)
dEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE dEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The maximum number of environment members to get information about.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Core.Text)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The type of environment member permissions to get information about. Available values include:
--
--
--     * @owner@ : Owns the environment.
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
-- If no value is specified, information about all environment members are returned.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPermissions :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe [Types.Permissions])
dPermissions = Lens.field @"permissions"
{-# INLINEABLE dPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

-- | The Amazon Resource Name (ARN) of an individual environment member to get information about. If no value is specified, information about all environment members are returned.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserArn :: Lens.Lens' DescribeEnvironmentMemberships (Core.Maybe Types.UserArn)
dUserArn = Lens.field @"userArn"
{-# INLINEABLE dUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery DescribeEnvironmentMemberships where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEnvironmentMemberships where
        toHeaders DescribeEnvironmentMemberships{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCloud9WorkspaceManagementService.DescribeEnvironmentMemberships")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEnvironmentMemberships where
        toJSON DescribeEnvironmentMemberships{..}
          = Core.object
              (Core.catMaybes
                 [("environmentId" Core..=) Core.<$> environmentId,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("permissions" Core..=) Core.<$> permissions,
                  ("userArn" Core..=) Core.<$> userArn])

instance Core.AWSRequest DescribeEnvironmentMemberships where
        type Rs DescribeEnvironmentMemberships =
             DescribeEnvironmentMembershipsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEnvironmentMembershipsResponse' Core.<$>
                   (x Core..:? "memberships") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEnvironmentMemberships where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"memberships" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeEnvironmentMembershipsResponse' smart constructor.
data DescribeEnvironmentMembershipsResponse = DescribeEnvironmentMembershipsResponse'
  { memberships :: Core.Maybe [Types.EnvironmentMember]
    -- ^ Information about the environment members for the environment.
  , nextToken :: Core.Maybe Core.Text
    -- ^ If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEnvironmentMembershipsResponse' value with any optional fields omitted.
mkDescribeEnvironmentMembershipsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEnvironmentMembershipsResponse
mkDescribeEnvironmentMembershipsResponse responseStatus
  = DescribeEnvironmentMembershipsResponse'{memberships =
                                              Core.Nothing,
                                            nextToken = Core.Nothing, responseStatus}

-- | Information about the environment members for the environment.
--
-- /Note:/ Consider using 'memberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demrfrsMemberships :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Core.Maybe [Types.EnvironmentMember])
demrfrsMemberships = Lens.field @"memberships"
{-# INLINEABLE demrfrsMemberships #-}
{-# DEPRECATED memberships "Use generic-lens or generic-optics with 'memberships' instead"  #-}

-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demrfrsNextToken :: Lens.Lens' DescribeEnvironmentMembershipsResponse (Core.Maybe Core.Text)
demrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE demrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demrfrsResponseStatus :: Lens.Lens' DescribeEnvironmentMembershipsResponse Core.Int
demrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE demrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
