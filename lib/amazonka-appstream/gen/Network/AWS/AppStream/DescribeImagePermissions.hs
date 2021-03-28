{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the permissions for shared AWS account IDs on a private image that you own. 
module Network.AWS.AppStream.DescribeImagePermissions
    (
    -- * Creating a request
      DescribeImagePermissions (..)
    , mkDescribeImagePermissions
    -- ** Request lenses
    , dipsName
    , dipsMaxResults
    , dipsNextToken
    , dipsSharedAwsAccountIds

    -- * Destructuring the response
    , DescribeImagePermissionsResponse (..)
    , mkDescribeImagePermissionsResponse
    -- ** Response lenses
    , diprfrsName
    , diprfrsNextToken
    , diprfrsSharedImagePermissionsList
    , diprfrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImagePermissions' smart constructor.
data DescribeImagePermissions = DescribeImagePermissions'
  { name :: Types.Name
    -- ^ The name of the private image for which to describe permissions. The image must be one that you own. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of each page of results.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
  , sharedAwsAccountIds :: Core.Maybe (Core.NonEmpty Types.AwsAccountId)
    -- ^ The 12-digit identifier of one or more AWS accounts with which the image is shared.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImagePermissions' value with any optional fields omitted.
mkDescribeImagePermissions
    :: Types.Name -- ^ 'name'
    -> DescribeImagePermissions
mkDescribeImagePermissions name
  = DescribeImagePermissions'{name, maxResults = Core.Nothing,
                              nextToken = Core.Nothing, sharedAwsAccountIds = Core.Nothing}

-- | The name of the private image for which to describe permissions. The image must be one that you own. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsName :: Lens.Lens' DescribeImagePermissions Types.Name
dipsName = Lens.field @"name"
{-# INLINEABLE dipsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsMaxResults :: Lens.Lens' DescribeImagePermissions (Core.Maybe Core.Natural)
dipsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dipsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsNextToken :: Lens.Lens' DescribeImagePermissions (Core.Maybe Core.Text)
dipsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dipsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The 12-digit identifier of one or more AWS accounts with which the image is shared.
--
-- /Note:/ Consider using 'sharedAwsAccountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsSharedAwsAccountIds :: Lens.Lens' DescribeImagePermissions (Core.Maybe (Core.NonEmpty Types.AwsAccountId))
dipsSharedAwsAccountIds = Lens.field @"sharedAwsAccountIds"
{-# INLINEABLE dipsSharedAwsAccountIds #-}
{-# DEPRECATED sharedAwsAccountIds "Use generic-lens or generic-optics with 'sharedAwsAccountIds' instead"  #-}

instance Core.ToQuery DescribeImagePermissions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeImagePermissions where
        toHeaders DescribeImagePermissions{..}
          = Core.pure
              ("X-Amz-Target",
               "PhotonAdminProxyService.DescribeImagePermissions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeImagePermissions where
        toJSON DescribeImagePermissions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SharedAwsAccountIds" Core..=) Core.<$> sharedAwsAccountIds])

instance Core.AWSRequest DescribeImagePermissions where
        type Rs DescribeImagePermissions = DescribeImagePermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeImagePermissionsResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> x Core..:? "NextToken" Core.<*>
                     x Core..:? "SharedImagePermissionsList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeImagePermissionsResponse' smart constructor.
data DescribeImagePermissionsResponse = DescribeImagePermissionsResponse'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the private image.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
  , sharedImagePermissionsList :: Core.Maybe [Types.SharedImagePermissions]
    -- ^ The permissions for a private image that you own. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImagePermissionsResponse' value with any optional fields omitted.
mkDescribeImagePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeImagePermissionsResponse
mkDescribeImagePermissionsResponse responseStatus
  = DescribeImagePermissionsResponse'{name = Core.Nothing,
                                      nextToken = Core.Nothing,
                                      sharedImagePermissionsList = Core.Nothing, responseStatus}

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsName :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe Types.Name)
diprfrsName = Lens.field @"name"
{-# INLINEABLE diprfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsNextToken :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe Core.Text)
diprfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diprfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The permissions for a private image that you own. 
--
-- /Note:/ Consider using 'sharedImagePermissionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsSharedImagePermissionsList :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe [Types.SharedImagePermissions])
diprfrsSharedImagePermissionsList = Lens.field @"sharedImagePermissionsList"
{-# INLINEABLE diprfrsSharedImagePermissionsList #-}
{-# DEPRECATED sharedImagePermissionsList "Use generic-lens or generic-optics with 'sharedImagePermissionsList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsResponseStatus :: Lens.Lens' DescribeImagePermissionsResponse Core.Int
diprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
