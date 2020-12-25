{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeImagePermissions (..),
    mkDescribeImagePermissions,

    -- ** Request lenses
    dipsName,
    dipsMaxResults,
    dipsNextToken,
    dipsSharedAwsAccountIds,

    -- * Destructuring the response
    DescribeImagePermissionsResponse (..),
    mkDescribeImagePermissionsResponse,

    -- ** Response lenses
    diprfrsName,
    diprfrsNextToken,
    diprfrsSharedImagePermissionsList,
    diprfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImagePermissions' smart constructor.
data DescribeImagePermissions = DescribeImagePermissions'
  { -- | The name of the private image for which to describe permissions. The image must be one that you own.
    name :: Types.Name,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String,
    -- | The 12-digit identifier of one or more AWS accounts with which the image is shared.
    sharedAwsAccountIds :: Core.Maybe (Core.NonEmpty Types.AwsAccountId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImagePermissions' value with any optional fields omitted.
mkDescribeImagePermissions ::
  -- | 'name'
  Types.Name ->
  DescribeImagePermissions
mkDescribeImagePermissions name =
  DescribeImagePermissions'
    { name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sharedAwsAccountIds = Core.Nothing
    }

-- | The name of the private image for which to describe permissions. The image must be one that you own.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsName :: Lens.Lens' DescribeImagePermissions Types.Name
dipsName = Lens.field @"name"
{-# DEPRECATED dipsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsMaxResults :: Lens.Lens' DescribeImagePermissions (Core.Maybe Core.Natural)
dipsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dipsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsNextToken :: Lens.Lens' DescribeImagePermissions (Core.Maybe Types.String)
dipsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dipsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The 12-digit identifier of one or more AWS accounts with which the image is shared.
--
-- /Note:/ Consider using 'sharedAwsAccountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsSharedAwsAccountIds :: Lens.Lens' DescribeImagePermissions (Core.Maybe (Core.NonEmpty Types.AwsAccountId))
dipsSharedAwsAccountIds = Lens.field @"sharedAwsAccountIds"
{-# DEPRECATED dipsSharedAwsAccountIds "Use generic-lens or generic-optics with 'sharedAwsAccountIds' instead." #-}

instance Core.FromJSON DescribeImagePermissions where
  toJSON DescribeImagePermissions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SharedAwsAccountIds" Core..=) Core.<$> sharedAwsAccountIds
          ]
      )

instance Core.AWSRequest DescribeImagePermissions where
  type Rs DescribeImagePermissions = DescribeImagePermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "PhotonAdminProxyService.DescribeImagePermissions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagePermissionsResponse'
            Core.<$> (x Core..:? "Name")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SharedImagePermissionsList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeImagePermissionsResponse' smart constructor.
data DescribeImagePermissionsResponse = DescribeImagePermissionsResponse'
  { -- | The name of the private image.
    name :: Core.Maybe Types.Name,
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | The permissions for a private image that you own.
    sharedImagePermissionsList :: Core.Maybe [Types.SharedImagePermissions],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImagePermissionsResponse' value with any optional fields omitted.
mkDescribeImagePermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImagePermissionsResponse
mkDescribeImagePermissionsResponse responseStatus =
  DescribeImagePermissionsResponse'
    { name = Core.Nothing,
      nextToken = Core.Nothing,
      sharedImagePermissionsList = Core.Nothing,
      responseStatus
    }

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsName :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe Types.Name)
diprfrsName = Lens.field @"name"
{-# DEPRECATED diprfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsNextToken :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe Types.String)
diprfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED diprfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The permissions for a private image that you own.
--
-- /Note:/ Consider using 'sharedImagePermissionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsSharedImagePermissionsList :: Lens.Lens' DescribeImagePermissionsResponse (Core.Maybe [Types.SharedImagePermissions])
diprfrsSharedImagePermissionsList = Lens.field @"sharedImagePermissionsList"
{-# DEPRECATED diprfrsSharedImagePermissionsList "Use generic-lens or generic-optics with 'sharedImagePermissionsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprfrsResponseStatus :: Lens.Lens' DescribeImagePermissionsResponse Core.Int
diprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
