{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user import jobs.
module Network.AWS.CognitoIdentityProvider.ListUserImportJobs
  ( -- * Creating a request
    ListUserImportJobs (..),
    mkListUserImportJobs,

    -- ** Request lenses
    luijUserPoolId,
    luijMaxResults,
    luijPaginationToken,

    -- * Destructuring the response
    ListUserImportJobsResponse (..),
    mkListUserImportJobsResponse,

    -- ** Response lenses
    luijrrsPaginationToken,
    luijrrsUserImportJobs,
    luijrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the user import jobs.
--
-- /See:/ 'mkListUserImportJobs' smart constructor.
data ListUserImportJobs = ListUserImportJobs'
  { -- | The user pool ID for the user pool that the users are being imported into.
    userPoolId :: Types.UserPoolIdType,
    -- | The maximum number of import jobs you want the request to return.
    maxResults :: Core.Natural,
    -- | An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
    paginationToken :: Core.Maybe Types.PaginationKeyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserImportJobs' value with any optional fields omitted.
mkListUserImportJobs ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  -- | 'maxResults'
  Core.Natural ->
  ListUserImportJobs
mkListUserImportJobs userPoolId maxResults =
  ListUserImportJobs'
    { userPoolId,
      maxResults,
      paginationToken = Core.Nothing
    }

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijUserPoolId :: Lens.Lens' ListUserImportJobs Types.UserPoolIdType
luijUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED luijUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The maximum number of import jobs you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijMaxResults :: Lens.Lens' ListUserImportJobs Core.Natural
luijMaxResults = Lens.field @"maxResults"
{-# DEPRECATED luijMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijPaginationToken :: Lens.Lens' ListUserImportJobs (Core.Maybe Types.PaginationKeyType)
luijPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED luijPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

instance Core.FromJSON ListUserImportJobs where
  toJSON ListUserImportJobs {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("MaxResults" Core..= maxResults),
            ("PaginationToken" Core..=) Core.<$> paginationToken
          ]
      )

instance Core.AWSRequest ListUserImportJobs where
  type Rs ListUserImportJobs = ListUserImportJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.ListUserImportJobs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserImportJobsResponse'
            Core.<$> (x Core..:? "PaginationToken")
            Core.<*> (x Core..:? "UserImportJobs")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server to the request to list the user import jobs.
--
-- /See:/ 'mkListUserImportJobsResponse' smart constructor.
data ListUserImportJobsResponse = ListUserImportJobsResponse'
  { -- | An identifier that can be used to return the next set of user import jobs in the list.
    paginationToken :: Core.Maybe Types.PaginationKeyType,
    -- | The user import jobs.
    userImportJobs :: Core.Maybe (Core.NonEmpty Types.UserImportJobType),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListUserImportJobsResponse' value with any optional fields omitted.
mkListUserImportJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListUserImportJobsResponse
mkListUserImportJobsResponse responseStatus =
  ListUserImportJobsResponse'
    { paginationToken = Core.Nothing,
      userImportJobs = Core.Nothing,
      responseStatus
    }

-- | An identifier that can be used to return the next set of user import jobs in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrrsPaginationToken :: Lens.Lens' ListUserImportJobsResponse (Core.Maybe Types.PaginationKeyType)
luijrrsPaginationToken = Lens.field @"paginationToken"
{-# DEPRECATED luijrrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The user import jobs.
--
-- /Note:/ Consider using 'userImportJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrrsUserImportJobs :: Lens.Lens' ListUserImportJobsResponse (Core.Maybe (Core.NonEmpty Types.UserImportJobType))
luijrrsUserImportJobs = Lens.field @"userImportJobs"
{-# DEPRECATED luijrrsUserImportJobs "Use generic-lens or generic-optics with 'userImportJobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrrsResponseStatus :: Lens.Lens' ListUserImportJobsResponse Core.Int
luijrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED luijrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
