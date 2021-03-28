{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListUserImportJobs (..)
    , mkListUserImportJobs
    -- ** Request lenses
    , luijUserPoolId
    , luijMaxResults
    , luijPaginationToken

    -- * Destructuring the response
    , ListUserImportJobsResponse (..)
    , mkListUserImportJobsResponse
    -- ** Response lenses
    , luijrrsPaginationToken
    , luijrrsUserImportJobs
    , luijrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the user import jobs.
--
-- /See:/ 'mkListUserImportJobs' smart constructor.
data ListUserImportJobs = ListUserImportJobs'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID for the user pool that the users are being imported into.
  , maxResults :: Core.Natural
    -- ^ The maximum number of import jobs you want the request to return.
  , paginationToken :: Core.Maybe Types.PaginationKeyType
    -- ^ An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUserImportJobs' value with any optional fields omitted.
mkListUserImportJobs
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Core.Natural -- ^ 'maxResults'
    -> ListUserImportJobs
mkListUserImportJobs userPoolId maxResults
  = ListUserImportJobs'{userPoolId, maxResults,
                        paginationToken = Core.Nothing}

-- | The user pool ID for the user pool that the users are being imported into.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijUserPoolId :: Lens.Lens' ListUserImportJobs Types.UserPoolIdType
luijUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE luijUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The maximum number of import jobs you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijMaxResults :: Lens.Lens' ListUserImportJobs Core.Natural
luijMaxResults = Lens.field @"maxResults"
{-# INLINEABLE luijMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An identifier that was returned from the previous call to @ListUserImportJobs@ , which can be used to return the next set of import jobs in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijPaginationToken :: Lens.Lens' ListUserImportJobs (Core.Maybe Types.PaginationKeyType)
luijPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE luijPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

instance Core.ToQuery ListUserImportJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListUserImportJobs where
        toHeaders ListUserImportJobs{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ListUserImportJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListUserImportJobs where
        toJSON ListUserImportJobs{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("MaxResults" Core..= maxResults),
                  ("PaginationToken" Core..=) Core.<$> paginationToken])

instance Core.AWSRequest ListUserImportJobs where
        type Rs ListUserImportJobs = ListUserImportJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListUserImportJobsResponse' Core.<$>
                   (x Core..:? "PaginationToken") Core.<*> x Core..:? "UserImportJobs"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to the request to list the user import jobs.
--
-- /See:/ 'mkListUserImportJobsResponse' smart constructor.
data ListUserImportJobsResponse = ListUserImportJobsResponse'
  { paginationToken :: Core.Maybe Types.PaginationKeyType
    -- ^ An identifier that can be used to return the next set of user import jobs in the list.
  , userImportJobs :: Core.Maybe (Core.NonEmpty Types.UserImportJobType)
    -- ^ The user import jobs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListUserImportJobsResponse' value with any optional fields omitted.
mkListUserImportJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListUserImportJobsResponse
mkListUserImportJobsResponse responseStatus
  = ListUserImportJobsResponse'{paginationToken = Core.Nothing,
                                userImportJobs = Core.Nothing, responseStatus}

-- | An identifier that can be used to return the next set of user import jobs in the list.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrrsPaginationToken :: Lens.Lens' ListUserImportJobsResponse (Core.Maybe Types.PaginationKeyType)
luijrrsPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE luijrrsPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

-- | The user import jobs.
--
-- /Note:/ Consider using 'userImportJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrrsUserImportJobs :: Lens.Lens' ListUserImportJobsResponse (Core.Maybe (Core.NonEmpty Types.UserImportJobType))
luijrrsUserImportJobs = Lens.field @"userImportJobs"
{-# INLINEABLE luijrrsUserImportJobs #-}
{-# DEPRECATED userImportJobs "Use generic-lens or generic-optics with 'userImportJobs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luijrrsResponseStatus :: Lens.Lens' ListUserImportJobsResponse Core.Int
luijrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE luijrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
