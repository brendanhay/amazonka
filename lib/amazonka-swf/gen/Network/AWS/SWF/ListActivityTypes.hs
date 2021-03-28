{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListActivityTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all activities registered in the specified domain that match the specified name and registration status. The result includes information like creation date, current status of the activity, etc. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the @nextPageToken@ returned by the initial call.
--
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListActivityTypes
    (
    -- * Creating a request
      ListActivityTypes (..)
    , mkListActivityTypes
    -- ** Request lenses
    , latDomain
    , latRegistrationStatus
    , latMaximumPageSize
    , latName
    , latNextPageToken
    , latReverseOrder

    -- * Destructuring the response
    , ListActivityTypesResponse (..)
    , mkListActivityTypesResponse
    -- ** Response lenses
    , latrrsTypeInfos
    , latrrsNextPageToken
    , latrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkListActivityTypes' smart constructor.
data ListActivityTypes = ListActivityTypes'
  { domain :: Types.DomainName
    -- ^ The name of the domain in which the activity types have been registered.
  , registrationStatus :: Types.RegistrationStatus
    -- ^ Specifies the registration status of the activity types to list.
  , maximumPageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results. 
  , name :: Core.Maybe Types.Name
    -- ^ If specified, only lists the activity types that have this name.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ". 
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call. 
  , reverseOrder :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActivityTypes' value with any optional fields omitted.
mkListActivityTypes
    :: Types.DomainName -- ^ 'domain'
    -> Types.RegistrationStatus -- ^ 'registrationStatus'
    -> ListActivityTypes
mkListActivityTypes domain registrationStatus
  = ListActivityTypes'{domain, registrationStatus,
                       maximumPageSize = Core.Nothing, name = Core.Nothing,
                       nextPageToken = Core.Nothing, reverseOrder = Core.Nothing}

-- | The name of the domain in which the activity types have been registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latDomain :: Lens.Lens' ListActivityTypes Types.DomainName
latDomain = Lens.field @"domain"
{-# INLINEABLE latDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specifies the registration status of the activity types to list.
--
-- /Note:/ Consider using 'registrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latRegistrationStatus :: Lens.Lens' ListActivityTypes Types.RegistrationStatus
latRegistrationStatus = Lens.field @"registrationStatus"
{-# INLINEABLE latRegistrationStatus #-}
{-# DEPRECATED registrationStatus "Use generic-lens or generic-optics with 'registrationStatus' instead"  #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results. 
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latMaximumPageSize :: Lens.Lens' ListActivityTypes (Core.Maybe Core.Natural)
latMaximumPageSize = Lens.field @"maximumPageSize"
{-# INLINEABLE latMaximumPageSize #-}
{-# DEPRECATED maximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead"  #-}

-- | If specified, only lists the activity types that have this name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latName :: Lens.Lens' ListActivityTypes (Core.Maybe Types.Name)
latName = Lens.field @"name"
{-# INLINEABLE latName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ". 
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call. 
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latNextPageToken :: Lens.Lens' ListActivityTypes (Core.Maybe Types.NextPageToken)
latNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE latNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latReverseOrder :: Lens.Lens' ListActivityTypes (Core.Maybe Core.Bool)
latReverseOrder = Lens.field @"reverseOrder"
{-# INLINEABLE latReverseOrder #-}
{-# DEPRECATED reverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead"  #-}

instance Core.ToQuery ListActivityTypes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListActivityTypes where
        toHeaders ListActivityTypes{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.ListActivityTypes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON ListActivityTypes where
        toJSON ListActivityTypes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("registrationStatus" Core..= registrationStatus),
                  ("maximumPageSize" Core..=) Core.<$> maximumPageSize,
                  ("name" Core..=) Core.<$> name,
                  ("nextPageToken" Core..=) Core.<$> nextPageToken,
                  ("reverseOrder" Core..=) Core.<$> reverseOrder])

instance Core.AWSRequest ListActivityTypes where
        type Rs ListActivityTypes = ListActivityTypesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListActivityTypesResponse' Core.<$>
                   (x Core..:? "typeInfos" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListActivityTypes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"typeInfos") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextPageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | Contains a paginated list of activity type information structures.
--
-- /See:/ 'mkListActivityTypesResponse' smart constructor.
data ListActivityTypesResponse = ListActivityTypesResponse'
  { typeInfos :: [Types.ActivityTypeInfo]
    -- ^ List of activity type information.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListActivityTypesResponse' value with any optional fields omitted.
mkListActivityTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListActivityTypesResponse
mkListActivityTypesResponse responseStatus
  = ListActivityTypesResponse'{typeInfos = Core.mempty,
                               nextPageToken = Core.Nothing, responseStatus}

-- | List of activity type information.
--
-- /Note:/ Consider using 'typeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsTypeInfos :: Lens.Lens' ListActivityTypesResponse [Types.ActivityTypeInfo]
latrrsTypeInfos = Lens.field @"typeInfos"
{-# INLINEABLE latrrsTypeInfos #-}
{-# DEPRECATED typeInfos "Use generic-lens or generic-optics with 'typeInfos' instead"  #-}

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsNextPageToken :: Lens.Lens' ListActivityTypesResponse (Core.Maybe Types.NextPageToken)
latrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE latrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latrrsResponseStatus :: Lens.Lens' ListActivityTypesResponse Core.Int
latrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE latrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
