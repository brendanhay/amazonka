{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events for one or more accounts in your organization. Information includes standard event data (Region, service, and so on, as returned by <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization> ), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included; to retrieve those, use the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> operation.
--
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master account.
-- When you call the @DescribeEventDetailsForOrganization@ operation, you specify the @organizationEventDetailFilters@ object in the request. Depending on the AWS Health event type, note the following differences:
--
--     * If the event is public, the @awsAccountId@ parameter must be empty. If you specify an account ID for a public event, then an error message is returned. That's because the event might apply to all AWS accounts and isn't specific to an account in your organization.
--
--
--     * If the event is specific to an account, then you must specify the @awsAccountId@ parameter in the request. If you don't specify an account ID, an error message returns because the event is specific to an AWS account in your organization. 
--
--
-- For more information, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> .
module Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
    (
    -- * Creating a request
      DescribeEventDetailsForOrganization (..)
    , mkDescribeEventDetailsForOrganization
    -- ** Request lenses
    , dedfoOrganizationEventDetailFilters
    , dedfoLocale

    -- * Destructuring the response
    , DescribeEventDetailsForOrganizationResponse (..)
    , mkDescribeEventDetailsForOrganizationResponse
    -- ** Response lenses
    , dedforrsFailedSet
    , dedforrsSuccessfulSet
    , dedforrsResponseStatus
    ) where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventDetailsForOrganization' smart constructor.
data DescribeEventDetailsForOrganization = DescribeEventDetailsForOrganization'
  { organizationEventDetailFilters :: Core.NonEmpty Types.EventAccountFilter
    -- ^ A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
  , locale :: Core.Maybe Types.Locale
    -- ^ The locale (language) to return information in. English (en) is the default and the only supported value at this time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventDetailsForOrganization' value with any optional fields omitted.
mkDescribeEventDetailsForOrganization
    :: Core.NonEmpty Types.EventAccountFilter -- ^ 'organizationEventDetailFilters'
    -> DescribeEventDetailsForOrganization
mkDescribeEventDetailsForOrganization
  organizationEventDetailFilters
  = DescribeEventDetailsForOrganization'{organizationEventDetailFilters,
                                         locale = Core.Nothing}

-- | A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
--
-- /Note:/ Consider using 'organizationEventDetailFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedfoOrganizationEventDetailFilters :: Lens.Lens' DescribeEventDetailsForOrganization (Core.NonEmpty Types.EventAccountFilter)
dedfoOrganizationEventDetailFilters = Lens.field @"organizationEventDetailFilters"
{-# INLINEABLE dedfoOrganizationEventDetailFilters #-}
{-# DEPRECATED organizationEventDetailFilters "Use generic-lens or generic-optics with 'organizationEventDetailFilters' instead"  #-}

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedfoLocale :: Lens.Lens' DescribeEventDetailsForOrganization (Core.Maybe Types.Locale)
dedfoLocale = Lens.field @"locale"
{-# INLINEABLE dedfoLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

instance Core.ToQuery DescribeEventDetailsForOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventDetailsForOrganization where
        toHeaders DescribeEventDetailsForOrganization{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSHealth_20160804.DescribeEventDetailsForOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEventDetailsForOrganization where
        toJSON DescribeEventDetailsForOrganization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("organizationEventDetailFilters" Core..=
                       organizationEventDetailFilters),
                  ("locale" Core..=) Core.<$> locale])

instance Core.AWSRequest DescribeEventDetailsForOrganization where
        type Rs DescribeEventDetailsForOrganization =
             DescribeEventDetailsForOrganizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventDetailsForOrganizationResponse' Core.<$>
                   (x Core..:? "failedSet") Core.<*> x Core..:? "successfulSet"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEventDetailsForOrganizationResponse' smart constructor.
data DescribeEventDetailsForOrganizationResponse = DescribeEventDetailsForOrganizationResponse'
  { failedSet :: Core.Maybe [Types.OrganizationEventDetailsErrorItem]
    -- ^ Error messages for any events that could not be retrieved.
  , successfulSet :: Core.Maybe [Types.OrganizationEventDetails]
    -- ^ Information about the events that could be retrieved.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventDetailsForOrganizationResponse' value with any optional fields omitted.
mkDescribeEventDetailsForOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventDetailsForOrganizationResponse
mkDescribeEventDetailsForOrganizationResponse responseStatus
  = DescribeEventDetailsForOrganizationResponse'{failedSet =
                                                   Core.Nothing,
                                                 successfulSet = Core.Nothing, responseStatus}

-- | Error messages for any events that could not be retrieved.
--
-- /Note:/ Consider using 'failedSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedforrsFailedSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Core.Maybe [Types.OrganizationEventDetailsErrorItem])
dedforrsFailedSet = Lens.field @"failedSet"
{-# INLINEABLE dedforrsFailedSet #-}
{-# DEPRECATED failedSet "Use generic-lens or generic-optics with 'failedSet' instead"  #-}

-- | Information about the events that could be retrieved.
--
-- /Note:/ Consider using 'successfulSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedforrsSuccessfulSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Core.Maybe [Types.OrganizationEventDetails])
dedforrsSuccessfulSet = Lens.field @"successfulSet"
{-# INLINEABLE dedforrsSuccessfulSet #-}
{-# DEPRECATED successfulSet "Use generic-lens or generic-optics with 'successfulSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedforrsResponseStatus :: Lens.Lens' DescribeEventDetailsForOrganizationResponse Core.Int
dedforrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dedforrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
