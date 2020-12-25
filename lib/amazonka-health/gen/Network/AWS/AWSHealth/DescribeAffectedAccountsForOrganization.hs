{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of accounts in the organization from AWS Organizations that are affected by the provided event. For more information about the different types of AWS Health events, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> .
--
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master account.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization
  ( -- * Creating a request
    DescribeAffectedAccountsForOrganization (..),
    mkDescribeAffectedAccountsForOrganization,

    -- ** Request lenses
    daafoEventArn,
    daafoMaxResults,
    daafoNextToken,

    -- * Destructuring the response
    DescribeAffectedAccountsForOrganizationResponse (..),
    mkDescribeAffectedAccountsForOrganizationResponse,

    -- ** Response lenses
    daaforrsAffectedAccounts,
    daaforrsEventScopeCode,
    daaforrsNextToken,
    daaforrsResponseStatus,
  )
where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAffectedAccountsForOrganization' smart constructor.
data DescribeAffectedAccountsForOrganization = DescribeAffectedAccountsForOrganization'
  { -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Types.EventArn,
    -- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAffectedAccountsForOrganization' value with any optional fields omitted.
mkDescribeAffectedAccountsForOrganization ::
  -- | 'eventArn'
  Types.EventArn ->
  DescribeAffectedAccountsForOrganization
mkDescribeAffectedAccountsForOrganization eventArn =
  DescribeAffectedAccountsForOrganization'
    { eventArn,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daafoEventArn :: Lens.Lens' DescribeAffectedAccountsForOrganization Types.EventArn
daafoEventArn = Lens.field @"eventArn"
{-# DEPRECATED daafoEventArn "Use generic-lens or generic-optics with 'eventArn' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daafoMaxResults :: Lens.Lens' DescribeAffectedAccountsForOrganization (Core.Maybe Core.Natural)
daafoMaxResults = Lens.field @"maxResults"
{-# DEPRECATED daafoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daafoNextToken :: Lens.Lens' DescribeAffectedAccountsForOrganization (Core.Maybe Types.NextToken)
daafoNextToken = Lens.field @"nextToken"
{-# DEPRECATED daafoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAffectedAccountsForOrganization where
  toJSON DescribeAffectedAccountsForOrganization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("eventArn" Core..= eventArn),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAffectedAccountsForOrganization where
  type
    Rs DescribeAffectedAccountsForOrganization =
      DescribeAffectedAccountsForOrganizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSHealth_20160804.DescribeAffectedAccountsForOrganization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedAccountsForOrganizationResponse'
            Core.<$> (x Core..:? "affectedAccounts")
            Core.<*> (x Core..:? "eventScopeCode")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAffectedAccountsForOrganization where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"affectedAccounts" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAffectedAccountsForOrganizationResponse' smart constructor.
data DescribeAffectedAccountsForOrganizationResponse = DescribeAffectedAccountsForOrganizationResponse'
  { -- | A JSON set of elements of the affected accounts.
    affectedAccounts :: Core.Maybe [Types.AccountId],
    -- | This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
    --
    --
    --     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.
    --
    --
    --     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.
    --
    --
    --     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
    eventScopeCode :: Core.Maybe Types.EventScopeCode,
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAffectedAccountsForOrganizationResponse' value with any optional fields omitted.
mkDescribeAffectedAccountsForOrganizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAffectedAccountsForOrganizationResponse
mkDescribeAffectedAccountsForOrganizationResponse responseStatus =
  DescribeAffectedAccountsForOrganizationResponse'
    { affectedAccounts =
        Core.Nothing,
      eventScopeCode = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A JSON set of elements of the affected accounts.
--
-- /Note:/ Consider using 'affectedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforrsAffectedAccounts :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Core.Maybe [Types.AccountId])
daaforrsAffectedAccounts = Lens.field @"affectedAccounts"
{-# DEPRECATED daaforrsAffectedAccounts "Use generic-lens or generic-optics with 'affectedAccounts' instead." #-}

-- | This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
--
--
--     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.
--
--
--     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.
--
--
--     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
--
--
--
-- /Note:/ Consider using 'eventScopeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforrsEventScopeCode :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Core.Maybe Types.EventScopeCode)
daaforrsEventScopeCode = Lens.field @"eventScopeCode"
{-# DEPRECATED daaforrsEventScopeCode "Use generic-lens or generic-optics with 'eventScopeCode' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforrsNextToken :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Core.Maybe Types.NextToken)
daaforrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daaforrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforrsResponseStatus :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse Core.Int
daaforrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daaforrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
