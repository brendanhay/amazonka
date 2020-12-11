{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    daafoNextToken,
    daafoMaxResults,
    daafoEventARN,

    -- * Destructuring the response
    DescribeAffectedAccountsForOrganizationResponse (..),
    mkDescribeAffectedAccountsForOrganizationResponse,

    -- ** Response lenses
    daaforsAffectedAccounts,
    daaforsEventScopeCode,
    daaforsNextToken,
    daaforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAffectedAccountsForOrganization' smart constructor.
data DescribeAffectedAccountsForOrganization = DescribeAffectedAccountsForOrganization'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    eventARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAffectedAccountsForOrganization' with the minimum fields required to make a request.
--
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
mkDescribeAffectedAccountsForOrganization ::
  -- | 'eventARN'
  Lude.Text ->
  DescribeAffectedAccountsForOrganization
mkDescribeAffectedAccountsForOrganization pEventARN_ =
  DescribeAffectedAccountsForOrganization'
    { nextToken =
        Lude.Nothing,
      maxResults = Lude.Nothing,
      eventARN = pEventARN_
    }

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daafoNextToken :: Lens.Lens' DescribeAffectedAccountsForOrganization (Lude.Maybe Lude.Text)
daafoNextToken = Lens.lens (nextToken :: DescribeAffectedAccountsForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAffectedAccountsForOrganization)
{-# DEPRECATED daafoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daafoMaxResults :: Lens.Lens' DescribeAffectedAccountsForOrganization (Lude.Maybe Lude.Natural)
daafoMaxResults = Lens.lens (maxResults :: DescribeAffectedAccountsForOrganization -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAffectedAccountsForOrganization)
{-# DEPRECATED daafoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daafoEventARN :: Lens.Lens' DescribeAffectedAccountsForOrganization Lude.Text
daafoEventARN = Lens.lens (eventARN :: DescribeAffectedAccountsForOrganization -> Lude.Text) (\s a -> s {eventARN = a} :: DescribeAffectedAccountsForOrganization)
{-# DEPRECATED daafoEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

instance Page.AWSPager DescribeAffectedAccountsForOrganization where
  page rq rs
    | Page.stop (rs Lens.^. daaforsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daaforsAffectedAccounts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daafoNextToken Lens..~ rs Lens.^. daaforsNextToken

instance Lude.AWSRequest DescribeAffectedAccountsForOrganization where
  type
    Rs DescribeAffectedAccountsForOrganization =
      DescribeAffectedAccountsForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAffectedAccountsForOrganizationResponse'
            Lude.<$> (x Lude..?> "affectedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "eventScopeCode")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAffectedAccountsForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.DescribeAffectedAccountsForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAffectedAccountsForOrganization where
  toJSON DescribeAffectedAccountsForOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("eventArn" Lude..= eventARN)
          ]
      )

instance Lude.ToPath DescribeAffectedAccountsForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAffectedAccountsForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAffectedAccountsForOrganizationResponse' smart constructor.
data DescribeAffectedAccountsForOrganizationResponse = DescribeAffectedAccountsForOrganizationResponse'
  { affectedAccounts ::
      Lude.Maybe
        [Lude.Text],
    eventScopeCode ::
      Lude.Maybe
        EventScopeCode,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeAffectedAccountsForOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'affectedAccounts' - A JSON set of elements of the affected accounts.
-- * 'eventScopeCode' - This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
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
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'responseStatus' - The response status code.
mkDescribeAffectedAccountsForOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAffectedAccountsForOrganizationResponse
mkDescribeAffectedAccountsForOrganizationResponse pResponseStatus_ =
  DescribeAffectedAccountsForOrganizationResponse'
    { affectedAccounts =
        Lude.Nothing,
      eventScopeCode = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JSON set of elements of the affected accounts.
--
-- /Note:/ Consider using 'affectedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforsAffectedAccounts :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Lude.Maybe [Lude.Text])
daaforsAffectedAccounts = Lens.lens (affectedAccounts :: DescribeAffectedAccountsForOrganizationResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {affectedAccounts = a} :: DescribeAffectedAccountsForOrganizationResponse)
{-# DEPRECATED daaforsAffectedAccounts "Use generic-lens or generic-optics with 'affectedAccounts' instead." #-}

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
daaforsEventScopeCode :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Lude.Maybe EventScopeCode)
daaforsEventScopeCode = Lens.lens (eventScopeCode :: DescribeAffectedAccountsForOrganizationResponse -> Lude.Maybe EventScopeCode) (\s a -> s {eventScopeCode = a} :: DescribeAffectedAccountsForOrganizationResponse)
{-# DEPRECATED daaforsEventScopeCode "Use generic-lens or generic-optics with 'eventScopeCode' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforsNextToken :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse (Lude.Maybe Lude.Text)
daaforsNextToken = Lens.lens (nextToken :: DescribeAffectedAccountsForOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAffectedAccountsForOrganizationResponse)
{-# DEPRECATED daaforsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaforsResponseStatus :: Lens.Lens' DescribeAffectedAccountsForOrganizationResponse Lude.Int
daaforsResponseStatus = Lens.lens (responseStatus :: DescribeAffectedAccountsForOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAffectedAccountsForOrganizationResponse)
{-# DEPRECATED daaforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
