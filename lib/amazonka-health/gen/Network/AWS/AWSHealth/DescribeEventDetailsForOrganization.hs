{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeEventDetailsForOrganization (..),
    mkDescribeEventDetailsForOrganization,

    -- ** Request lenses
    dedfoLocale,
    dedfoOrganizationEventDetailFilters,

    -- * Destructuring the response
    DescribeEventDetailsForOrganizationResponse (..),
    mkDescribeEventDetailsForOrganizationResponse,

    -- ** Response lenses
    dedforsSuccessfulSet,
    dedforsFailedSet,
    dedforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventDetailsForOrganization' smart constructor.
data DescribeEventDetailsForOrganization = DescribeEventDetailsForOrganization'
  { -- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
    locale :: Lude.Maybe Lude.Text,
    -- | A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
    organizationEventDetailFilters :: Lude.NonEmpty EventAccountFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventDetailsForOrganization' with the minimum fields required to make a request.
--
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
-- * 'organizationEventDetailFilters' - A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
mkDescribeEventDetailsForOrganization ::
  -- | 'organizationEventDetailFilters'
  Lude.NonEmpty EventAccountFilter ->
  DescribeEventDetailsForOrganization
mkDescribeEventDetailsForOrganization
  pOrganizationEventDetailFilters_ =
    DescribeEventDetailsForOrganization'
      { locale = Lude.Nothing,
        organizationEventDetailFilters =
          pOrganizationEventDetailFilters_
      }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedfoLocale :: Lens.Lens' DescribeEventDetailsForOrganization (Lude.Maybe Lude.Text)
dedfoLocale = Lens.lens (locale :: DescribeEventDetailsForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeEventDetailsForOrganization)
{-# DEPRECATED dedfoLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
--
-- /Note:/ Consider using 'organizationEventDetailFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedfoOrganizationEventDetailFilters :: Lens.Lens' DescribeEventDetailsForOrganization (Lude.NonEmpty EventAccountFilter)
dedfoOrganizationEventDetailFilters = Lens.lens (organizationEventDetailFilters :: DescribeEventDetailsForOrganization -> Lude.NonEmpty EventAccountFilter) (\s a -> s {organizationEventDetailFilters = a} :: DescribeEventDetailsForOrganization)
{-# DEPRECATED dedfoOrganizationEventDetailFilters "Use generic-lens or generic-optics with 'organizationEventDetailFilters' instead." #-}

instance Lude.AWSRequest DescribeEventDetailsForOrganization where
  type
    Rs DescribeEventDetailsForOrganization =
      DescribeEventDetailsForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventDetailsForOrganizationResponse'
            Lude.<$> (x Lude..?> "successfulSet" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedSet" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventDetailsForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.DescribeEventDetailsForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventDetailsForOrganization where
  toJSON DescribeEventDetailsForOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            Lude.Just
              ( "organizationEventDetailFilters"
                  Lude..= organizationEventDetailFilters
              )
          ]
      )

instance Lude.ToPath DescribeEventDetailsForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventDetailsForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventDetailsForOrganizationResponse' smart constructor.
data DescribeEventDetailsForOrganizationResponse = DescribeEventDetailsForOrganizationResponse'
  { -- | Information about the events that could be retrieved.
    successfulSet :: Lude.Maybe [OrganizationEventDetails],
    -- | Error messages for any events that could not be retrieved.
    failedSet :: Lude.Maybe [OrganizationEventDetailsErrorItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventDetailsForOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'successfulSet' - Information about the events that could be retrieved.
-- * 'failedSet' - Error messages for any events that could not be retrieved.
-- * 'responseStatus' - The response status code.
mkDescribeEventDetailsForOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventDetailsForOrganizationResponse
mkDescribeEventDetailsForOrganizationResponse pResponseStatus_ =
  DescribeEventDetailsForOrganizationResponse'
    { successfulSet =
        Lude.Nothing,
      failedSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the events that could be retrieved.
--
-- /Note:/ Consider using 'successfulSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedforsSuccessfulSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Lude.Maybe [OrganizationEventDetails])
dedforsSuccessfulSet = Lens.lens (successfulSet :: DescribeEventDetailsForOrganizationResponse -> Lude.Maybe [OrganizationEventDetails]) (\s a -> s {successfulSet = a} :: DescribeEventDetailsForOrganizationResponse)
{-# DEPRECATED dedforsSuccessfulSet "Use generic-lens or generic-optics with 'successfulSet' instead." #-}

-- | Error messages for any events that could not be retrieved.
--
-- /Note:/ Consider using 'failedSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedforsFailedSet :: Lens.Lens' DescribeEventDetailsForOrganizationResponse (Lude.Maybe [OrganizationEventDetailsErrorItem])
dedforsFailedSet = Lens.lens (failedSet :: DescribeEventDetailsForOrganizationResponse -> Lude.Maybe [OrganizationEventDetailsErrorItem]) (\s a -> s {failedSet = a} :: DescribeEventDetailsForOrganizationResponse)
{-# DEPRECATED dedforsFailedSet "Use generic-lens or generic-optics with 'failedSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedforsResponseStatus :: Lens.Lens' DescribeEventDetailsForOrganizationResponse Lude.Int
dedforsResponseStatus = Lens.lens (responseStatus :: DescribeEventDetailsForOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventDetailsForOrganizationResponse)
{-# DEPRECATED dedforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
