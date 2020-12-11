{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by one or more events for one or more accounts in your organization in AWS Organizations, based on the filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service.
--
-- At least one event Amazon Resource Name (ARN) and account ID are required. Results are sorted by the @lastUpdatedTime@ of the entity, starting with the most recent.
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master account.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
  ( -- * Creating a request
    DescribeAffectedEntitiesForOrganization (..),
    mkDescribeAffectedEntitiesForOrganization,

    -- ** Request lenses
    daefoLocale,
    daefoNextToken,
    daefoMaxResults,
    daefoOrganizationEntityFilters,

    -- * Destructuring the response
    DescribeAffectedEntitiesForOrganizationResponse (..),
    mkDescribeAffectedEntitiesForOrganizationResponse,

    -- ** Response lenses
    daeforsEntities,
    daeforsFailedSet,
    daeforsNextToken,
    daeforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAffectedEntitiesForOrganization' smart constructor.
data DescribeAffectedEntitiesForOrganization = DescribeAffectedEntitiesForOrganization'
  { locale ::
      Lude.Maybe
        Lude.Text,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    organizationEntityFilters ::
      Lude.NonEmpty
        EventAccountFilter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAffectedEntitiesForOrganization' with the minimum fields required to make a request.
--
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'organizationEntityFilters' - A JSON set of elements including the @awsAccountId@ and the @eventArn@ .
mkDescribeAffectedEntitiesForOrganization ::
  -- | 'organizationEntityFilters'
  Lude.NonEmpty EventAccountFilter ->
  DescribeAffectedEntitiesForOrganization
mkDescribeAffectedEntitiesForOrganization
  pOrganizationEntityFilters_ =
    DescribeAffectedEntitiesForOrganization'
      { locale = Lude.Nothing,
        nextToken = Lude.Nothing,
        maxResults = Lude.Nothing,
        organizationEntityFilters =
          pOrganizationEntityFilters_
      }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoLocale :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Lude.Maybe Lude.Text)
daefoLocale = Lens.lens (locale :: DescribeAffectedEntitiesForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeAffectedEntitiesForOrganization)
{-# DEPRECATED daefoLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoNextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Lude.Maybe Lude.Text)
daefoNextToken = Lens.lens (nextToken :: DescribeAffectedEntitiesForOrganization -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAffectedEntitiesForOrganization)
{-# DEPRECATED daefoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoMaxResults :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Lude.Maybe Lude.Natural)
daefoMaxResults = Lens.lens (maxResults :: DescribeAffectedEntitiesForOrganization -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAffectedEntitiesForOrganization)
{-# DEPRECATED daefoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A JSON set of elements including the @awsAccountId@ and the @eventArn@ .
--
-- /Note:/ Consider using 'organizationEntityFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoOrganizationEntityFilters :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Lude.NonEmpty EventAccountFilter)
daefoOrganizationEntityFilters = Lens.lens (organizationEntityFilters :: DescribeAffectedEntitiesForOrganization -> Lude.NonEmpty EventAccountFilter) (\s a -> s {organizationEntityFilters = a} :: DescribeAffectedEntitiesForOrganization)
{-# DEPRECATED daefoOrganizationEntityFilters "Use generic-lens or generic-optics with 'organizationEntityFilters' instead." #-}

instance Page.AWSPager DescribeAffectedEntitiesForOrganization where
  page rq rs
    | Page.stop (rs Lens.^. daeforsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daeforsEntities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daefoNextToken Lens..~ rs Lens.^. daeforsNextToken

instance Lude.AWSRequest DescribeAffectedEntitiesForOrganization where
  type
    Rs DescribeAffectedEntitiesForOrganization =
      DescribeAffectedEntitiesForOrganizationResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesForOrganizationResponse'
            Lude.<$> (x Lude..?> "entities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failedSet" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAffectedEntitiesForOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSHealth_20160804.DescribeAffectedEntitiesForOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAffectedEntitiesForOrganization where
  toJSON DescribeAffectedEntitiesForOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just
              ("organizationEntityFilters" Lude..= organizationEntityFilters)
          ]
      )

instance Lude.ToPath DescribeAffectedEntitiesForOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAffectedEntitiesForOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAffectedEntitiesForOrganizationResponse' smart constructor.
data DescribeAffectedEntitiesForOrganizationResponse = DescribeAffectedEntitiesForOrganizationResponse'
  { entities ::
      Lude.Maybe
        [AffectedEntity],
    failedSet ::
      Lude.Maybe
        [OrganizationAffectedEntitiesErrorItem],
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

-- | Creates a value of 'DescribeAffectedEntitiesForOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'entities' - A JSON set of elements including the @awsAccountId@ and its @entityArn@ , @entityValue@ and its @entityArn@ , @lastUpdatedTime@ , and @statusCode@ .
-- * 'failedSet' - A JSON set of elements of the failed response, including the @awsAccountId@ , @errorMessage@ , @errorName@ , and @eventArn@ .
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'responseStatus' - The response status code.
mkDescribeAffectedEntitiesForOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAffectedEntitiesForOrganizationResponse
mkDescribeAffectedEntitiesForOrganizationResponse pResponseStatus_ =
  DescribeAffectedEntitiesForOrganizationResponse'
    { entities =
        Lude.Nothing,
      failedSet = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JSON set of elements including the @awsAccountId@ and its @entityArn@ , @entityValue@ and its @entityArn@ , @lastUpdatedTime@ , and @statusCode@ .
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforsEntities :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Lude.Maybe [AffectedEntity])
daeforsEntities = Lens.lens (entities :: DescribeAffectedEntitiesForOrganizationResponse -> Lude.Maybe [AffectedEntity]) (\s a -> s {entities = a} :: DescribeAffectedEntitiesForOrganizationResponse)
{-# DEPRECATED daeforsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | A JSON set of elements of the failed response, including the @awsAccountId@ , @errorMessage@ , @errorName@ , and @eventArn@ .
--
-- /Note:/ Consider using 'failedSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforsFailedSet :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Lude.Maybe [OrganizationAffectedEntitiesErrorItem])
daeforsFailedSet = Lens.lens (failedSet :: DescribeAffectedEntitiesForOrganizationResponse -> Lude.Maybe [OrganizationAffectedEntitiesErrorItem]) (\s a -> s {failedSet = a} :: DescribeAffectedEntitiesForOrganizationResponse)
{-# DEPRECATED daeforsFailedSet "Use generic-lens or generic-optics with 'failedSet' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforsNextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Lude.Maybe Lude.Text)
daeforsNextToken = Lens.lens (nextToken :: DescribeAffectedEntitiesForOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAffectedEntitiesForOrganizationResponse)
{-# DEPRECATED daeforsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforsResponseStatus :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse Lude.Int
daeforsResponseStatus = Lens.lens (responseStatus :: DescribeAffectedEntitiesForOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAffectedEntitiesForOrganizationResponse)
{-# DEPRECATED daeforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
