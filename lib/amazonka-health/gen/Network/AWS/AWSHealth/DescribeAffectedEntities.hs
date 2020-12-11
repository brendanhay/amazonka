{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.
--
-- At least one event ARN is required. Results are sorted by the @lastUpdatedTime@ of the entity, starting with the most recent.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntities
  ( -- * Creating a request
    DescribeAffectedEntities (..),
    mkDescribeAffectedEntities,

    -- ** Request lenses
    daeLocale,
    daeNextToken,
    daeMaxResults,
    daeFilter,

    -- * Destructuring the response
    DescribeAffectedEntitiesResponse (..),
    mkDescribeAffectedEntitiesResponse,

    -- ** Response lenses
    daersEntities,
    daersNextToken,
    daersResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAffectedEntities' smart constructor.
data DescribeAffectedEntities = DescribeAffectedEntities'
  { locale ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    filter :: EntityFilter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAffectedEntities' with the minimum fields required to make a request.
--
-- * 'filter' - Values to narrow the results returned. At least one event ARN is required.
-- * 'locale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
-- * 'maxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
mkDescribeAffectedEntities ::
  -- | 'filter'
  EntityFilter ->
  DescribeAffectedEntities
mkDescribeAffectedEntities pFilter_ =
  DescribeAffectedEntities'
    { locale = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      filter = pFilter_
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeLocale :: Lens.Lens' DescribeAffectedEntities (Lude.Maybe Lude.Text)
daeLocale = Lens.lens (locale :: DescribeAffectedEntities -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: DescribeAffectedEntities)
{-# DEPRECATED daeLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeNextToken :: Lens.Lens' DescribeAffectedEntities (Lude.Maybe Lude.Text)
daeNextToken = Lens.lens (nextToken :: DescribeAffectedEntities -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAffectedEntities)
{-# DEPRECATED daeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeMaxResults :: Lens.Lens' DescribeAffectedEntities (Lude.Maybe Lude.Natural)
daeMaxResults = Lens.lens (maxResults :: DescribeAffectedEntities -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAffectedEntities)
{-# DEPRECATED daeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Values to narrow the results returned. At least one event ARN is required.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeFilter :: Lens.Lens' DescribeAffectedEntities EntityFilter
daeFilter = Lens.lens (filter :: DescribeAffectedEntities -> EntityFilter) (\s a -> s {filter = a} :: DescribeAffectedEntities)
{-# DEPRECATED daeFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Page.AWSPager DescribeAffectedEntities where
  page rq rs
    | Page.stop (rs Lens.^. daersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daersEntities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daeNextToken Lens..~ rs Lens.^. daersNextToken

instance Lude.AWSRequest DescribeAffectedEntities where
  type Rs DescribeAffectedEntities = DescribeAffectedEntitiesResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesResponse'
            Lude.<$> (x Lude..?> "entities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAffectedEntities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSHealth_20160804.DescribeAffectedEntities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAffectedEntities where
  toJSON DescribeAffectedEntities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("locale" Lude..=) Lude.<$> locale,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("filter" Lude..= filter)
          ]
      )

instance Lude.ToPath DescribeAffectedEntities where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAffectedEntities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAffectedEntitiesResponse' smart constructor.
data DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse'
  { entities ::
      Lude.Maybe
        [AffectedEntity],
    nextToken ::
      Lude.Maybe Lude.Text,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAffectedEntitiesResponse' with the minimum fields required to make a request.
--
-- * 'entities' - The entities that match the filter criteria.
-- * 'nextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
-- * 'responseStatus' - The response status code.
mkDescribeAffectedEntitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAffectedEntitiesResponse
mkDescribeAffectedEntitiesResponse pResponseStatus_ =
  DescribeAffectedEntitiesResponse'
    { entities = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The entities that match the filter criteria.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daersEntities :: Lens.Lens' DescribeAffectedEntitiesResponse (Lude.Maybe [AffectedEntity])
daersEntities = Lens.lens (entities :: DescribeAffectedEntitiesResponse -> Lude.Maybe [AffectedEntity]) (\s a -> s {entities = a} :: DescribeAffectedEntitiesResponse)
{-# DEPRECATED daersEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daersNextToken :: Lens.Lens' DescribeAffectedEntitiesResponse (Lude.Maybe Lude.Text)
daersNextToken = Lens.lens (nextToken :: DescribeAffectedEntitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAffectedEntitiesResponse)
{-# DEPRECATED daersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daersResponseStatus :: Lens.Lens' DescribeAffectedEntitiesResponse Lude.Int
daersResponseStatus = Lens.lens (responseStatus :: DescribeAffectedEntitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAffectedEntitiesResponse)
{-# DEPRECATED daersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
