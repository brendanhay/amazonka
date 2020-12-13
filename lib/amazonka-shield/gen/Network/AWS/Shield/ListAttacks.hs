{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListAttacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all ongoing DDoS attacks or all DDoS attacks during a specified time period.
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListAttacks
  ( -- * Creating a request
    ListAttacks (..),
    mkListAttacks,

    -- ** Request lenses
    laStartTime,
    laResourceARNs,
    laNextToken,
    laEndTime,
    laMaxResults,

    -- * Destructuring the response
    ListAttacksResponse (..),
    mkListAttacksResponse,

    -- ** Response lenses
    larsAttackSummaries,
    larsNextToken,
    larsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkListAttacks' smart constructor.
data ListAttacks = ListAttacks'
  { -- | The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
    startTime :: Lude.Maybe TimeRange,
    -- | The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
    resourceARNs :: Lude.Maybe [Lude.Text],
    -- | The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
    endTime :: Lude.Maybe TimeRange,
    -- | The maximum number of 'AttackSummary' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttacks' with the minimum fields required to make a request.
--
-- * 'startTime' - The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
-- * 'resourceARNs' - The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
-- * 'nextToken' - The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
-- * 'endTime' - The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
-- * 'maxResults' - The maximum number of 'AttackSummary' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
mkListAttacks ::
  ListAttacks
mkListAttacks =
  ListAttacks'
    { startTime = Lude.Nothing,
      resourceARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laStartTime :: Lens.Lens' ListAttacks (Lude.Maybe TimeRange)
laStartTime = Lens.lens (startTime :: ListAttacks -> Lude.Maybe TimeRange) (\s a -> s {startTime = a} :: ListAttacks)
{-# DEPRECATED laStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laResourceARNs :: Lens.Lens' ListAttacks (Lude.Maybe [Lude.Text])
laResourceARNs = Lens.lens (resourceARNs :: ListAttacks -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceARNs = a} :: ListAttacks)
{-# DEPRECATED laResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAttacks (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListAttacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAttacks)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laEndTime :: Lens.Lens' ListAttacks (Lude.Maybe TimeRange)
laEndTime = Lens.lens (endTime :: ListAttacks -> Lude.Maybe TimeRange) (\s a -> s {endTime = a} :: ListAttacks)
{-# DEPRECATED laEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of 'AttackSummary' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAttacks (Lude.Maybe Lude.Natural)
laMaxResults = Lens.lens (maxResults :: ListAttacks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAttacks)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListAttacks where
  page rq rs
    | Page.stop (rs Lens.^. larsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAttackSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. larsNextToken

instance Lude.AWSRequest ListAttacks where
  type Rs ListAttacks = ListAttacksResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAttacksResponse'
            Lude.<$> (x Lude..?> "AttackSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttacks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.ListAttacks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAttacks where
  toJSON ListAttacks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("ResourceArns" Lude..=) Lude.<$> resourceARNs,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListAttacks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAttacks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAttacksResponse' smart constructor.
data ListAttacksResponse = ListAttacksResponse'
  { -- | The attack information for the specified time range.
    attackSummaries :: Lude.Maybe [AttackSummary],
    -- | The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
    --
    -- Shield Advanced might return the list of 'AttackSummary' objects in batches smaller than the number specified by MaxResults. If there are more attack summary objects to return, Shield Advanced will always also return a @NextToken@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttacksResponse' with the minimum fields required to make a request.
--
-- * 'attackSummaries' - The attack information for the specified time range.
-- * 'nextToken' - The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
--
-- Shield Advanced might return the list of 'AttackSummary' objects in batches smaller than the number specified by MaxResults. If there are more attack summary objects to return, Shield Advanced will always also return a @NextToken@ .
-- * 'responseStatus' - The response status code.
mkListAttacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttacksResponse
mkListAttacksResponse pResponseStatus_ =
  ListAttacksResponse'
    { attackSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attack information for the specified time range.
--
-- /Note:/ Consider using 'attackSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAttackSummaries :: Lens.Lens' ListAttacksResponse (Lude.Maybe [AttackSummary])
larsAttackSummaries = Lens.lens (attackSummaries :: ListAttacksResponse -> Lude.Maybe [AttackSummary]) (\s a -> s {attackSummaries = a} :: ListAttacksResponse)
{-# DEPRECATED larsAttackSummaries "Use generic-lens or generic-optics with 'attackSummaries' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
--
-- Shield Advanced might return the list of 'AttackSummary' objects in batches smaller than the number specified by MaxResults. If there are more attack summary objects to return, Shield Advanced will always also return a @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextToken :: Lens.Lens' ListAttacksResponse (Lude.Maybe Lude.Text)
larsNextToken = Lens.lens (nextToken :: ListAttacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAttacksResponse)
{-# DEPRECATED larsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAttacksResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAttacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttacksResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
