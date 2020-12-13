{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeMatchmakingRuleSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details for FlexMatch matchmaking rule sets. You can request all existing rule sets for the Region, or provide a list of one or more rule set names. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a rule set is returned for each requested name.
--
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
--
-- __Related operations__
--
--     * 'CreateMatchmakingConfiguration'
--
--
--     * 'DescribeMatchmakingConfigurations'
--
--
--     * 'UpdateMatchmakingConfiguration'
--
--
--     * 'DeleteMatchmakingConfiguration'
--
--
--     * 'CreateMatchmakingRuleSet'
--
--
--     * 'DescribeMatchmakingRuleSets'
--
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--     * 'DeleteMatchmakingRuleSet'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeMatchmakingRuleSets
  ( -- * Creating a request
    DescribeMatchmakingRuleSets (..),
    mkDescribeMatchmakingRuleSets,

    -- ** Request lenses
    dmrsNextToken,
    dmrsNames,
    dmrsLimit,

    -- * Destructuring the response
    DescribeMatchmakingRuleSetsResponse (..),
    mkDescribeMatchmakingRuleSetsResponse,

    -- ** Response lenses
    dmrsrsRuleSets,
    dmrsrsNextToken,
    dmrsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeMatchmakingRuleSets' smart constructor.
data DescribeMatchmakingRuleSets = DescribeMatchmakingRuleSets'
  { -- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of one or more matchmaking rule set names to retrieve details for. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
    names :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMatchmakingRuleSets' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'names' - A list of one or more matchmaking rule set names to retrieve details for. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
mkDescribeMatchmakingRuleSets ::
  DescribeMatchmakingRuleSets
mkDescribeMatchmakingRuleSets =
  DescribeMatchmakingRuleSets'
    { nextToken = Lude.Nothing,
      names = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsNextToken :: Lens.Lens' DescribeMatchmakingRuleSets (Lude.Maybe Lude.Text)
dmrsNextToken = Lens.lens (nextToken :: DescribeMatchmakingRuleSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMatchmakingRuleSets)
{-# DEPRECATED dmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of one or more matchmaking rule set names to retrieve details for. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsNames :: Lens.Lens' DescribeMatchmakingRuleSets (Lude.Maybe (Lude.NonEmpty Lude.Text))
dmrsNames = Lens.lens (names :: DescribeMatchmakingRuleSets -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {names = a} :: DescribeMatchmakingRuleSets)
{-# DEPRECATED dmrsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsLimit :: Lens.Lens' DescribeMatchmakingRuleSets (Lude.Maybe Lude.Natural)
dmrsLimit = Lens.lens (limit :: DescribeMatchmakingRuleSets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeMatchmakingRuleSets)
{-# DEPRECATED dmrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeMatchmakingRuleSets where
  page rq rs
    | Page.stop (rs Lens.^. dmrsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmrsrsRuleSets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmrsNextToken Lens..~ rs Lens.^. dmrsrsNextToken

instance Lude.AWSRequest DescribeMatchmakingRuleSets where
  type
    Rs DescribeMatchmakingRuleSets =
      DescribeMatchmakingRuleSetsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMatchmakingRuleSetsResponse'
            Lude.<$> (x Lude..?> "RuleSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMatchmakingRuleSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeMatchmakingRuleSets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMatchmakingRuleSets where
  toJSON DescribeMatchmakingRuleSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeMatchmakingRuleSets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMatchmakingRuleSets where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeMatchmakingRuleSetsResponse' smart constructor.
data DescribeMatchmakingRuleSetsResponse = DescribeMatchmakingRuleSetsResponse'
  { -- | A collection of requested matchmaking rule set objects.
    ruleSets :: [MatchmakingRuleSet],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMatchmakingRuleSetsResponse' with the minimum fields required to make a request.
--
-- * 'ruleSets' - A collection of requested matchmaking rule set objects.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeMatchmakingRuleSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMatchmakingRuleSetsResponse
mkDescribeMatchmakingRuleSetsResponse pResponseStatus_ =
  DescribeMatchmakingRuleSetsResponse'
    { ruleSets = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of requested matchmaking rule set objects.
--
-- /Note:/ Consider using 'ruleSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrsRuleSets :: Lens.Lens' DescribeMatchmakingRuleSetsResponse [MatchmakingRuleSet]
dmrsrsRuleSets = Lens.lens (ruleSets :: DescribeMatchmakingRuleSetsResponse -> [MatchmakingRuleSet]) (\s a -> s {ruleSets = a} :: DescribeMatchmakingRuleSetsResponse)
{-# DEPRECATED dmrsrsRuleSets "Use generic-lens or generic-optics with 'ruleSets' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrsNextToken :: Lens.Lens' DescribeMatchmakingRuleSetsResponse (Lude.Maybe Lude.Text)
dmrsrsNextToken = Lens.lens (nextToken :: DescribeMatchmakingRuleSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMatchmakingRuleSetsResponse)
{-# DEPRECATED dmrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrsResponseStatus :: Lens.Lens' DescribeMatchmakingRuleSetsResponse Lude.Int
dmrsrsResponseStatus = Lens.lens (responseStatus :: DescribeMatchmakingRuleSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMatchmakingRuleSetsResponse)
{-# DEPRECATED dmrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
