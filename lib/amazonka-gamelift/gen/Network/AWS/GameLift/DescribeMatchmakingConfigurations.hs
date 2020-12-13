{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeMatchmakingConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of FlexMatch matchmaking configurations.
--
-- This operation offers the following options: (1) retrieve all matchmaking configurations, (2) retrieve configurations for a specified list, or (3) retrieve all configurations that use a specified rule set name. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages.
-- If successful, a configuration is returned for each requested name. When specifying a list of names, only configurations that currently exist are returned.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/matchmaker-build.html Setting Up FlexMatch Matchmakers>
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
module Network.AWS.GameLift.DescribeMatchmakingConfigurations
  ( -- * Creating a request
    DescribeMatchmakingConfigurations (..),
    mkDescribeMatchmakingConfigurations,

    -- ** Request lenses
    dmcRuleSetName,
    dmcNextToken,
    dmcNames,
    dmcLimit,

    -- * Destructuring the response
    DescribeMatchmakingConfigurationsResponse (..),
    mkDescribeMatchmakingConfigurationsResponse,

    -- ** Response lenses
    drsConfigurations,
    drsNextToken,
    drsResponseStatus,
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
-- /See:/ 'mkDescribeMatchmakingConfigurations' smart constructor.
data DescribeMatchmakingConfigurations = DescribeMatchmakingConfigurations'
  { -- | A unique identifier for a matchmaking rule set. You can use either the rule set name or ARN value. Use this parameter to retrieve all matchmaking configurations that use this rule set.
    ruleSetName :: Lude.Maybe Lude.Text,
    -- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a matchmaking configuration(s) to retrieve. You can use either the configuration name or ARN value. To request all existing configurations, leave this parameter empty.
    names :: Lude.Maybe [Lude.Text],
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMatchmakingConfigurations' with the minimum fields required to make a request.
--
-- * 'ruleSetName' - A unique identifier for a matchmaking rule set. You can use either the rule set name or ARN value. Use this parameter to retrieve all matchmaking configurations that use this rule set.
-- * 'nextToken' - A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'names' - A unique identifier for a matchmaking configuration(s) to retrieve. You can use either the configuration name or ARN value. To request all existing configurations, leave this parameter empty.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
mkDescribeMatchmakingConfigurations ::
  DescribeMatchmakingConfigurations
mkDescribeMatchmakingConfigurations =
  DescribeMatchmakingConfigurations'
    { ruleSetName = Lude.Nothing,
      nextToken = Lude.Nothing,
      names = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | A unique identifier for a matchmaking rule set. You can use either the rule set name or ARN value. Use this parameter to retrieve all matchmaking configurations that use this rule set.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcRuleSetName :: Lens.Lens' DescribeMatchmakingConfigurations (Lude.Maybe Lude.Text)
dmcRuleSetName = Lens.lens (ruleSetName :: DescribeMatchmakingConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetName = a} :: DescribeMatchmakingConfigurations)
{-# DEPRECATED dmcRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcNextToken :: Lens.Lens' DescribeMatchmakingConfigurations (Lude.Maybe Lude.Text)
dmcNextToken = Lens.lens (nextToken :: DescribeMatchmakingConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMatchmakingConfigurations)
{-# DEPRECATED dmcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A unique identifier for a matchmaking configuration(s) to retrieve. You can use either the configuration name or ARN value. To request all existing configurations, leave this parameter empty.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcNames :: Lens.Lens' DescribeMatchmakingConfigurations (Lude.Maybe [Lude.Text])
dmcNames = Lens.lens (names :: DescribeMatchmakingConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeMatchmakingConfigurations)
{-# DEPRECATED dmcNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcLimit :: Lens.Lens' DescribeMatchmakingConfigurations (Lude.Maybe Lude.Natural)
dmcLimit = Lens.lens (limit :: DescribeMatchmakingConfigurations -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeMatchmakingConfigurations)
{-# DEPRECATED dmcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeMatchmakingConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmcNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeMatchmakingConfigurations where
  type
    Rs DescribeMatchmakingConfigurations =
      DescribeMatchmakingConfigurationsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMatchmakingConfigurationsResponse'
            Lude.<$> (x Lude..?> "Configurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMatchmakingConfigurations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeMatchmakingConfigurations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMatchmakingConfigurations where
  toJSON DescribeMatchmakingConfigurations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RuleSetName" Lude..=) Lude.<$> ruleSetName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Names" Lude..=) Lude.<$> names,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeMatchmakingConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMatchmakingConfigurations where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeMatchmakingConfigurationsResponse' smart constructor.
data DescribeMatchmakingConfigurationsResponse = DescribeMatchmakingConfigurationsResponse'
  { -- | A collection of requested matchmaking configurations.
    configurations :: Lude.Maybe [MatchmakingConfiguration],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMatchmakingConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'configurations' - A collection of requested matchmaking configurations.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeMatchmakingConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMatchmakingConfigurationsResponse
mkDescribeMatchmakingConfigurationsResponse pResponseStatus_ =
  DescribeMatchmakingConfigurationsResponse'
    { configurations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of requested matchmaking configurations.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsConfigurations :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Lude.Maybe [MatchmakingConfiguration])
drsConfigurations = Lens.lens (configurations :: DescribeMatchmakingConfigurationsResponse -> Lude.Maybe [MatchmakingConfiguration]) (\s a -> s {configurations = a} :: DescribeMatchmakingConfigurationsResponse)
{-# DEPRECATED drsConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeMatchmakingConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMatchmakingConfigurationsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeMatchmakingConfigurationsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeMatchmakingConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMatchmakingConfigurationsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
