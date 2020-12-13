{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your AWS Config rules.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRules
  ( -- * Creating a request
    DescribeConfigRules (..),
    mkDescribeConfigRules,

    -- ** Request lenses
    dcrConfigRuleNames,
    dcrNextToken,

    -- * Destructuring the response
    DescribeConfigRulesResponse (..),
    mkDescribeConfigRulesResponse,

    -- ** Response lenses
    dcrrsConfigRules,
    dcrrsNextToken,
    dcrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeConfigRules' smart constructor.
data DescribeConfigRules = DescribeConfigRules'
  { -- | The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
    configRuleNames :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigRules' with the minimum fields required to make a request.
--
-- * 'configRuleNames' - The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
mkDescribeConfigRules ::
  DescribeConfigRules
mkDescribeConfigRules =
  DescribeConfigRules'
    { configRuleNames = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigRuleNames :: Lens.Lens' DescribeConfigRules (Lude.Maybe [Lude.Text])
dcrConfigRuleNames = Lens.lens (configRuleNames :: DescribeConfigRules -> Lude.Maybe [Lude.Text]) (\s a -> s {configRuleNames = a} :: DescribeConfigRules)
{-# DEPRECATED dcrConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrNextToken :: Lens.Lens' DescribeConfigRules (Lude.Maybe Lude.Text)
dcrNextToken = Lens.lens (nextToken :: DescribeConfigRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigRules)
{-# DEPRECATED dcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager DescribeConfigRules where
  page rq rs
    | Page.stop (rs Lens.^. dcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcrrsConfigRules) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcrNextToken Lens..~ rs Lens.^. dcrrsNextToken

instance Lude.AWSRequest DescribeConfigRules where
  type Rs DescribeConfigRules = DescribeConfigRulesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConfigRulesResponse'
            Lude.<$> (x Lude..?> "ConfigRules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConfigRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DescribeConfigRules" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConfigRules where
  toJSON DescribeConfigRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigRuleNames" Lude..=) Lude.<$> configRuleNames,
            ("NextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath DescribeConfigRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConfigRules where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeConfigRulesResponse' smart constructor.
data DescribeConfigRulesResponse = DescribeConfigRulesResponse'
  { -- | The details about your AWS Config rules.
    configRules :: Lude.Maybe [ConfigRule],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConfigRulesResponse' with the minimum fields required to make a request.
--
-- * 'configRules' - The details about your AWS Config rules.
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeConfigRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConfigRulesResponse
mkDescribeConfigRulesResponse pResponseStatus_ =
  DescribeConfigRulesResponse'
    { configRules = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details about your AWS Config rules.
--
-- /Note:/ Consider using 'configRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsConfigRules :: Lens.Lens' DescribeConfigRulesResponse (Lude.Maybe [ConfigRule])
dcrrsConfigRules = Lens.lens (configRules :: DescribeConfigRulesResponse -> Lude.Maybe [ConfigRule]) (\s a -> s {configRules = a} :: DescribeConfigRulesResponse)
{-# DEPRECATED dcrrsConfigRules "Use generic-lens or generic-optics with 'configRules' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsNextToken :: Lens.Lens' DescribeConfigRulesResponse (Lude.Maybe Lude.Text)
dcrrsNextToken = Lens.lens (nextToken :: DescribeConfigRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConfigRulesResponse)
{-# DEPRECATED dcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeConfigRulesResponse Lude.Int
dcrrsResponseStatus = Lens.lens (responseStatus :: DescribeConfigRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConfigRulesResponse)
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
