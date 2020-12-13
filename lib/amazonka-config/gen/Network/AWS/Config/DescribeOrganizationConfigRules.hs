{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization config rules.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added. 
module Network.AWS.Config.DescribeOrganizationConfigRules
  ( -- * Creating a request
    DescribeOrganizationConfigRules (..),
    mkDescribeOrganizationConfigRules,

    -- ** Request lenses
    docrOrganizationConfigRuleNames,
    docrNextToken,
    docrLimit,

    -- * Destructuring the response
    DescribeOrganizationConfigRulesResponse (..),
    mkDescribeOrganizationConfigRulesResponse,

    -- ** Response lenses
    docrrsOrganizationConfigRules,
    docrrsNextToken,
    docrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganizationConfigRules' smart constructor.
data DescribeOrganizationConfigRules = DescribeOrganizationConfigRules'
  { -- | The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
    organizationConfigRuleNames :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConfigRules' with the minimum fields required to make a request.
--
-- * 'organizationConfigRuleNames' - The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
mkDescribeOrganizationConfigRules ::
  DescribeOrganizationConfigRules
mkDescribeOrganizationConfigRules =
  DescribeOrganizationConfigRules'
    { organizationConfigRuleNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
--
-- /Note:/ Consider using 'organizationConfigRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrOrganizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRules (Lude.Maybe [Lude.Text])
docrOrganizationConfigRuleNames = Lens.lens (organizationConfigRuleNames :: DescribeOrganizationConfigRules -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationConfigRuleNames = a} :: DescribeOrganizationConfigRules)
{-# DEPRECATED docrOrganizationConfigRuleNames "Use generic-lens or generic-optics with 'organizationConfigRuleNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrNextToken :: Lens.Lens' DescribeOrganizationConfigRules (Lude.Maybe Lude.Text)
docrNextToken = Lens.lens (nextToken :: DescribeOrganizationConfigRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConfigRules)
{-# DEPRECATED docrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrLimit :: Lens.Lens' DescribeOrganizationConfigRules (Lude.Maybe Lude.Natural)
docrLimit = Lens.lens (limit :: DescribeOrganizationConfigRules -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeOrganizationConfigRules)
{-# DEPRECATED docrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeOrganizationConfigRules where
  type
    Rs DescribeOrganizationConfigRules =
      DescribeOrganizationConfigRulesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRulesResponse'
            Lude.<$> (x Lude..?> "OrganizationConfigRules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganizationConfigRules where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeOrganizationConfigRules" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganizationConfigRules where
  toJSON DescribeOrganizationConfigRules' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationConfigRuleNames" Lude..=)
              Lude.<$> organizationConfigRuleNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeOrganizationConfigRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganizationConfigRules where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationConfigRulesResponse' smart constructor.
data DescribeOrganizationConfigRulesResponse = DescribeOrganizationConfigRulesResponse'
  { -- | Returns a list of @OrganizationConfigRule@ objects.
    organizationConfigRules :: Lude.Maybe [OrganizationConfigRule],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConfigRulesResponse' with the minimum fields required to make a request.
--
-- * 'organizationConfigRules' - Returns a list of @OrganizationConfigRule@ objects.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationConfigRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationConfigRulesResponse
mkDescribeOrganizationConfigRulesResponse pResponseStatus_ =
  DescribeOrganizationConfigRulesResponse'
    { organizationConfigRules =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of @OrganizationConfigRule@ objects.
--
-- /Note:/ Consider using 'organizationConfigRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsOrganizationConfigRules :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Lude.Maybe [OrganizationConfigRule])
docrrsOrganizationConfigRules = Lens.lens (organizationConfigRules :: DescribeOrganizationConfigRulesResponse -> Lude.Maybe [OrganizationConfigRule]) (\s a -> s {organizationConfigRules = a} :: DescribeOrganizationConfigRulesResponse)
{-# DEPRECATED docrrsOrganizationConfigRules "Use generic-lens or generic-optics with 'organizationConfigRules' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsNextToken :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Lude.Maybe Lude.Text)
docrrsNextToken = Lens.lens (nextToken :: DescribeOrganizationConfigRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConfigRulesResponse)
{-# DEPRECATED docrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsResponseStatus :: Lens.Lens' DescribeOrganizationConfigRulesResponse Lude.Int
docrrsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationConfigRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationConfigRulesResponse)
{-# DEPRECATED docrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
