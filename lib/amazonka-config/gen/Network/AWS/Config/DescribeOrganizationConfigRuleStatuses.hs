{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization config rule deployment status for an organization.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
  ( -- * Creating a request
    DescribeOrganizationConfigRuleStatuses (..),
    mkDescribeOrganizationConfigRuleStatuses,

    -- ** Request lenses
    docrsOrganizationConfigRuleNames,
    docrsNextToken,
    docrsLimit,

    -- * Destructuring the response
    DescribeOrganizationConfigRuleStatusesResponse (..),
    mkDescribeOrganizationConfigRuleStatusesResponse,

    -- ** Response lenses
    docrsrsNextToken,
    docrsrsOrganizationConfigRuleStatuses,
    docrsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganizationConfigRuleStatuses' smart constructor.
data DescribeOrganizationConfigRuleStatuses = DescribeOrganizationConfigRuleStatuses'
  { -- | The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
    organizationConfigRuleNames :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConfigRuleStatuses' with the minimum fields required to make a request.
--
-- * 'organizationConfigRuleNames' - The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
mkDescribeOrganizationConfigRuleStatuses ::
  DescribeOrganizationConfigRuleStatuses
mkDescribeOrganizationConfigRuleStatuses =
  DescribeOrganizationConfigRuleStatuses'
    { organizationConfigRuleNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The names of organization config rules for which you want status details. If you do not specify any names, AWS Config returns details for all your organization AWS Confg rules.
--
-- /Note:/ Consider using 'organizationConfigRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsOrganizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Lude.Maybe [Lude.Text])
docrsOrganizationConfigRuleNames = Lens.lens (organizationConfigRuleNames :: DescribeOrganizationConfigRuleStatuses -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationConfigRuleNames = a} :: DescribeOrganizationConfigRuleStatuses)
{-# DEPRECATED docrsOrganizationConfigRuleNames "Use generic-lens or generic-optics with 'organizationConfigRuleNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsNextToken :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Lude.Maybe Lude.Text)
docrsNextToken = Lens.lens (nextToken :: DescribeOrganizationConfigRuleStatuses -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConfigRuleStatuses)
{-# DEPRECATED docrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of @OrganizationConfigRuleStatuses@ returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsLimit :: Lens.Lens' DescribeOrganizationConfigRuleStatuses (Lude.Maybe Lude.Natural)
docrsLimit = Lens.lens (limit :: DescribeOrganizationConfigRuleStatuses -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeOrganizationConfigRuleStatuses)
{-# DEPRECATED docrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeOrganizationConfigRuleStatuses where
  type
    Rs DescribeOrganizationConfigRuleStatuses =
      DescribeOrganizationConfigRuleStatusesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRuleStatusesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "OrganizationConfigRuleStatuses" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganizationConfigRuleStatuses where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeOrganizationConfigRuleStatuses" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganizationConfigRuleStatuses where
  toJSON DescribeOrganizationConfigRuleStatuses' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationConfigRuleNames" Lude..=)
              Lude.<$> organizationConfigRuleNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeOrganizationConfigRuleStatuses where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganizationConfigRuleStatuses where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationConfigRuleStatusesResponse' smart constructor.
data DescribeOrganizationConfigRuleStatusesResponse = DescribeOrganizationConfigRuleStatusesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @OrganizationConfigRuleStatus@ objects.
    organizationConfigRuleStatuses :: Lude.Maybe [OrganizationConfigRuleStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConfigRuleStatusesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'organizationConfigRuleStatuses' - A list of @OrganizationConfigRuleStatus@ objects.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationConfigRuleStatusesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationConfigRuleStatusesResponse
mkDescribeOrganizationConfigRuleStatusesResponse pResponseStatus_ =
  DescribeOrganizationConfigRuleStatusesResponse'
    { nextToken =
        Lude.Nothing,
      organizationConfigRuleStatuses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsrsNextToken :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse (Lude.Maybe Lude.Text)
docrsrsNextToken = Lens.lens (nextToken :: DescribeOrganizationConfigRuleStatusesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConfigRuleStatusesResponse)
{-# DEPRECATED docrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @OrganizationConfigRuleStatus@ objects.
--
-- /Note:/ Consider using 'organizationConfigRuleStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsrsOrganizationConfigRuleStatuses :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse (Lude.Maybe [OrganizationConfigRuleStatus])
docrsrsOrganizationConfigRuleStatuses = Lens.lens (organizationConfigRuleStatuses :: DescribeOrganizationConfigRuleStatusesResponse -> Lude.Maybe [OrganizationConfigRuleStatus]) (\s a -> s {organizationConfigRuleStatuses = a} :: DescribeOrganizationConfigRuleStatusesResponse)
{-# DEPRECATED docrsrsOrganizationConfigRuleStatuses "Use generic-lens or generic-optics with 'organizationConfigRuleStatuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrsrsResponseStatus :: Lens.Lens' DescribeOrganizationConfigRuleStatusesResponse Lude.Int
docrsrsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationConfigRuleStatusesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationConfigRuleStatusesResponse)
{-# DEPRECATED docrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
