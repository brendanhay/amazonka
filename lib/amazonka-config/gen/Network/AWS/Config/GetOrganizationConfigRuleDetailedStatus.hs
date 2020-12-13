{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization for a given organization config rule.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
  ( -- * Creating a request
    GetOrganizationConfigRuleDetailedStatus (..),
    mkGetOrganizationConfigRuleDetailedStatus,

    -- ** Request lenses
    gocrdsFilters,
    gocrdsNextToken,
    gocrdsLimit,
    gocrdsOrganizationConfigRuleName,

    -- * Destructuring the response
    GetOrganizationConfigRuleDetailedStatusResponse (..),
    mkGetOrganizationConfigRuleDetailedStatusResponse,

    -- ** Response lenses
    gocrdsrsOrganizationConfigRuleDetailedStatus,
    gocrdsrsNextToken,
    gocrdsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOrganizationConfigRuleDetailedStatus' smart constructor.
data GetOrganizationConfigRuleDetailedStatus = GetOrganizationConfigRuleDetailedStatus'
  { -- | A @StatusDetailFilters@ object.
    filters :: Lude.Maybe StatusDetailFilters,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
    limit :: Lude.Maybe Lude.Natural,
    -- | The name of organization config rule for which you want status details for member accounts.
    organizationConfigRuleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOrganizationConfigRuleDetailedStatus' with the minimum fields required to make a request.
--
-- * 'filters' - A @StatusDetailFilters@ object.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
-- * 'organizationConfigRuleName' - The name of organization config rule for which you want status details for member accounts.
mkGetOrganizationConfigRuleDetailedStatus ::
  -- | 'organizationConfigRuleName'
  Lude.Text ->
  GetOrganizationConfigRuleDetailedStatus
mkGetOrganizationConfigRuleDetailedStatus
  pOrganizationConfigRuleName_ =
    GetOrganizationConfigRuleDetailedStatus'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        limit = Lude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | A @StatusDetailFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsFilters :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Lude.Maybe StatusDetailFilters)
gocrdsFilters = Lens.lens (filters :: GetOrganizationConfigRuleDetailedStatus -> Lude.Maybe StatusDetailFilters) (\s a -> s {filters = a} :: GetOrganizationConfigRuleDetailedStatus)
{-# DEPRECATED gocrdsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsNextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Lude.Maybe Lude.Text)
gocrdsNextToken = Lens.lens (nextToken :: GetOrganizationConfigRuleDetailedStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOrganizationConfigRuleDetailedStatus)
{-# DEPRECATED gocrdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of @OrganizationConfigRuleDetailedStatus@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsLimit :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus (Lude.Maybe Lude.Natural)
gocrdsLimit = Lens.lens (limit :: GetOrganizationConfigRuleDetailedStatus -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetOrganizationConfigRuleDetailedStatus)
{-# DEPRECATED gocrdsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of organization config rule for which you want status details for member accounts.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsOrganizationConfigRuleName :: Lens.Lens' GetOrganizationConfigRuleDetailedStatus Lude.Text
gocrdsOrganizationConfigRuleName = Lens.lens (organizationConfigRuleName :: GetOrganizationConfigRuleDetailedStatus -> Lude.Text) (\s a -> s {organizationConfigRuleName = a} :: GetOrganizationConfigRuleDetailedStatus)
{-# DEPRECATED gocrdsOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

instance Lude.AWSRequest GetOrganizationConfigRuleDetailedStatus where
  type
    Rs GetOrganizationConfigRuleDetailedStatus =
      GetOrganizationConfigRuleDetailedStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOrganizationConfigRuleDetailedStatusResponse'
            Lude.<$> ( x Lude..?> "OrganizationConfigRuleDetailedStatus"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOrganizationConfigRuleDetailedStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetOrganizationConfigRuleDetailedStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOrganizationConfigRuleDetailedStatus where
  toJSON GetOrganizationConfigRuleDetailedStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just
              ("OrganizationConfigRuleName" Lude..= organizationConfigRuleName)
          ]
      )

instance Lude.ToPath GetOrganizationConfigRuleDetailedStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOrganizationConfigRuleDetailedStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOrganizationConfigRuleDetailedStatusResponse' smart constructor.
data GetOrganizationConfigRuleDetailedStatusResponse = GetOrganizationConfigRuleDetailedStatusResponse'
  { -- | A list of @MemberAccountStatus@ objects.
    organizationConfigRuleDetailedStatus :: Lude.Maybe [MemberAccountStatus],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOrganizationConfigRuleDetailedStatusResponse' with the minimum fields required to make a request.
--
-- * 'organizationConfigRuleDetailedStatus' - A list of @MemberAccountStatus@ objects.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetOrganizationConfigRuleDetailedStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOrganizationConfigRuleDetailedStatusResponse
mkGetOrganizationConfigRuleDetailedStatusResponse pResponseStatus_ =
  GetOrganizationConfigRuleDetailedStatusResponse'
    { organizationConfigRuleDetailedStatus =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @MemberAccountStatus@ objects.
--
-- /Note:/ Consider using 'organizationConfigRuleDetailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsrsOrganizationConfigRuleDetailedStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Lude.Maybe [MemberAccountStatus])
gocrdsrsOrganizationConfigRuleDetailedStatus = Lens.lens (organizationConfigRuleDetailedStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> Lude.Maybe [MemberAccountStatus]) (\s a -> s {organizationConfigRuleDetailedStatus = a} :: GetOrganizationConfigRuleDetailedStatusResponse)
{-# DEPRECATED gocrdsrsOrganizationConfigRuleDetailedStatus "Use generic-lens or generic-optics with 'organizationConfigRuleDetailedStatus' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsrsNextToken :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse (Lude.Maybe Lude.Text)
gocrdsrsNextToken = Lens.lens (nextToken :: GetOrganizationConfigRuleDetailedStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOrganizationConfigRuleDetailedStatusResponse)
{-# DEPRECATED gocrdsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocrdsrsResponseStatus :: Lens.Lens' GetOrganizationConfigRuleDetailedStatusResponse Lude.Int
gocrdsrsResponseStatus = Lens.lens (responseStatus :: GetOrganizationConfigRuleDetailedStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOrganizationConfigRuleDetailedStatusResponse)
{-# DEPRECATED gocrdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
