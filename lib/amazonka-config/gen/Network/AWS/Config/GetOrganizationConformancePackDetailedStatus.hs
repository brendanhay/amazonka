{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed status for each member account within an organization for a given organization conformance pack.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
  ( -- * Creating a request
    GetOrganizationConformancePackDetailedStatus (..),
    mkGetOrganizationConformancePackDetailedStatus,

    -- ** Request lenses
    gocpdsOrganizationConformancePackName,
    gocpdsFilters,
    gocpdsNextToken,
    gocpdsLimit,

    -- * Destructuring the response
    GetOrganizationConformancePackDetailedStatusResponse (..),
    mkGetOrganizationConformancePackDetailedStatusResponse,

    -- ** Response lenses
    gocpdsrsOrganizationConformancePackDetailedStatuses,
    gocpdsrsNextToken,
    gocpdsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOrganizationConformancePackDetailedStatus' smart constructor.
data GetOrganizationConformancePackDetailedStatus = GetOrganizationConformancePackDetailedStatus'
  { -- | The name of organization conformance pack for which you want status details for member accounts.
    organizationConformancePackName :: Lude.Text,
    -- | An @OrganizationResourceDetailedStatusFilters@ object.
    filters :: Lude.Maybe OrganizationResourceDetailedStatusFilters,
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOrganizationConformancePackDetailedStatus' with the minimum fields required to make a request.
--
-- * 'organizationConformancePackName' - The name of organization conformance pack for which you want status details for member accounts.
-- * 'filters' - An @OrganizationResourceDetailedStatusFilters@ object.
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
mkGetOrganizationConformancePackDetailedStatus ::
  -- | 'organizationConformancePackName'
  Lude.Text ->
  GetOrganizationConformancePackDetailedStatus
mkGetOrganizationConformancePackDetailedStatus
  pOrganizationConformancePackName_ =
    GetOrganizationConformancePackDetailedStatus'
      { organizationConformancePackName =
          pOrganizationConformancePackName_,
        filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        limit = Lude.Nothing
      }

-- | The name of organization conformance pack for which you want status details for member accounts.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsOrganizationConformancePackName :: Lens.Lens' GetOrganizationConformancePackDetailedStatus Lude.Text
gocpdsOrganizationConformancePackName = Lens.lens (organizationConformancePackName :: GetOrganizationConformancePackDetailedStatus -> Lude.Text) (\s a -> s {organizationConformancePackName = a} :: GetOrganizationConformancePackDetailedStatus)
{-# DEPRECATED gocpdsOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

-- | An @OrganizationResourceDetailedStatusFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsFilters :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Lude.Maybe OrganizationResourceDetailedStatusFilters)
gocpdsFilters = Lens.lens (filters :: GetOrganizationConformancePackDetailedStatus -> Lude.Maybe OrganizationResourceDetailedStatusFilters) (\s a -> s {filters = a} :: GetOrganizationConformancePackDetailedStatus)
{-# DEPRECATED gocpdsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsNextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Lude.Maybe Lude.Text)
gocpdsNextToken = Lens.lens (nextToken :: GetOrganizationConformancePackDetailedStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOrganizationConformancePackDetailedStatus)
{-# DEPRECATED gocpdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of @OrganizationConformancePackDetailedStatuses@ returned on each page. If you do not specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsLimit :: Lens.Lens' GetOrganizationConformancePackDetailedStatus (Lude.Maybe Lude.Natural)
gocpdsLimit = Lens.lens (limit :: GetOrganizationConformancePackDetailedStatus -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetOrganizationConformancePackDetailedStatus)
{-# DEPRECATED gocpdsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance
  Lude.AWSRequest
    GetOrganizationConformancePackDetailedStatus
  where
  type
    Rs GetOrganizationConformancePackDetailedStatus =
      GetOrganizationConformancePackDetailedStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOrganizationConformancePackDetailedStatusResponse'
            Lude.<$> ( x Lude..?> "OrganizationConformancePackDetailedStatuses"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    GetOrganizationConformancePackDetailedStatus
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.GetOrganizationConformancePackDetailedStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOrganizationConformancePackDetailedStatus where
  toJSON GetOrganizationConformancePackDetailedStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "OrganizationConformancePackName"
                  Lude..= organizationConformancePackName
              ),
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath GetOrganizationConformancePackDetailedStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOrganizationConformancePackDetailedStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOrganizationConformancePackDetailedStatusResponse' smart constructor.
data GetOrganizationConformancePackDetailedStatusResponse = GetOrganizationConformancePackDetailedStatusResponse'
  { -- | A list of @OrganizationConformancePackDetailedStatus@ objects.
    organizationConformancePackDetailedStatuses :: Lude.Maybe [OrganizationConformancePackDetailedStatus],
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOrganizationConformancePackDetailedStatusResponse' with the minimum fields required to make a request.
--
-- * 'organizationConformancePackDetailedStatuses' - A list of @OrganizationConformancePackDetailedStatus@ objects.
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkGetOrganizationConformancePackDetailedStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOrganizationConformancePackDetailedStatusResponse
mkGetOrganizationConformancePackDetailedStatusResponse
  pResponseStatus_ =
    GetOrganizationConformancePackDetailedStatusResponse'
      { organizationConformancePackDetailedStatuses =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | A list of @OrganizationConformancePackDetailedStatus@ objects.
--
-- /Note:/ Consider using 'organizationConformancePackDetailedStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsrsOrganizationConformancePackDetailedStatuses :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Lude.Maybe [OrganizationConformancePackDetailedStatus])
gocpdsrsOrganizationConformancePackDetailedStatuses = Lens.lens (organizationConformancePackDetailedStatuses :: GetOrganizationConformancePackDetailedStatusResponse -> Lude.Maybe [OrganizationConformancePackDetailedStatus]) (\s a -> s {organizationConformancePackDetailedStatuses = a} :: GetOrganizationConformancePackDetailedStatusResponse)
{-# DEPRECATED gocpdsrsOrganizationConformancePackDetailedStatuses "Use generic-lens or generic-optics with 'organizationConformancePackDetailedStatuses' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsrsNextToken :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse (Lude.Maybe Lude.Text)
gocpdsrsNextToken = Lens.lens (nextToken :: GetOrganizationConformancePackDetailedStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetOrganizationConformancePackDetailedStatusResponse)
{-# DEPRECATED gocpdsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gocpdsrsResponseStatus :: Lens.Lens' GetOrganizationConformancePackDetailedStatusResponse Lude.Int
gocpdsrsResponseStatus = Lens.lens (responseStatus :: GetOrganizationConformancePackDetailedStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOrganizationConformancePackDetailedStatusResponse)
{-# DEPRECATED gocpdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
