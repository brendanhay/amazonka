{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePackStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides organization conformance pack deployment status for an organization.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConformancePackStatuses
  ( -- * Creating a request
    DescribeOrganizationConformancePackStatuses (..),
    mkDescribeOrganizationConformancePackStatuses,

    -- ** Request lenses
    docpsNextToken,
    docpsLimit,
    docpsOrganizationConformancePackNames,

    -- * Destructuring the response
    DescribeOrganizationConformancePackStatusesResponse (..),
    mkDescribeOrganizationConformancePackStatusesResponse,

    -- ** Response lenses
    docpsrsOrganizationConformancePackStatuses,
    docpsrsNextToken,
    docpsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganizationConformancePackStatuses' smart constructor.
data DescribeOrganizationConformancePackStatuses = DescribeOrganizationConformancePackStatuses'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    limit ::
      Lude.Maybe
        Lude.Natural,
    organizationConformancePackNames ::
      Lude.Maybe
        [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConformancePackStatuses' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of OrganizationConformancePackStatuses returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'organizationConformancePackNames' - The names of organization conformance packs for which you want status details. If you do not specify any names, AWS Config returns details for all your organization conformance packs.
mkDescribeOrganizationConformancePackStatuses ::
  DescribeOrganizationConformancePackStatuses
mkDescribeOrganizationConformancePackStatuses =
  DescribeOrganizationConformancePackStatuses'
    { nextToken =
        Lude.Nothing,
      limit = Lude.Nothing,
      organizationConformancePackNames = Lude.Nothing
    }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsNextToken :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Lude.Maybe Lude.Text)
docpsNextToken = Lens.lens (nextToken :: DescribeOrganizationConformancePackStatuses -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConformancePackStatuses)
{-# DEPRECATED docpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of OrganizationConformancePackStatuses returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsLimit :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Lude.Maybe Lude.Natural)
docpsLimit = Lens.lens (limit :: DescribeOrganizationConformancePackStatuses -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeOrganizationConformancePackStatuses)
{-# DEPRECATED docpsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The names of organization conformance packs for which you want status details. If you do not specify any names, AWS Config returns details for all your organization conformance packs.
--
-- /Note:/ Consider using 'organizationConformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsOrganizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePackStatuses (Lude.Maybe [Lude.Text])
docpsOrganizationConformancePackNames = Lens.lens (organizationConformancePackNames :: DescribeOrganizationConformancePackStatuses -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationConformancePackNames = a} :: DescribeOrganizationConformancePackStatuses)
{-# DEPRECATED docpsOrganizationConformancePackNames "Use generic-lens or generic-optics with 'organizationConformancePackNames' instead." #-}

instance
  Lude.AWSRequest
    DescribeOrganizationConformancePackStatuses
  where
  type
    Rs DescribeOrganizationConformancePackStatuses =
      DescribeOrganizationConformancePackStatusesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePackStatusesResponse'
            Lude.<$> ( x Lude..?> "OrganizationConformancePackStatuses"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganizationConformancePackStatuses where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeOrganizationConformancePackStatuses" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganizationConformancePackStatuses where
  toJSON DescribeOrganizationConformancePackStatuses' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("OrganizationConformancePackNames" Lude..=)
              Lude.<$> organizationConformancePackNames
          ]
      )

instance Lude.ToPath DescribeOrganizationConformancePackStatuses where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganizationConformancePackStatuses where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationConformancePackStatusesResponse' smart constructor.
data DescribeOrganizationConformancePackStatusesResponse = DescribeOrganizationConformancePackStatusesResponse'
  { organizationConformancePackStatuses ::
      Lude.Maybe
        [OrganizationConformancePackStatus],
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

-- | Creates a value of 'DescribeOrganizationConformancePackStatusesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'organizationConformancePackStatuses' - A list of @OrganizationConformancePackStatus@ objects.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationConformancePackStatusesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationConformancePackStatusesResponse
mkDescribeOrganizationConformancePackStatusesResponse
  pResponseStatus_ =
    DescribeOrganizationConformancePackStatusesResponse'
      { organizationConformancePackStatuses =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | A list of @OrganizationConformancePackStatus@ objects.
--
-- /Note:/ Consider using 'organizationConformancePackStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsrsOrganizationConformancePackStatuses :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse (Lude.Maybe [OrganizationConformancePackStatus])
docpsrsOrganizationConformancePackStatuses = Lens.lens (organizationConformancePackStatuses :: DescribeOrganizationConformancePackStatusesResponse -> Lude.Maybe [OrganizationConformancePackStatus]) (\s a -> s {organizationConformancePackStatuses = a} :: DescribeOrganizationConformancePackStatusesResponse)
{-# DEPRECATED docpsrsOrganizationConformancePackStatuses "Use generic-lens or generic-optics with 'organizationConformancePackStatuses' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsrsNextToken :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse (Lude.Maybe Lude.Text)
docpsrsNextToken = Lens.lens (nextToken :: DescribeOrganizationConformancePackStatusesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConformancePackStatusesResponse)
{-# DEPRECATED docpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpsrsResponseStatus :: Lens.Lens' DescribeOrganizationConformancePackStatusesResponse Lude.Int
docpsrsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationConformancePackStatusesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationConformancePackStatusesResponse)
{-# DEPRECATED docpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
