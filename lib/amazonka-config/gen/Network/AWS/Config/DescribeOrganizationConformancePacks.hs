{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization conformance packs.
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
module Network.AWS.Config.DescribeOrganizationConformancePacks
  ( -- * Creating a request
    DescribeOrganizationConformancePacks (..),
    mkDescribeOrganizationConformancePacks,

    -- ** Request lenses
    docpNextToken,
    docpLimit,
    docpOrganizationConformancePackNames,

    -- * Destructuring the response
    DescribeOrganizationConformancePacksResponse (..),
    mkDescribeOrganizationConformancePacksResponse,

    -- ** Response lenses
    docprsOrganizationConformancePacks,
    docprsNextToken,
    docprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOrganizationConformancePacks' smart constructor.
data DescribeOrganizationConformancePacks = DescribeOrganizationConformancePacks'
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

-- | Creates a value of 'DescribeOrganizationConformancePacks' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of organization config packs returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'organizationConformancePackNames' - The name that you assign to an organization conformance pack.
mkDescribeOrganizationConformancePacks ::
  DescribeOrganizationConformancePacks
mkDescribeOrganizationConformancePacks =
  DescribeOrganizationConformancePacks'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      organizationConformancePackNames = Lude.Nothing
    }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpNextToken :: Lens.Lens' DescribeOrganizationConformancePacks (Lude.Maybe Lude.Text)
docpNextToken = Lens.lens (nextToken :: DescribeOrganizationConformancePacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConformancePacks)
{-# DEPRECATED docpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of organization config packs returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpLimit :: Lens.Lens' DescribeOrganizationConformancePacks (Lude.Maybe Lude.Natural)
docpLimit = Lens.lens (limit :: DescribeOrganizationConformancePacks -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeOrganizationConformancePacks)
{-# DEPRECATED docpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name that you assign to an organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpOrganizationConformancePackNames :: Lens.Lens' DescribeOrganizationConformancePacks (Lude.Maybe [Lude.Text])
docpOrganizationConformancePackNames = Lens.lens (organizationConformancePackNames :: DescribeOrganizationConformancePacks -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationConformancePackNames = a} :: DescribeOrganizationConformancePacks)
{-# DEPRECATED docpOrganizationConformancePackNames "Use generic-lens or generic-optics with 'organizationConformancePackNames' instead." #-}

instance Lude.AWSRequest DescribeOrganizationConformancePacks where
  type
    Rs DescribeOrganizationConformancePacks =
      DescribeOrganizationConformancePacksResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrganizationConformancePacksResponse'
            Lude.<$> (x Lude..?> "OrganizationConformancePacks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrganizationConformancePacks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeOrganizationConformancePacks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrganizationConformancePacks where
  toJSON DescribeOrganizationConformancePacks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("OrganizationConformancePackNames" Lude..=)
              Lude.<$> organizationConformancePackNames
          ]
      )

instance Lude.ToPath DescribeOrganizationConformancePacks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrganizationConformancePacks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOrganizationConformancePacksResponse' smart constructor.
data DescribeOrganizationConformancePacksResponse = DescribeOrganizationConformancePacksResponse'
  { organizationConformancePacks ::
      Lude.Maybe
        [OrganizationConformancePack],
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrganizationConformancePacksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'organizationConformancePacks' - Returns a list of OrganizationConformancePacks objects.
-- * 'responseStatus' - The response status code.
mkDescribeOrganizationConformancePacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrganizationConformancePacksResponse
mkDescribeOrganizationConformancePacksResponse pResponseStatus_ =
  DescribeOrganizationConformancePacksResponse'
    { organizationConformancePacks =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of OrganizationConformancePacks objects.
--
-- /Note:/ Consider using 'organizationConformancePacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docprsOrganizationConformancePacks :: Lens.Lens' DescribeOrganizationConformancePacksResponse (Lude.Maybe [OrganizationConformancePack])
docprsOrganizationConformancePacks = Lens.lens (organizationConformancePacks :: DescribeOrganizationConformancePacksResponse -> Lude.Maybe [OrganizationConformancePack]) (\s a -> s {organizationConformancePacks = a} :: DescribeOrganizationConformancePacksResponse)
{-# DEPRECATED docprsOrganizationConformancePacks "Use generic-lens or generic-optics with 'organizationConformancePacks' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docprsNextToken :: Lens.Lens' DescribeOrganizationConformancePacksResponse (Lude.Maybe Lude.Text)
docprsNextToken = Lens.lens (nextToken :: DescribeOrganizationConformancePacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOrganizationConformancePacksResponse)
{-# DEPRECATED docprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docprsResponseStatus :: Lens.Lens' DescribeOrganizationConformancePacksResponse Lude.Int
docprsResponseStatus = Lens.lens (responseStatus :: DescribeOrganizationConformancePacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrganizationConformancePacksResponse)
{-# DEPRECATED docprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
