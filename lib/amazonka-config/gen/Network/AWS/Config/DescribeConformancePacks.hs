{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of one or more conformance packs.
module Network.AWS.Config.DescribeConformancePacks
  ( -- * Creating a request
    DescribeConformancePacks (..),
    mkDescribeConformancePacks,

    -- ** Request lenses
    dcpConformancePackNames,
    dcpNextToken,
    dcpLimit,

    -- * Destructuring the response
    DescribeConformancePacksResponse (..),
    mkDescribeConformancePacksResponse,

    -- ** Response lenses
    dcprsNextToken,
    dcprsConformancePackDetails,
    dcprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { conformancePackNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConformancePacks' with the minimum fields required to make a request.
--
-- * 'conformancePackNames' - Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs.
-- * 'limit' - The maximum number of conformance packs returned on each page.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
mkDescribeConformancePacks ::
  DescribeConformancePacks
mkDescribeConformancePacks =
  DescribeConformancePacks'
    { conformancePackNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConformancePackNames :: Lens.Lens' DescribeConformancePacks (Lude.Maybe [Lude.Text])
dcpConformancePackNames = Lens.lens (conformancePackNames :: DescribeConformancePacks -> Lude.Maybe [Lude.Text]) (\s a -> s {conformancePackNames = a} :: DescribeConformancePacks)
{-# DEPRECATED dcpConformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeConformancePacks (Lude.Maybe Lude.Text)
dcpNextToken = Lens.lens (nextToken :: DescribeConformancePacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConformancePacks)
{-# DEPRECATED dcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of conformance packs returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpLimit :: Lens.Lens' DescribeConformancePacks (Lude.Maybe Lude.Natural)
dcpLimit = Lens.lens (limit :: DescribeConformancePacks -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConformancePacks)
{-# DEPRECATED dcpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeConformancePacks where
  type Rs DescribeConformancePacks = DescribeConformancePacksResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConformancePacksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ConformancePackDetails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConformancePacks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConformancePacks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConformancePacks where
  toJSON DescribeConformancePacks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConformancePackNames" Lude..=) Lude.<$> conformancePackNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeConformancePacks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConformancePacks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    conformancePackDetails ::
      Lude.Maybe
        [ConformancePackDetail],
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

-- | Creates a value of 'DescribeConformancePacksResponse' with the minimum fields required to make a request.
--
-- * 'conformancePackDetails' - Returns a list of @ConformancePackDetail@ objects.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeConformancePacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConformancePacksResponse
mkDescribeConformancePacksResponse pResponseStatus_ =
  DescribeConformancePacksResponse'
    { nextToken = Lude.Nothing,
      conformancePackDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsNextToken :: Lens.Lens' DescribeConformancePacksResponse (Lude.Maybe Lude.Text)
dcprsNextToken = Lens.lens (nextToken :: DescribeConformancePacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConformancePacksResponse)
{-# DEPRECATED dcprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list of @ConformancePackDetail@ objects.
--
-- /Note:/ Consider using 'conformancePackDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsConformancePackDetails :: Lens.Lens' DescribeConformancePacksResponse (Lude.Maybe [ConformancePackDetail])
dcprsConformancePackDetails = Lens.lens (conformancePackDetails :: DescribeConformancePacksResponse -> Lude.Maybe [ConformancePackDetail]) (\s a -> s {conformancePackDetails = a} :: DescribeConformancePacksResponse)
{-# DEPRECATED dcprsConformancePackDetails "Use generic-lens or generic-optics with 'conformancePackDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DescribeConformancePacksResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DescribeConformancePacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConformancePacksResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
