{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides one or more conformance packs deployment status.
module Network.AWS.Config.DescribeConformancePackStatus
  ( -- * Creating a request
    DescribeConformancePackStatus (..),
    mkDescribeConformancePackStatus,

    -- ** Request lenses
    dcpsConformancePackNames,
    dcpsNextToken,
    dcpsLimit,

    -- * Destructuring the response
    DescribeConformancePackStatusResponse (..),
    mkDescribeConformancePackStatusResponse,

    -- ** Response lenses
    dcpsrsConformancePackStatusDetails,
    dcpsrsNextToken,
    dcpsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { -- | Comma-separated list of conformance pack names.
    conformancePackNames :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of conformance packs status returned on each page.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConformancePackStatus' with the minimum fields required to make a request.
--
-- * 'conformancePackNames' - Comma-separated list of conformance pack names.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'limit' - The maximum number of conformance packs status returned on each page.
mkDescribeConformancePackStatus ::
  DescribeConformancePackStatus
mkDescribeConformancePackStatus =
  DescribeConformancePackStatus'
    { conformancePackNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Comma-separated list of conformance pack names.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsConformancePackNames :: Lens.Lens' DescribeConformancePackStatus (Lude.Maybe [Lude.Text])
dcpsConformancePackNames = Lens.lens (conformancePackNames :: DescribeConformancePackStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {conformancePackNames = a} :: DescribeConformancePackStatus)
{-# DEPRECATED dcpsConformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsNextToken :: Lens.Lens' DescribeConformancePackStatus (Lude.Maybe Lude.Text)
dcpsNextToken = Lens.lens (nextToken :: DescribeConformancePackStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConformancePackStatus)
{-# DEPRECATED dcpsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of conformance packs status returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsLimit :: Lens.Lens' DescribeConformancePackStatus (Lude.Maybe Lude.Natural)
dcpsLimit = Lens.lens (limit :: DescribeConformancePackStatus -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeConformancePackStatus)
{-# DEPRECATED dcpsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeConformancePackStatus where
  type
    Rs DescribeConformancePackStatus =
      DescribeConformancePackStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConformancePackStatusResponse'
            Lude.<$> (x Lude..?> "ConformancePackStatusDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConformancePackStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeConformancePackStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConformancePackStatus where
  toJSON DescribeConformancePackStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConformancePackNames" Lude..=) Lude.<$> conformancePackNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeConformancePackStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConformancePackStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeConformancePackStatusResponse' smart constructor.
data DescribeConformancePackStatusResponse = DescribeConformancePackStatusResponse'
  { -- | A list of @ConformancePackStatusDetail@ objects.
    conformancePackStatusDetails :: Lude.Maybe [ConformancePackStatusDetail],
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeConformancePackStatusResponse' with the minimum fields required to make a request.
--
-- * 'conformancePackStatusDetails' - A list of @ConformancePackStatusDetail@ objects.
-- * 'nextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
-- * 'responseStatus' - The response status code.
mkDescribeConformancePackStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConformancePackStatusResponse
mkDescribeConformancePackStatusResponse pResponseStatus_ =
  DescribeConformancePackStatusResponse'
    { conformancePackStatusDetails =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ConformancePackStatusDetail@ objects.
--
-- /Note:/ Consider using 'conformancePackStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsConformancePackStatusDetails :: Lens.Lens' DescribeConformancePackStatusResponse (Lude.Maybe [ConformancePackStatusDetail])
dcpsrsConformancePackStatusDetails = Lens.lens (conformancePackStatusDetails :: DescribeConformancePackStatusResponse -> Lude.Maybe [ConformancePackStatusDetail]) (\s a -> s {conformancePackStatusDetails = a} :: DescribeConformancePackStatusResponse)
{-# DEPRECATED dcpsrsConformancePackStatusDetails "Use generic-lens or generic-optics with 'conformancePackStatusDetails' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsNextToken :: Lens.Lens' DescribeConformancePackStatusResponse (Lude.Maybe Lude.Text)
dcpsrsNextToken = Lens.lens (nextToken :: DescribeConformancePackStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeConformancePackStatusResponse)
{-# DEPRECATED dcpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrsResponseStatus :: Lens.Lens' DescribeConformancePackStatusResponse Lude.Int
dcpsrsResponseStatus = Lens.lens (responseStatus :: DescribeConformancePackStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConformancePackStatusResponse)
{-# DEPRECATED dcpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
