{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail content delivery network (CDN) distributions.
module Network.AWS.Lightsail.GetDistributions
  ( -- * Creating a request
    GetDistributions (..),
    mkGetDistributions,

    -- ** Request lenses
    gdgDistributionName,
    gdgPageToken,

    -- * Destructuring the response
    GetDistributionsResponse (..),
    mkGetDistributionsResponse,

    -- ** Response lenses
    gdhrsNextPageToken,
    gdhrsDistributions,
    gdhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { -- | The name of the distribution for which to return information.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    -- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
    distributionName :: Lude.Maybe Lude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributions' with the minimum fields required to make a request.
--
-- * 'distributionName' - The name of the distribution for which to return information.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetDistributions ::
  GetDistributions
mkGetDistributions =
  GetDistributions'
    { distributionName = Lude.Nothing,
      pageToken = Lude.Nothing
    }

-- | The name of the distribution for which to return information.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes all of your distributions in the AWS Region where the request is made.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgDistributionName :: Lens.Lens' GetDistributions (Lude.Maybe Lude.Text)
gdgDistributionName = Lens.lens (distributionName :: GetDistributions -> Lude.Maybe Lude.Text) (\s a -> s {distributionName = a} :: GetDistributions)
{-# DEPRECATED gdgDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgPageToken :: Lens.Lens' GetDistributions (Lude.Maybe Lude.Text)
gdgPageToken = Lens.lens (pageToken :: GetDistributions -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetDistributions)
{-# DEPRECATED gdgPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Lude.AWSRequest GetDistributions where
  type Rs GetDistributions = GetDistributionsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDistributionsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "distributions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDistributions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDistributions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDistributions where
  toJSON GetDistributions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("distributionName" Lude..=) Lude.<$> distributionName,
            ("pageToken" Lude..=) Lude.<$> pageToken
          ]
      )

instance Lude.ToPath GetDistributions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDistributions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects that describe your distributions.
    distributions :: Lude.Maybe [LightsailDistribution],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
-- * 'distributions' - An array of objects that describe your distributions.
-- * 'responseStatus' - The response status code.
mkGetDistributionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDistributionsResponse
mkGetDistributionsResponse pResponseStatus_ =
  GetDistributionsResponse'
    { nextPageToken = Lude.Nothing,
      distributions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDistributions@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdhrsNextPageToken :: Lens.Lens' GetDistributionsResponse (Lude.Maybe Lude.Text)
gdhrsNextPageToken = Lens.lens (nextPageToken :: GetDistributionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDistributionsResponse)
{-# DEPRECATED gdhrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects that describe your distributions.
--
-- /Note:/ Consider using 'distributions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdhrsDistributions :: Lens.Lens' GetDistributionsResponse (Lude.Maybe [LightsailDistribution])
gdhrsDistributions = Lens.lens (distributions :: GetDistributionsResponse -> Lude.Maybe [LightsailDistribution]) (\s a -> s {distributions = a} :: GetDistributionsResponse)
{-# DEPRECATED gdhrsDistributions "Use generic-lens or generic-optics with 'distributions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdhrsResponseStatus :: Lens.Lens' GetDistributionsResponse Lude.Int
gdhrsResponseStatus = Lens.lens (responseStatus :: GetDistributionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDistributionsResponse)
{-# DEPRECATED gdhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
