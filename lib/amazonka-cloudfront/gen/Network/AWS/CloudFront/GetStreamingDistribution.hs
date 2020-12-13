{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified RTMP distribution, including the distribution configuration.
module Network.AWS.CloudFront.GetStreamingDistribution
  ( -- * Creating a request
    GetStreamingDistribution (..),
    mkGetStreamingDistribution,

    -- ** Request lenses
    gsdId,

    -- * Destructuring the response
    GetStreamingDistributionResponse (..),
    mkGetStreamingDistributionResponse,

    -- ** Response lenses
    gsdrsETag,
    gsdrsStreamingDistribution,
    gsdrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to get a streaming distribution's information.
--
-- /See:/ 'mkGetStreamingDistribution' smart constructor.
newtype GetStreamingDistribution = GetStreamingDistribution'
  { -- | The streaming distribution's ID.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStreamingDistribution' with the minimum fields required to make a request.
--
-- * 'id' - The streaming distribution's ID.
mkGetStreamingDistribution ::
  -- | 'id'
  Lude.Text ->
  GetStreamingDistribution
mkGetStreamingDistribution pId_ =
  GetStreamingDistribution' {id = pId_}

-- | The streaming distribution's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdId :: Lens.Lens' GetStreamingDistribution Lude.Text
gsdId = Lens.lens (id :: GetStreamingDistribution -> Lude.Text) (\s a -> s {id = a} :: GetStreamingDistribution)
{-# DEPRECATED gsdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetStreamingDistribution where
  type Rs GetStreamingDistribution = GetStreamingDistributionResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetStreamingDistributionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStreamingDistribution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetStreamingDistribution where
  toPath GetStreamingDistribution' {..} =
    Lude.mconcat
      ["/2020-05-31/streaming-distribution/", Lude.toBS id]

instance Lude.ToQuery GetStreamingDistribution where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetStreamingDistributionResponse' smart constructor.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse'
  { -- | The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | The streaming distribution's information.
    streamingDistribution :: Lude.Maybe StreamingDistribution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
-- * 'streamingDistribution' - The streaming distribution's information.
-- * 'responseStatus' - The response status code.
mkGetStreamingDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStreamingDistributionResponse
mkGetStreamingDistributionResponse pResponseStatus_ =
  GetStreamingDistributionResponse'
    { eTag = Lude.Nothing,
      streamingDistribution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsETag :: Lens.Lens' GetStreamingDistributionResponse (Lude.Maybe Lude.Text)
gsdrsETag = Lens.lens (eTag :: GetStreamingDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetStreamingDistributionResponse)
{-# DEPRECATED gsdrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsStreamingDistribution :: Lens.Lens' GetStreamingDistributionResponse (Lude.Maybe StreamingDistribution)
gsdrsStreamingDistribution = Lens.lens (streamingDistribution :: GetStreamingDistributionResponse -> Lude.Maybe StreamingDistribution) (\s a -> s {streamingDistribution = a} :: GetStreamingDistributionResponse)
{-# DEPRECATED gsdrsStreamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrsResponseStatus :: Lens.Lens' GetStreamingDistributionResponse Lude.Int
gsdrsResponseStatus = Lens.lens (responseStatus :: GetStreamingDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStreamingDistributionResponse)
{-# DEPRECATED gsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
