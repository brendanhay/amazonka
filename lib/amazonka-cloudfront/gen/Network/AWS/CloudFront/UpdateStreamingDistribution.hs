{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a streaming distribution.
module Network.AWS.CloudFront.UpdateStreamingDistribution
  ( -- * Creating a request
    UpdateStreamingDistribution (..),
    mkUpdateStreamingDistribution,

    -- ** Request lenses
    usdIfMatch,
    usdStreamingDistributionConfig,
    usdId,

    -- * Destructuring the response
    UpdateStreamingDistributionResponse (..),
    mkUpdateStreamingDistributionResponse,

    -- ** Response lenses
    usdrsETag,
    usdrsStreamingDistribution,
    usdrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to update a streaming distribution.
--
-- /See:/ 'mkUpdateStreamingDistribution' smart constructor.
data UpdateStreamingDistribution = UpdateStreamingDistribution'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    streamingDistributionConfig ::
      StreamingDistributionConfig,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStreamingDistribution' with the minimum fields required to make a request.
--
-- * 'id' - The streaming distribution's id.
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'streamingDistributionConfig' - The streaming distribution's configuration information.
mkUpdateStreamingDistribution ::
  -- | 'streamingDistributionConfig'
  StreamingDistributionConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateStreamingDistribution
mkUpdateStreamingDistribution pStreamingDistributionConfig_ pId_ =
  UpdateStreamingDistribution'
    { ifMatch = Lude.Nothing,
      streamingDistributionConfig = pStreamingDistributionConfig_,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdIfMatch :: Lens.Lens' UpdateStreamingDistribution (Lude.Maybe Lude.Text)
usdIfMatch = Lens.lens (ifMatch :: UpdateStreamingDistribution -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateStreamingDistribution)
{-# DEPRECATED usdIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdStreamingDistributionConfig :: Lens.Lens' UpdateStreamingDistribution StreamingDistributionConfig
usdStreamingDistributionConfig = Lens.lens (streamingDistributionConfig :: UpdateStreamingDistribution -> StreamingDistributionConfig) (\s a -> s {streamingDistributionConfig = a} :: UpdateStreamingDistribution)
{-# DEPRECATED usdStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

-- | The streaming distribution's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdId :: Lens.Lens' UpdateStreamingDistribution Lude.Text
usdId = Lens.lens (id :: UpdateStreamingDistribution -> Lude.Text) (\s a -> s {id = a} :: UpdateStreamingDistribution)
{-# DEPRECATED usdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateStreamingDistribution where
  type
    Rs UpdateStreamingDistribution =
      UpdateStreamingDistributionResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateStreamingDistributionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateStreamingDistribution where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}StreamingDistributionConfig"
      Lude.. streamingDistributionConfig

instance Lude.ToHeaders UpdateStreamingDistribution where
  toHeaders UpdateStreamingDistribution' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateStreamingDistribution where
  toPath UpdateStreamingDistribution' {..} =
    Lude.mconcat
      ["/2020-05-31/streaming-distribution/", Lude.toBS id, "/config"]

instance Lude.ToQuery UpdateStreamingDistribution where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateStreamingDistributionResponse' smart constructor.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
  { eTag ::
      Lude.Maybe
        Lude.Text,
    streamingDistribution ::
      Lude.Maybe
        StreamingDistribution,
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

-- | Creates a value of 'UpdateStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'responseStatus' - The response status code.
-- * 'streamingDistribution' - The streaming distribution's information.
mkUpdateStreamingDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStreamingDistributionResponse
mkUpdateStreamingDistributionResponse pResponseStatus_ =
  UpdateStreamingDistributionResponse'
    { eTag = Lude.Nothing,
      streamingDistribution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrsETag :: Lens.Lens' UpdateStreamingDistributionResponse (Lude.Maybe Lude.Text)
usdrsETag = Lens.lens (eTag :: UpdateStreamingDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateStreamingDistributionResponse)
{-# DEPRECATED usdrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrsStreamingDistribution :: Lens.Lens' UpdateStreamingDistributionResponse (Lude.Maybe StreamingDistribution)
usdrsStreamingDistribution = Lens.lens (streamingDistribution :: UpdateStreamingDistributionResponse -> Lude.Maybe StreamingDistribution) (\s a -> s {streamingDistribution = a} :: UpdateStreamingDistributionResponse)
{-# DEPRECATED usdrsStreamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrsResponseStatus :: Lens.Lens' UpdateStreamingDistributionResponse Lude.Int
usdrsResponseStatus = Lens.lens (responseStatus :: UpdateStreamingDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStreamingDistributionResponse)
{-# DEPRECATED usdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
