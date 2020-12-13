{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a streaming distribution.
module Network.AWS.CloudFront.GetStreamingDistributionConfig
  ( -- * Creating a request
    GetStreamingDistributionConfig (..),
    mkGetStreamingDistributionConfig,

    -- ** Request lenses
    gsdcId,

    -- * Destructuring the response
    GetStreamingDistributionConfigResponse (..),
    mkGetStreamingDistributionConfigResponse,

    -- ** Response lenses
    gsdcrsStreamingDistributionConfig,
    gsdcrsETag,
    gsdcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | To request to get a streaming distribution configuration.
--
-- /See:/ 'mkGetStreamingDistributionConfig' smart constructor.
newtype GetStreamingDistributionConfig = GetStreamingDistributionConfig'
  { -- | The streaming distribution's ID.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStreamingDistributionConfig' with the minimum fields required to make a request.
--
-- * 'id' - The streaming distribution's ID.
mkGetStreamingDistributionConfig ::
  -- | 'id'
  Lude.Text ->
  GetStreamingDistributionConfig
mkGetStreamingDistributionConfig pId_ =
  GetStreamingDistributionConfig' {id = pId_}

-- | The streaming distribution's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdcId :: Lens.Lens' GetStreamingDistributionConfig Lude.Text
gsdcId = Lens.lens (id :: GetStreamingDistributionConfig -> Lude.Text) (\s a -> s {id = a} :: GetStreamingDistributionConfig)
{-# DEPRECATED gsdcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetStreamingDistributionConfig where
  type
    Rs GetStreamingDistributionConfig =
      GetStreamingDistributionConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetStreamingDistributionConfigResponse'
            Lude.<$> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "ETag")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStreamingDistributionConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetStreamingDistributionConfig where
  toPath GetStreamingDistributionConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/streaming-distribution/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetStreamingDistributionConfig where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetStreamingDistributionConfigResponse' smart constructor.
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse'
  { -- | The streaming distribution's configuration information.
    streamingDistributionConfig :: Lude.Maybe StreamingDistributionConfig,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStreamingDistributionConfigResponse' with the minimum fields required to make a request.
--
-- * 'streamingDistributionConfig' - The streaming distribution's configuration information.
-- * 'eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'responseStatus' - The response status code.
mkGetStreamingDistributionConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStreamingDistributionConfigResponse
mkGetStreamingDistributionConfigResponse pResponseStatus_ =
  GetStreamingDistributionConfigResponse'
    { streamingDistributionConfig =
        Lude.Nothing,
      eTag = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdcrsStreamingDistributionConfig :: Lens.Lens' GetStreamingDistributionConfigResponse (Lude.Maybe StreamingDistributionConfig)
gsdcrsStreamingDistributionConfig = Lens.lens (streamingDistributionConfig :: GetStreamingDistributionConfigResponse -> Lude.Maybe StreamingDistributionConfig) (\s a -> s {streamingDistributionConfig = a} :: GetStreamingDistributionConfigResponse)
{-# DEPRECATED gsdcrsStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdcrsETag :: Lens.Lens' GetStreamingDistributionConfigResponse (Lude.Maybe Lude.Text)
gsdcrsETag = Lens.lens (eTag :: GetStreamingDistributionConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetStreamingDistributionConfigResponse)
{-# DEPRECATED gsdcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdcrsResponseStatus :: Lens.Lens' GetStreamingDistributionConfigResponse Lude.Int
gsdcrsResponseStatus = Lens.lens (responseStatus :: GetStreamingDistributionConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStreamingDistributionConfigResponse)
{-# DEPRECATED gsdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
