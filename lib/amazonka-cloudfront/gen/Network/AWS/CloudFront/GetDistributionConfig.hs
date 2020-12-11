{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a distribution.
module Network.AWS.CloudFront.GetDistributionConfig
  ( -- * Creating a request
    GetDistributionConfig (..),
    mkGetDistributionConfig,

    -- ** Request lenses
    gdcId,

    -- * Destructuring the response
    GetDistributionConfigResponse (..),
    mkGetDistributionConfigResponse,

    -- ** Response lenses
    gdcrsETag,
    gdcrsDistributionConfig,
    gdcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to get a distribution configuration.
--
-- /See:/ 'mkGetDistributionConfig' smart constructor.
newtype GetDistributionConfig = GetDistributionConfig'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionConfig' with the minimum fields required to make a request.
--
-- * 'id' - The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
mkGetDistributionConfig ::
  -- | 'id'
  Lude.Text ->
  GetDistributionConfig
mkGetDistributionConfig pId_ = GetDistributionConfig' {id = pId_}

-- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcId :: Lens.Lens' GetDistributionConfig Lude.Text
gdcId = Lens.lens (id :: GetDistributionConfig -> Lude.Text) (\s a -> s {id = a} :: GetDistributionConfig)
{-# DEPRECATED gdcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetDistributionConfig where
  type Rs GetDistributionConfig = GetDistributionConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetDistributionConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDistributionConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetDistributionConfig where
  toPath GetDistributionConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/distribution/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetDistributionConfig where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetDistributionConfigResponse' smart constructor.
data GetDistributionConfigResponse = GetDistributionConfigResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    distributionConfig ::
      Lude.Maybe DistributionConfig,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionConfigResponse' with the minimum fields required to make a request.
--
-- * 'distributionConfig' - The distribution's configuration information.
-- * 'eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'responseStatus' - The response status code.
mkGetDistributionConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDistributionConfigResponse
mkGetDistributionConfigResponse pResponseStatus_ =
  GetDistributionConfigResponse'
    { eTag = Lude.Nothing,
      distributionConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsETag :: Lens.Lens' GetDistributionConfigResponse (Lude.Maybe Lude.Text)
gdcrsETag = Lens.lens (eTag :: GetDistributionConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetDistributionConfigResponse)
{-# DEPRECATED gdcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsDistributionConfig :: Lens.Lens' GetDistributionConfigResponse (Lude.Maybe DistributionConfig)
gdcrsDistributionConfig = Lens.lens (distributionConfig :: GetDistributionConfigResponse -> Lude.Maybe DistributionConfig) (\s a -> s {distributionConfig = a} :: GetDistributionConfigResponse)
{-# DEPRECATED gdcrsDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsResponseStatus :: Lens.Lens' GetDistributionConfigResponse Lude.Int
gdcrsResponseStatus = Lens.lens (responseStatus :: GetDistributionConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDistributionConfigResponse)
{-# DEPRECATED gdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
