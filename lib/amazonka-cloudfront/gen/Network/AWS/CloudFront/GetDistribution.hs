{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about a distribution.
module Network.AWS.CloudFront.GetDistribution
  ( -- * Creating a request
    GetDistribution (..),
    mkGetDistribution,

    -- ** Request lenses
    gdId,

    -- * Destructuring the response
    GetDistributionResponse (..),
    mkGetDistributionResponse,

    -- ** Response lenses
    gdrsETag,
    gdrsDistribution,
    gdrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to get a distribution's information.
--
-- /See:/ 'mkGetDistribution' smart constructor.
newtype GetDistribution = GetDistribution'
  { -- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistribution' with the minimum fields required to make a request.
--
-- * 'id' - The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
mkGetDistribution ::
  -- | 'id'
  Lude.Text ->
  GetDistribution
mkGetDistribution pId_ = GetDistribution' {id = pId_}

-- | The distribution's ID. If the ID is empty, an empty distribution configuration is returned.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdId :: Lens.Lens' GetDistribution Lude.Text
gdId = Lens.lens (id :: GetDistribution -> Lude.Text) (\s a -> s {id = a} :: GetDistribution)
{-# DEPRECATED gdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetDistribution where
  type Rs GetDistribution = GetDistributionResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetDistributionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDistribution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetDistribution where
  toPath GetDistribution' {..} =
    Lude.mconcat ["/2020-05-31/distribution/", Lude.toBS id]

instance Lude.ToQuery GetDistribution where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkGetDistributionResponse' smart constructor.
data GetDistributionResponse = GetDistributionResponse'
  { -- | The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | The distribution's information.
    distribution :: Lude.Maybe Distribution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
-- * 'distribution' - The distribution's information.
-- * 'responseStatus' - The response status code.
mkGetDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDistributionResponse
mkGetDistributionResponse pResponseStatus_ =
  GetDistributionResponse'
    { eTag = Lude.Nothing,
      distribution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsETag :: Lens.Lens' GetDistributionResponse (Lude.Maybe Lude.Text)
gdrsETag = Lens.lens (eTag :: GetDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetDistributionResponse)
{-# DEPRECATED gdrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDistribution :: Lens.Lens' GetDistributionResponse (Lude.Maybe Distribution)
gdrsDistribution = Lens.lens (distribution :: GetDistributionResponse -> Lude.Maybe Distribution) (\s a -> s {distribution = a} :: GetDistributionResponse)
{-# DEPRECATED gdrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDistributionResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDistributionResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
