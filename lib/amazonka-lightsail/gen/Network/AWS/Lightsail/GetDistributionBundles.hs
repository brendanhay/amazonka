{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributionBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list bundles that can be applied to you Amazon Lightsail content delivery network (CDN) distributions.
--
-- A distribution bundle specifies the monthly network transfer quota and monthly cost of your dsitribution.
module Network.AWS.Lightsail.GetDistributionBundles
  ( -- * Creating a request
    GetDistributionBundles (..),
    mkGetDistributionBundles,

    -- * Destructuring the response
    GetDistributionBundlesResponse (..),
    mkGetDistributionBundlesResponse,

    -- ** Response lenses
    gdbrsBundles,
    gdbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDistributionBundles' smart constructor.
data GetDistributionBundles = GetDistributionBundles'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionBundles' with the minimum fields required to make a request.
mkGetDistributionBundles ::
  GetDistributionBundles
mkGetDistributionBundles = GetDistributionBundles'

instance Lude.AWSRequest GetDistributionBundles where
  type Rs GetDistributionBundles = GetDistributionBundlesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDistributionBundlesResponse'
            Lude.<$> (x Lude..?> "bundles" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDistributionBundles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDistributionBundles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDistributionBundles where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetDistributionBundles where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDistributionBundles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDistributionBundlesResponse' smart constructor.
data GetDistributionBundlesResponse = GetDistributionBundlesResponse'
  { bundles ::
      Lude.Maybe
        [DistributionBundle],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDistributionBundlesResponse' with the minimum fields required to make a request.
--
-- * 'bundles' - An object that describes a distribution bundle.
-- * 'responseStatus' - The response status code.
mkGetDistributionBundlesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDistributionBundlesResponse
mkGetDistributionBundlesResponse pResponseStatus_ =
  GetDistributionBundlesResponse'
    { bundles = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes a distribution bundle.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdbrsBundles :: Lens.Lens' GetDistributionBundlesResponse (Lude.Maybe [DistributionBundle])
gdbrsBundles = Lens.lens (bundles :: GetDistributionBundlesResponse -> Lude.Maybe [DistributionBundle]) (\s a -> s {bundles = a} :: GetDistributionBundlesResponse)
{-# DEPRECATED gdbrsBundles "Use generic-lens or generic-optics with 'bundles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdbrsResponseStatus :: Lens.Lens' GetDistributionBundlesResponse Lude.Int
gdbrsResponseStatus = Lens.lens (responseStatus :: GetDistributionBundlesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDistributionBundlesResponse)
{-# DEPRECATED gdbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
