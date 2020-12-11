{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateDistributionBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bundle of your Amazon Lightsail content delivery network (CDN) distribution.
--
-- A distribution bundle specifies the monthly network transfer quota and monthly cost of your dsitribution.
-- Update your distribution's bundle if your distribution is going over its monthly network transfer quota and is incurring an overage fee.
-- You can update your distribution's bundle only one time within your monthly AWS billing cycle. To determine if you can update your distribution's bundle, use the @GetDistributions@ action. The @ableToUpdateBundle@ parameter in the result will indicate whether you can currently update your distribution's bundle.
module Network.AWS.Lightsail.UpdateDistributionBundle
  ( -- * Creating a request
    UpdateDistributionBundle (..),
    mkUpdateDistributionBundle,

    -- ** Request lenses
    udbBundleId,
    udbDistributionName,

    -- * Destructuring the response
    UpdateDistributionBundleResponse (..),
    mkUpdateDistributionBundleResponse,

    -- ** Response lenses
    udbrsOperation,
    udbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDistributionBundle' smart constructor.
data UpdateDistributionBundle = UpdateDistributionBundle'
  { bundleId ::
      Lude.Maybe Lude.Text,
    distributionName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDistributionBundle' with the minimum fields required to make a request.
--
-- * 'bundleId' - The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
-- * 'distributionName' - The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
mkUpdateDistributionBundle ::
  UpdateDistributionBundle
mkUpdateDistributionBundle =
  UpdateDistributionBundle'
    { bundleId = Lude.Nothing,
      distributionName = Lude.Nothing
    }

-- | The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbBundleId :: Lens.Lens' UpdateDistributionBundle (Lude.Maybe Lude.Text)
udbBundleId = Lens.lens (bundleId :: UpdateDistributionBundle -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: UpdateDistributionBundle)
{-# DEPRECATED udbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbDistributionName :: Lens.Lens' UpdateDistributionBundle (Lude.Maybe Lude.Text)
udbDistributionName = Lens.lens (distributionName :: UpdateDistributionBundle -> Lude.Maybe Lude.Text) (\s a -> s {distributionName = a} :: UpdateDistributionBundle)
{-# DEPRECATED udbDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Lude.AWSRequest UpdateDistributionBundle where
  type Rs UpdateDistributionBundle = UpdateDistributionBundleResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDistributionBundleResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDistributionBundle where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.UpdateDistributionBundle" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDistributionBundle where
  toJSON UpdateDistributionBundle' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bundleId" Lude..=) Lude.<$> bundleId,
            ("distributionName" Lude..=) Lude.<$> distributionName
          ]
      )

instance Lude.ToPath UpdateDistributionBundle where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDistributionBundle where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDistributionBundleResponse' smart constructor.
data UpdateDistributionBundleResponse = UpdateDistributionBundleResponse'
  { operation ::
      Lude.Maybe Operation,
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

-- | Creates a value of 'UpdateDistributionBundleResponse' with the minimum fields required to make a request.
--
-- * 'operation' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateDistributionBundleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDistributionBundleResponse
mkUpdateDistributionBundleResponse pResponseStatus_ =
  UpdateDistributionBundleResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbrsOperation :: Lens.Lens' UpdateDistributionBundleResponse (Lude.Maybe Operation)
udbrsOperation = Lens.lens (operation :: UpdateDistributionBundleResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: UpdateDistributionBundleResponse)
{-# DEPRECATED udbrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbrsResponseStatus :: Lens.Lens' UpdateDistributionBundleResponse Lude.Int
udbrsResponseStatus = Lens.lens (responseStatus :: UpdateDistributionBundleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDistributionBundleResponse)
{-# DEPRECATED udbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
