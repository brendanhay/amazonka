{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.DescribeBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the bundle details for the requested bundle id.
module Network.AWS.Mobile.DescribeBundle
  ( -- * Creating a request
    DescribeBundle (..),
    mkDescribeBundle,

    -- ** Request lenses
    dbBundleId,

    -- * Destructuring the response
    DescribeBundleResponse (..),
    mkDescribeBundleResponse,

    -- ** Response lenses
    dbrsDetails,
    dbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure to request the details of a specific bundle.
--
-- /See:/ 'mkDescribeBundle' smart constructor.
newtype DescribeBundle = DescribeBundle' {bundleId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBundle' with the minimum fields required to make a request.
--
-- * 'bundleId' - Unique bundle identifier.
mkDescribeBundle ::
  -- | 'bundleId'
  Lude.Text ->
  DescribeBundle
mkDescribeBundle pBundleId_ =
  DescribeBundle' {bundleId = pBundleId_}

-- | Unique bundle identifier.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBundleId :: Lens.Lens' DescribeBundle Lude.Text
dbBundleId = Lens.lens (bundleId :: DescribeBundle -> Lude.Text) (\s a -> s {bundleId = a} :: DescribeBundle)
{-# DEPRECATED dbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

instance Lude.AWSRequest DescribeBundle where
  type Rs DescribeBundle = DescribeBundleResponse
  request = Req.get mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBundleResponse'
            Lude.<$> (x Lude..?> "details") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBundle where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeBundle where
  toPath DescribeBundle' {..} =
    Lude.mconcat ["/bundles/", Lude.toBS bundleId]

instance Lude.ToQuery DescribeBundle where
  toQuery = Lude.const Lude.mempty

-- | Result structure contains the details of the bundle.
--
-- /See:/ 'mkDescribeBundleResponse' smart constructor.
data DescribeBundleResponse = DescribeBundleResponse'
  { details ::
      Lude.Maybe BundleDetails,
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

-- | Creates a value of 'DescribeBundleResponse' with the minimum fields required to make a request.
--
-- * 'details' - The details of the bundle.
-- * 'responseStatus' - The response status code.
mkDescribeBundleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBundleResponse
mkDescribeBundleResponse pResponseStatus_ =
  DescribeBundleResponse'
    { details = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the bundle.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsDetails :: Lens.Lens' DescribeBundleResponse (Lude.Maybe BundleDetails)
dbrsDetails = Lens.lens (details :: DescribeBundleResponse -> Lude.Maybe BundleDetails) (\s a -> s {details = a} :: DescribeBundleResponse)
{-# DEPRECATED dbrsDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DescribeBundleResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DescribeBundleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBundleResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
