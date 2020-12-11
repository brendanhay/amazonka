{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListPublicKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all public keys that have been added to CloudFront for this account.
module Network.AWS.CloudFront.ListPublicKeys
  ( -- * Creating a request
    ListPublicKeys (..),
    mkListPublicKeys,

    -- ** Request lenses
    lpkMarker,
    lpkMaxItems,

    -- * Destructuring the response
    ListPublicKeysResponse (..),
    mkListPublicKeysResponse,

    -- ** Response lenses
    lpkrsPublicKeyList,
    lpkrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublicKeys' with the minimum fields required to make a request.
--
-- * 'marker' - Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page).
-- * 'maxItems' - The maximum number of public keys you want in the response body.
mkListPublicKeys ::
  ListPublicKeys
mkListPublicKeys =
  ListPublicKeys' {marker = Lude.Nothing, maxItems = Lude.Nothing}

-- | Use this when paginating results to indicate where to begin in your list of public keys. The results include public keys in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last public key on that page).
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkMarker :: Lens.Lens' ListPublicKeys (Lude.Maybe Lude.Text)
lpkMarker = Lens.lens (marker :: ListPublicKeys -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPublicKeys)
{-# DEPRECATED lpkMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of public keys you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkMaxItems :: Lens.Lens' ListPublicKeys (Lude.Maybe Lude.Text)
lpkMaxItems = Lens.lens (maxItems :: ListPublicKeys -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListPublicKeys)
{-# DEPRECATED lpkMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListPublicKeys where
  type Rs ListPublicKeys = ListPublicKeysResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListPublicKeysResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPublicKeys where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPublicKeys where
  toPath = Lude.const "/2020-05-31/public-key"

instance Lude.ToQuery ListPublicKeys where
  toQuery ListPublicKeys' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { publicKeyList ::
      Lude.Maybe PublicKeyList,
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

-- | Creates a value of 'ListPublicKeysResponse' with the minimum fields required to make a request.
--
-- * 'publicKeyList' - Returns a list of all public keys that have been added to CloudFront for this account.
-- * 'responseStatus' - The response status code.
mkListPublicKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPublicKeysResponse
mkListPublicKeysResponse pResponseStatus_ =
  ListPublicKeysResponse'
    { publicKeyList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a list of all public keys that have been added to CloudFront for this account.
--
-- /Note:/ Consider using 'publicKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrsPublicKeyList :: Lens.Lens' ListPublicKeysResponse (Lude.Maybe PublicKeyList)
lpkrsPublicKeyList = Lens.lens (publicKeyList :: ListPublicKeysResponse -> Lude.Maybe PublicKeyList) (\s a -> s {publicKeyList = a} :: ListPublicKeysResponse)
{-# DEPRECATED lpkrsPublicKeyList "Use generic-lens or generic-optics with 'publicKeyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrsResponseStatus :: Lens.Lens' ListPublicKeysResponse Lude.Int
lpkrsResponseStatus = Lens.lens (responseStatus :: ListPublicKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPublicKeysResponse)
{-# DEPRECATED lpkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
