{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetKeyPairs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all key pairs in the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetKeyPairs
  ( -- * Creating a request
    GetKeyPairs (..),
    mkGetKeyPairs,

    -- ** Request lenses
    gkpPageToken,

    -- * Destructuring the response
    GetKeyPairsResponse (..),
    mkGetKeyPairsResponse,

    -- ** Response lenses
    gkpsrsNextPageToken,
    gkpsrsKeyPairs,
    gkpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetKeyPairs' smart constructor.
newtype GetKeyPairs = GetKeyPairs'
  { pageToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyPairs' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetKeyPairs ::
  GetKeyPairs
mkGetKeyPairs = GetKeyPairs' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpPageToken :: Lens.Lens' GetKeyPairs (Lude.Maybe Lude.Text)
gkpPageToken = Lens.lens (pageToken :: GetKeyPairs -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetKeyPairs)
{-# DEPRECATED gkpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetKeyPairs where
  page rq rs
    | Page.stop (rs Lens.^. gkpsrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gkpsrsKeyPairs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gkpPageToken Lens..~ rs Lens.^. gkpsrsNextPageToken

instance Lude.AWSRequest GetKeyPairs where
  type Rs GetKeyPairs = GetKeyPairsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetKeyPairsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "keyPairs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetKeyPairs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetKeyPairs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetKeyPairs where
  toJSON GetKeyPairs' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetKeyPairs where
  toPath = Lude.const "/"

instance Lude.ToQuery GetKeyPairs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetKeyPairsResponse' smart constructor.
data GetKeyPairsResponse = GetKeyPairsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    keyPairs :: Lude.Maybe [KeyPair],
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

-- | Creates a value of 'GetKeyPairsResponse' with the minimum fields required to make a request.
--
-- * 'keyPairs' - An array of key-value pairs containing information about the key pairs.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetKeyPairs@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetKeyPairsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetKeyPairsResponse
mkGetKeyPairsResponse pResponseStatus_ =
  GetKeyPairsResponse'
    { nextPageToken = Lude.Nothing,
      keyPairs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetKeyPairs@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpsrsNextPageToken :: Lens.Lens' GetKeyPairsResponse (Lude.Maybe Lude.Text)
gkpsrsNextPageToken = Lens.lens (nextPageToken :: GetKeyPairsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetKeyPairsResponse)
{-# DEPRECATED gkpsrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of key-value pairs containing information about the key pairs.
--
-- /Note:/ Consider using 'keyPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpsrsKeyPairs :: Lens.Lens' GetKeyPairsResponse (Lude.Maybe [KeyPair])
gkpsrsKeyPairs = Lens.lens (keyPairs :: GetKeyPairsResponse -> Lude.Maybe [KeyPair]) (\s a -> s {keyPairs = a} :: GetKeyPairsResponse)
{-# DEPRECATED gkpsrsKeyPairs "Use generic-lens or generic-optics with 'keyPairs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpsrsResponseStatus :: Lens.Lens' GetKeyPairsResponse Lude.Int
gkpsrsResponseStatus = Lens.lens (responseStatus :: GetKeyPairsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetKeyPairsResponse)
{-# DEPRECATED gkpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
