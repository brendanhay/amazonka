{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.ListTrails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists trails that are in the current account.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListTrails
  ( -- * Creating a request
    ListTrails (..),
    mkListTrails,

    -- ** Request lenses
    lNextToken,

    -- * Destructuring the response
    ListTrailsResponse (..),
    mkListTrailsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsTrails,
    lrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTrails' smart constructor.
newtype ListTrails = ListTrails' {nextToken :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrails' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
mkListTrails ::
  ListTrails
mkListTrails = ListTrails' {nextToken = Lude.Nothing}

-- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListTrails (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListTrails -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrails)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListTrails where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsTrails) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListTrails where
  type Rs ListTrails = ListTrailsResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTrailsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Trails" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTrails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTrails" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTrails where
  toJSON ListTrails' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath ListTrails where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTrails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTrailsResponse' smart constructor.
data ListTrailsResponse = ListTrailsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    trails :: Lude.Maybe [TrailInfo],
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

-- | Creates a value of 'ListTrailsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
-- * 'responseStatus' - The response status code.
-- * 'trails' - Returns the name, ARN, and home region of trails in the current account.
mkListTrailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTrailsResponse
mkListTrailsResponse pResponseStatus_ =
  ListTrailsResponse'
    { nextToken = Lude.Nothing,
      trails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListTrailsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListTrailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrailsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns the name, ARN, and home region of trails in the current account.
--
-- /Note:/ Consider using 'trails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsTrails :: Lens.Lens' ListTrailsResponse (Lude.Maybe [TrailInfo])
lrsTrails = Lens.lens (trails :: ListTrailsResponse -> Lude.Maybe [TrailInfo]) (\s a -> s {trails = a} :: ListTrailsResponse)
{-# DEPRECATED lrsTrails "Use generic-lens or generic-optics with 'trails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListTrailsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListTrailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrailsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
