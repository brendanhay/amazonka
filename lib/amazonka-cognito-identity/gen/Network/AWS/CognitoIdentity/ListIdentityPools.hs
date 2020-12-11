{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Cognito identity pools registered for your account.
--
-- You must use AWS Developer credentials to call this API.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentity.ListIdentityPools
  ( -- * Creating a request
    ListIdentityPools (..),
    mkListIdentityPools,

    -- ** Request lenses
    lipNextToken,
    lipMaxResults,

    -- * Destructuring the response
    ListIdentityPoolsResponse (..),
    mkListIdentityPoolsResponse,

    -- ** Response lenses
    liprsIdentityPools,
    liprsNextToken,
    liprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the ListIdentityPools action.
--
-- /See:/ 'mkListIdentityPools' smart constructor.
data ListIdentityPools = ListIdentityPools'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIdentityPools' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of identities to return.
-- * 'nextToken' - A pagination token.
mkListIdentityPools ::
  -- | 'maxResults'
  Lude.Natural ->
  ListIdentityPools
mkListIdentityPools pMaxResults_ =
  ListIdentityPools'
    { nextToken = Lude.Nothing,
      maxResults = pMaxResults_
    }

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipNextToken :: Lens.Lens' ListIdentityPools (Lude.Maybe Lude.Text)
lipNextToken = Lens.lens (nextToken :: ListIdentityPools -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentityPools)
{-# DEPRECATED lipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of identities to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxResults :: Lens.Lens' ListIdentityPools Lude.Natural
lipMaxResults = Lens.lens (maxResults :: ListIdentityPools -> Lude.Natural) (\s a -> s {maxResults = a} :: ListIdentityPools)
{-# DEPRECATED lipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListIdentityPools where
  page rq rs
    | Page.stop (rs Lens.^. liprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. liprsIdentityPools) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lipNextToken Lens..~ rs Lens.^. liprsNextToken

instance Lude.AWSRequest ListIdentityPools where
  type Rs ListIdentityPools = ListIdentityPoolsResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIdentityPoolsResponse'
            Lude.<$> (x Lude..?> "IdentityPools" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIdentityPools where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.ListIdentityPools" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListIdentityPools where
  toJSON ListIdentityPools' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("MaxResults" Lude..= maxResults)
          ]
      )

instance Lude.ToPath ListIdentityPools where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIdentityPools where
  toQuery = Lude.const Lude.mempty

-- | The result of a successful ListIdentityPools action.
--
-- /See:/ 'mkListIdentityPoolsResponse' smart constructor.
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
  { identityPools ::
      Lude.Maybe
        [IdentityPoolShortDescription],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListIdentityPoolsResponse' with the minimum fields required to make a request.
--
-- * 'identityPools' - The identity pools returned by the ListIdentityPools action.
-- * 'nextToken' - A pagination token.
-- * 'responseStatus' - The response status code.
mkListIdentityPoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIdentityPoolsResponse
mkListIdentityPoolsResponse pResponseStatus_ =
  ListIdentityPoolsResponse'
    { identityPools = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identity pools returned by the ListIdentityPools action.
--
-- /Note:/ Consider using 'identityPools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsIdentityPools :: Lens.Lens' ListIdentityPoolsResponse (Lude.Maybe [IdentityPoolShortDescription])
liprsIdentityPools = Lens.lens (identityPools :: ListIdentityPoolsResponse -> Lude.Maybe [IdentityPoolShortDescription]) (\s a -> s {identityPools = a} :: ListIdentityPoolsResponse)
{-# DEPRECATED liprsIdentityPools "Use generic-lens or generic-optics with 'identityPools' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsNextToken :: Lens.Lens' ListIdentityPoolsResponse (Lude.Maybe Lude.Text)
liprsNextToken = Lens.lens (nextToken :: ListIdentityPoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentityPoolsResponse)
{-# DEPRECATED liprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsResponseStatus :: Lens.Lens' ListIdentityPoolsResponse Lude.Int
liprsResponseStatus = Lens.lens (responseStatus :: ListIdentityPoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIdentityPoolsResponse)
{-# DEPRECATED liprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
