{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserPools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user pools associated with an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUserPools
  ( -- * Creating a request
    ListUserPools (..),
    mkListUserPools,

    -- ** Request lenses
    lupNextToken,
    lupMaxResults,

    -- * Destructuring the response
    ListUserPoolsResponse (..),
    mkListUserPoolsResponse,

    -- ** Response lenses
    luprsUserPools,
    luprsNextToken,
    luprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list user pools.
--
-- /See:/ 'mkListUserPools' smart constructor.
data ListUserPools = ListUserPools'
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

-- | Creates a value of 'ListUserPools' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results you want the request to return when listing the user pools.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListUserPools ::
  -- | 'maxResults'
  Lude.Natural ->
  ListUserPools
mkListUserPools pMaxResults_ =
  ListUserPools'
    { nextToken = Lude.Nothing,
      maxResults = pMaxResults_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUserPools (Lude.Maybe Lude.Text)
lupNextToken = Lens.lens (nextToken :: ListUserPools -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserPools)
{-# DEPRECATED lupNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return when listing the user pools.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMaxResults :: Lens.Lens' ListUserPools Lude.Natural
lupMaxResults = Lens.lens (maxResults :: ListUserPools -> Lude.Natural) (\s a -> s {maxResults = a} :: ListUserPools)
{-# DEPRECATED lupMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListUserPools where
  page rq rs
    | Page.stop (rs Lens.^. luprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. luprsUserPools) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lupNextToken Lens..~ rs Lens.^. luprsNextToken

instance Lude.AWSRequest ListUserPools where
  type Rs ListUserPools = ListUserPoolsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUserPoolsResponse'
            Lude.<$> (x Lude..?> "UserPools" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUserPools where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListUserPools" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUserPools where
  toJSON ListUserPools' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("MaxResults" Lude..= maxResults)
          ]
      )

instance Lude.ToPath ListUserPools where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserPools where
  toQuery = Lude.const Lude.mempty

-- | Represents the response to list user pools.
--
-- /See:/ 'mkListUserPoolsResponse' smart constructor.
data ListUserPoolsResponse = ListUserPoolsResponse'
  { userPools ::
      Lude.Maybe [UserPoolDescriptionType],
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

-- | Creates a value of 'ListUserPoolsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'userPools' - The user pools from the response to list users.
mkListUserPoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserPoolsResponse
mkListUserPoolsResponse pResponseStatus_ =
  ListUserPoolsResponse'
    { userPools = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user pools from the response to list users.
--
-- /Note:/ Consider using 'userPools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsUserPools :: Lens.Lens' ListUserPoolsResponse (Lude.Maybe [UserPoolDescriptionType])
luprsUserPools = Lens.lens (userPools :: ListUserPoolsResponse -> Lude.Maybe [UserPoolDescriptionType]) (\s a -> s {userPools = a} :: ListUserPoolsResponse)
{-# DEPRECATED luprsUserPools "Use generic-lens or generic-optics with 'userPools' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsNextToken :: Lens.Lens' ListUserPoolsResponse (Lude.Maybe Lude.Text)
luprsNextToken = Lens.lens (nextToken :: ListUserPoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserPoolsResponse)
{-# DEPRECATED luprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsResponseStatus :: Lens.Lens' ListUserPoolsResponse Lude.Int
luprsResponseStatus = Lens.lens (responseStatus :: ListUserPoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserPoolsResponse)
{-# DEPRECATED luprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
