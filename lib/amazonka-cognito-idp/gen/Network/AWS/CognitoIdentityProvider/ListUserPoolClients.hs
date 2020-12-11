{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserPoolClients
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the clients that have been created for the specified user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUserPoolClients
  ( -- * Creating a request
    ListUserPoolClients (..),
    mkListUserPoolClients,

    -- ** Request lenses
    lupcNextToken,
    lupcMaxResults,
    lupcUserPoolId,

    -- * Destructuring the response
    ListUserPoolClientsResponse (..),
    mkListUserPoolClientsResponse,

    -- ** Response lenses
    lupcrsNextToken,
    lupcrsUserPoolClients,
    lupcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to list the user pool clients.
--
-- /See:/ 'mkListUserPoolClients' smart constructor.
data ListUserPoolClients = ListUserPoolClients'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserPoolClients' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results you want the request to return when listing the user pool clients.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'userPoolId' - The user pool ID for the user pool where you want to list user pool clients.
mkListUserPoolClients ::
  -- | 'userPoolId'
  Lude.Text ->
  ListUserPoolClients
mkListUserPoolClients pUserPoolId_ =
  ListUserPoolClients'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcNextToken :: Lens.Lens' ListUserPoolClients (Lude.Maybe Lude.Text)
lupcNextToken = Lens.lens (nextToken :: ListUserPoolClients -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserPoolClients)
{-# DEPRECATED lupcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return when listing the user pool clients.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcMaxResults :: Lens.Lens' ListUserPoolClients (Lude.Maybe Lude.Natural)
lupcMaxResults = Lens.lens (maxResults :: ListUserPoolClients -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListUserPoolClients)
{-# DEPRECATED lupcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The user pool ID for the user pool where you want to list user pool clients.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcUserPoolId :: Lens.Lens' ListUserPoolClients Lude.Text
lupcUserPoolId = Lens.lens (userPoolId :: ListUserPoolClients -> Lude.Text) (\s a -> s {userPoolId = a} :: ListUserPoolClients)
{-# DEPRECATED lupcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Page.AWSPager ListUserPoolClients where
  page rq rs
    | Page.stop (rs Lens.^. lupcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lupcrsUserPoolClients) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lupcNextToken Lens..~ rs Lens.^. lupcrsNextToken

instance Lude.AWSRequest ListUserPoolClients where
  type Rs ListUserPoolClients = ListUserPoolClientsResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListUserPoolClientsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "UserPoolClients" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUserPoolClients where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListUserPoolClients" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListUserPoolClients where
  toJSON ListUserPoolClients' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath ListUserPoolClients where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserPoolClients where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server that lists user pool clients.
--
-- /See:/ 'mkListUserPoolClientsResponse' smart constructor.
data ListUserPoolClientsResponse = ListUserPoolClientsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    userPoolClients ::
      Lude.Maybe
        [UserPoolClientDescription],
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserPoolClientsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'userPoolClients' - The user pool clients in the response that lists user pool clients.
mkListUserPoolClientsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserPoolClientsResponse
mkListUserPoolClientsResponse pResponseStatus_ =
  ListUserPoolClientsResponse'
    { nextToken = Lude.Nothing,
      userPoolClients = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcrsNextToken :: Lens.Lens' ListUserPoolClientsResponse (Lude.Maybe Lude.Text)
lupcrsNextToken = Lens.lens (nextToken :: ListUserPoolClientsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListUserPoolClientsResponse)
{-# DEPRECATED lupcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The user pool clients in the response that lists user pool clients.
--
-- /Note:/ Consider using 'userPoolClients' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcrsUserPoolClients :: Lens.Lens' ListUserPoolClientsResponse (Lude.Maybe [UserPoolClientDescription])
lupcrsUserPoolClients = Lens.lens (userPoolClients :: ListUserPoolClientsResponse -> Lude.Maybe [UserPoolClientDescription]) (\s a -> s {userPoolClients = a} :: ListUserPoolClientsResponse)
{-# DEPRECATED lupcrsUserPoolClients "Use generic-lens or generic-optics with 'userPoolClients' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupcrsResponseStatus :: Lens.Lens' ListUserPoolClientsResponse Lude.Int
lupcrsResponseStatus = Lens.lens (responseStatus :: ListUserPoolClientsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserPoolClientsResponse)
{-# DEPRECATED lupcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
