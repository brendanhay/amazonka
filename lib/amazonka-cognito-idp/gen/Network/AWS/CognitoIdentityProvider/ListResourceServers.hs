{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListResourceServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource servers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListResourceServers
  ( -- * Creating a request
    ListResourceServers (..),
    mkListResourceServers,

    -- ** Request lenses
    lrsNextToken,
    lrsMaxResults,
    lrsUserPoolId,

    -- * Destructuring the response
    ListResourceServersResponse (..),
    mkListResourceServersResponse,

    -- ** Response lenses
    lrsrsNextToken,
    lrsrsResponseStatus,
    lrsrsResourceServers,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResourceServers' smart constructor.
data ListResourceServers = ListResourceServers'
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

-- | Creates a value of 'ListResourceServers' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of resource servers to return.
-- * 'nextToken' - A pagination token.
-- * 'userPoolId' - The user pool ID for the user pool.
mkListResourceServers ::
  -- | 'userPoolId'
  Lude.Text ->
  ListResourceServers
mkListResourceServers pUserPoolId_ =
  ListResourceServers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListResourceServers (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListResourceServers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceServers)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of resource servers to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMaxResults :: Lens.Lens' ListResourceServers (Lude.Maybe Lude.Natural)
lrsMaxResults = Lens.lens (maxResults :: ListResourceServers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResourceServers)
{-# DEPRECATED lrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsUserPoolId :: Lens.Lens' ListResourceServers Lude.Text
lrsUserPoolId = Lens.lens (userPoolId :: ListResourceServers -> Lude.Text) (\s a -> s {userPoolId = a} :: ListResourceServers)
{-# DEPRECATED lrsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Page.AWSPager ListResourceServers where
  page rq rs
    | Page.stop (rs Lens.^. lrsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsrsResourceServers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrsNextToken Lens..~ rs Lens.^. lrsrsNextToken

instance Lude.AWSRequest ListResourceServers where
  type Rs ListResourceServers = ListResourceServersResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceServersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ResourceServers" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListResourceServers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListResourceServers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourceServers where
  toJSON ListResourceServers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath ListResourceServers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourceServers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourceServersResponse' smart constructor.
data ListResourceServersResponse = ListResourceServersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    resourceServers ::
      [ResourceServerType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceServersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token.
-- * 'resourceServers' - The resource servers.
-- * 'responseStatus' - The response status code.
mkListResourceServersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceServersResponse
mkListResourceServersResponse pResponseStatus_ =
  ListResourceServersResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      resourceServers = Lude.mempty
    }

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrsNextToken :: Lens.Lens' ListResourceServersResponse (Lude.Maybe Lude.Text)
lrsrsNextToken = Lens.lens (nextToken :: ListResourceServersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceServersResponse)
{-# DEPRECATED lrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrsResponseStatus :: Lens.Lens' ListResourceServersResponse Lude.Int
lrsrsResponseStatus = Lens.lens (responseStatus :: ListResourceServersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceServersResponse)
{-# DEPRECATED lrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The resource servers.
--
-- /Note:/ Consider using 'resourceServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrsResourceServers :: Lens.Lens' ListResourceServersResponse [ResourceServerType]
lrsrsResourceServers = Lens.lens (resourceServers :: ListResourceServersResponse -> [ResourceServerType]) (\s a -> s {resourceServers = a} :: ListResourceServersResponse)
{-# DEPRECATED lrsrsResourceServers "Use generic-lens or generic-optics with 'resourceServers' instead." #-}
