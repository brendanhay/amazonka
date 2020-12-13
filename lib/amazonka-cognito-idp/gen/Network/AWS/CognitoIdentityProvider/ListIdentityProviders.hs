{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListIdentityProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about all identity providers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListIdentityProviders
  ( -- * Creating a request
    ListIdentityProviders (..),
    mkListIdentityProviders,

    -- ** Request lenses
    lipUserPoolId,
    lipNextToken,
    lipMaxResults,

    -- * Destructuring the response
    ListIdentityProvidersResponse (..),
    mkListIdentityProvidersResponse,

    -- ** Response lenses
    liprsNextToken,
    liprsProviders,
    liprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | A pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of identity providers to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIdentityProviders' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
-- * 'nextToken' - A pagination token.
-- * 'maxResults' - The maximum number of identity providers to return.
mkListIdentityProviders ::
  -- | 'userPoolId'
  Lude.Text ->
  ListIdentityProviders
mkListIdentityProviders pUserPoolId_ =
  ListIdentityProviders'
    { userPoolId = pUserPoolId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipUserPoolId :: Lens.Lens' ListIdentityProviders Lude.Text
lipUserPoolId = Lens.lens (userPoolId :: ListIdentityProviders -> Lude.Text) (\s a -> s {userPoolId = a} :: ListIdentityProviders)
{-# DEPRECATED lipUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipNextToken :: Lens.Lens' ListIdentityProviders (Lude.Maybe Lude.Text)
lipNextToken = Lens.lens (nextToken :: ListIdentityProviders -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentityProviders)
{-# DEPRECATED lipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of identity providers to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxResults :: Lens.Lens' ListIdentityProviders (Lude.Maybe Lude.Natural)
lipMaxResults = Lens.lens (maxResults :: ListIdentityProviders -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListIdentityProviders)
{-# DEPRECATED lipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListIdentityProviders where
  page rq rs
    | Page.stop (rs Lens.^. liprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. liprsProviders) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lipNextToken Lens..~ rs Lens.^. liprsNextToken

instance Lude.AWSRequest ListIdentityProviders where
  type Rs ListIdentityProviders = ListIdentityProvidersResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIdentityProvidersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Providers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIdentityProviders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ListIdentityProviders" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListIdentityProviders where
  toJSON ListIdentityProviders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListIdentityProviders where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIdentityProviders where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { -- | A pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of identity provider objects.
    providers :: [ProviderDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIdentityProvidersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token.
-- * 'providers' - A list of identity provider objects.
-- * 'responseStatus' - The response status code.
mkListIdentityProvidersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIdentityProvidersResponse
mkListIdentityProvidersResponse pResponseStatus_ =
  ListIdentityProvidersResponse'
    { nextToken = Lude.Nothing,
      providers = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsNextToken :: Lens.Lens' ListIdentityProvidersResponse (Lude.Maybe Lude.Text)
liprsNextToken = Lens.lens (nextToken :: ListIdentityProvidersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentityProvidersResponse)
{-# DEPRECATED liprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of identity provider objects.
--
-- /Note:/ Consider using 'providers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsProviders :: Lens.Lens' ListIdentityProvidersResponse [ProviderDescription]
liprsProviders = Lens.lens (providers :: ListIdentityProvidersResponse -> [ProviderDescription]) (\s a -> s {providers = a} :: ListIdentityProvidersResponse)
{-# DEPRECATED liprsProviders "Use generic-lens or generic-optics with 'providers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsResponseStatus :: Lens.Lens' ListIdentityProvidersResponse Lude.Int
liprsResponseStatus = Lens.lens (responseStatus :: ListIdentityProvidersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIdentityProvidersResponse)
{-# DEPRECATED liprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
