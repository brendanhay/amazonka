{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the identities in an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.ListIdentities
  ( -- * Creating a request
    ListIdentities (..),
    mkListIdentities,

    -- ** Request lenses
    liHideDisabled,
    liNextToken,
    liIdentityPoolId,
    liMaxResults,

    -- * Destructuring the response
    ListIdentitiesResponse (..),
    mkListIdentitiesResponse,

    -- ** Response lenses
    lirsIdentityPoolId,
    lirsNextToken,
    lirsIdentities,
    lirsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the ListIdentities action.
--
-- /See:/ 'mkListIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { hideDisabled ::
      Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    identityPoolId :: Lude.Text,
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

-- | Creates a value of 'ListIdentities' with the minimum fields required to make a request.
--
-- * 'hideDisabled' - An optional boolean parameter that allows you to hide disabled identities. If omitted, the ListIdentities API will include disabled identities in the response.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'maxResults' - The maximum number of identities to return.
-- * 'nextToken' - A pagination token.
mkListIdentities ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'maxResults'
  Lude.Natural ->
  ListIdentities
mkListIdentities pIdentityPoolId_ pMaxResults_ =
  ListIdentities'
    { hideDisabled = Lude.Nothing,
      nextToken = Lude.Nothing,
      identityPoolId = pIdentityPoolId_,
      maxResults = pMaxResults_
    }

-- | An optional boolean parameter that allows you to hide disabled identities. If omitted, the ListIdentities API will include disabled identities in the response.
--
-- /Note:/ Consider using 'hideDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liHideDisabled :: Lens.Lens' ListIdentities (Lude.Maybe Lude.Bool)
liHideDisabled = Lens.lens (hideDisabled :: ListIdentities -> Lude.Maybe Lude.Bool) (\s a -> s {hideDisabled = a} :: ListIdentities)
{-# DEPRECATED liHideDisabled "Use generic-lens or generic-optics with 'hideDisabled' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIdentities (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListIdentities -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentities)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liIdentityPoolId :: Lens.Lens' ListIdentities Lude.Text
liIdentityPoolId = Lens.lens (identityPoolId :: ListIdentities -> Lude.Text) (\s a -> s {identityPoolId = a} :: ListIdentities)
{-# DEPRECATED liIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The maximum number of identities to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListIdentities Lude.Natural
liMaxResults = Lens.lens (maxResults :: ListIdentities -> Lude.Natural) (\s a -> s {maxResults = a} :: ListIdentities)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListIdentities where
  type Rs ListIdentities = ListIdentitiesResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIdentitiesResponse'
            Lude.<$> (x Lude..?> "IdentityPoolId")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Identities" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIdentities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.ListIdentities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListIdentities where
  toJSON ListIdentities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HideDisabled" Lude..=) Lude.<$> hideDisabled,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            Lude.Just ("MaxResults" Lude..= maxResults)
          ]
      )

instance Lude.ToPath ListIdentities where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIdentities where
  toQuery = Lude.const Lude.mempty

-- | The response to a ListIdentities request.
--
-- /See:/ 'mkListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { identityPoolId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    identities ::
      Lude.Maybe [IdentityDescription],
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

-- | Creates a value of 'ListIdentitiesResponse' with the minimum fields required to make a request.
--
-- * 'identities' - An object containing a set of identities and associated mappings.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'nextToken' - A pagination token.
-- * 'responseStatus' - The response status code.
mkListIdentitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIdentitiesResponse
mkListIdentitiesResponse pResponseStatus_ =
  ListIdentitiesResponse'
    { identityPoolId = Lude.Nothing,
      nextToken = Lude.Nothing,
      identities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsIdentityPoolId :: Lens.Lens' ListIdentitiesResponse (Lude.Maybe Lude.Text)
lirsIdentityPoolId = Lens.lens (identityPoolId :: ListIdentitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListIdentitiesResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListIdentitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An object containing a set of identities and associated mappings.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsIdentities :: Lens.Lens' ListIdentitiesResponse (Lude.Maybe [IdentityDescription])
lirsIdentities = Lens.lens (identities :: ListIdentitiesResponse -> Lude.Maybe [IdentityDescription]) (\s a -> s {identities = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListIdentitiesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListIdentitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
