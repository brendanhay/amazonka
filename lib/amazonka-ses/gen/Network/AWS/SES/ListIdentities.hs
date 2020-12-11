{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list containing all of the identities (email addresses and domains) for your AWS account in the current AWS Region, regardless of verification status.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListIdentities
  ( -- * Creating a request
    ListIdentities (..),
    mkListIdentities,

    -- ** Request lenses
    liIdentityType,
    liNextToken,
    liMaxItems,

    -- * Destructuring the response
    ListIdentitiesResponse (..),
    mkListIdentitiesResponse,

    -- ** Response lenses
    lirsNextToken,
    lirsResponseStatus,
    lirsIdentities,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return a list of all identities (email addresses and domains) that you have attempted to verify under your AWS account, regardless of verification status.
--
-- /See:/ 'mkListIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { identityType ::
      Lude.Maybe IdentityType,
    nextToken :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Int
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
-- * 'identityType' - The type of the identities to list. Possible values are "EmailAddress" and "Domain". If this parameter is omitted, then all identities will be listed.
-- * 'maxItems' - The maximum number of identities per page. Possible values are 1-1000 inclusive.
-- * 'nextToken' - The token to use for pagination.
mkListIdentities ::
  ListIdentities
mkListIdentities =
  ListIdentities'
    { identityType = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The type of the identities to list. Possible values are "EmailAddress" and "Domain". If this parameter is omitted, then all identities will be listed.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liIdentityType :: Lens.Lens' ListIdentities (Lude.Maybe IdentityType)
liIdentityType = Lens.lens (identityType :: ListIdentities -> Lude.Maybe IdentityType) (\s a -> s {identityType = a} :: ListIdentities)
{-# DEPRECATED liIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The token to use for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIdentities (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListIdentities -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentities)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of identities per page. Possible values are 1-1000 inclusive.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxItems :: Lens.Lens' ListIdentities (Lude.Maybe Lude.Int)
liMaxItems = Lens.lens (maxItems :: ListIdentities -> Lude.Maybe Lude.Int) (\s a -> s {maxItems = a} :: ListIdentities)
{-# DEPRECATED liMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListIdentities where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsIdentities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListIdentities where
  type Rs ListIdentities = ListIdentitiesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListIdentitiesResult"
      ( \s h x ->
          ListIdentitiesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "Identities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListIdentities where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListIdentities where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIdentities where
  toQuery ListIdentities' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListIdentities" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "IdentityType" Lude.=: identityType,
        "NextToken" Lude.=: nextToken,
        "MaxItems" Lude.=: maxItems
      ]

-- | A list of all identities that you have attempted to verify under your AWS account, regardless of verification status.
--
-- /See:/ 'mkListIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    identities :: [Lude.Text]
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
-- * 'identities' - A list of identities.
-- * 'nextToken' - The token used for pagination.
-- * 'responseStatus' - The response status code.
mkListIdentitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIdentitiesResponse
mkListIdentitiesResponse pResponseStatus_ =
  ListIdentitiesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      identities = Lude.mempty
    }

-- | The token used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListIdentitiesResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListIdentitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListIdentitiesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListIdentitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of identities.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsIdentities :: Lens.Lens' ListIdentitiesResponse [Lude.Text]
lirsIdentities = Lens.lens (identities :: ListIdentitiesResponse -> [Lude.Text]) (\s a -> s {identities = a} :: ListIdentitiesResponse)
{-# DEPRECATED lirsIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}
