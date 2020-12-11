{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListSecurityKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all security keys associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityKeys
  ( -- * Creating a request
    ListSecurityKeys (..),
    mkListSecurityKeys,

    -- ** Request lenses
    lskNextToken,
    lskMaxResults,
    lskInstanceId,

    -- * Destructuring the response
    ListSecurityKeysResponse (..),
    mkListSecurityKeysResponse,

    -- ** Response lenses
    lskrsNextToken,
    lskrsSecurityKeys,
    lskrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSecurityKeys' smart constructor.
data ListSecurityKeys = ListSecurityKeys'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecurityKeys' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListSecurityKeys ::
  -- | 'instanceId'
  Lude.Text ->
  ListSecurityKeys
mkListSecurityKeys pInstanceId_ =
  ListSecurityKeys'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskNextToken :: Lens.Lens' ListSecurityKeys (Lude.Maybe Lude.Text)
lskNextToken = Lens.lens (nextToken :: ListSecurityKeys -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityKeys)
{-# DEPRECATED lskNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskMaxResults :: Lens.Lens' ListSecurityKeys (Lude.Maybe Lude.Natural)
lskMaxResults = Lens.lens (maxResults :: ListSecurityKeys -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSecurityKeys)
{-# DEPRECATED lskMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskInstanceId :: Lens.Lens' ListSecurityKeys Lude.Text
lskInstanceId = Lens.lens (instanceId :: ListSecurityKeys -> Lude.Text) (\s a -> s {instanceId = a} :: ListSecurityKeys)
{-# DEPRECATED lskInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListSecurityKeys where
  page rq rs
    | Page.stop (rs Lens.^. lskrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lskrsSecurityKeys) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lskNextToken Lens..~ rs Lens.^. lskrsNextToken

instance Lude.AWSRequest ListSecurityKeys where
  type Rs ListSecurityKeys = ListSecurityKeysResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecurityKeysResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SecurityKeys" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecurityKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListSecurityKeys where
  toPath ListSecurityKeys' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/security-keys"]

instance Lude.ToQuery ListSecurityKeys where
  toQuery ListSecurityKeys' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListSecurityKeysResponse' smart constructor.
data ListSecurityKeysResponse = ListSecurityKeysResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    securityKeys :: Lude.Maybe [SecurityKey],
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

-- | Creates a value of 'ListSecurityKeysResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'securityKeys' - The security keys.
mkListSecurityKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecurityKeysResponse
mkListSecurityKeysResponse pResponseStatus_ =
  ListSecurityKeysResponse'
    { nextToken = Lude.Nothing,
      securityKeys = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrsNextToken :: Lens.Lens' ListSecurityKeysResponse (Lude.Maybe Lude.Text)
lskrsNextToken = Lens.lens (nextToken :: ListSecurityKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecurityKeysResponse)
{-# DEPRECATED lskrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The security keys.
--
-- /Note:/ Consider using 'securityKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrsSecurityKeys :: Lens.Lens' ListSecurityKeysResponse (Lude.Maybe [SecurityKey])
lskrsSecurityKeys = Lens.lens (securityKeys :: ListSecurityKeysResponse -> Lude.Maybe [SecurityKey]) (\s a -> s {securityKeys = a} :: ListSecurityKeysResponse)
{-# DEPRECATED lskrsSecurityKeys "Use generic-lens or generic-optics with 'securityKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lskrsResponseStatus :: Lens.Lens' ListSecurityKeysResponse Lude.Int
lskrsResponseStatus = Lens.lens (responseStatus :: ListSecurityKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecurityKeysResponse)
{-# DEPRECATED lskrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
