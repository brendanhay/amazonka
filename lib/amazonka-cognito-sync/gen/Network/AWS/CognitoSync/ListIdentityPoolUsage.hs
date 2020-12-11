{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of identity pools registered with Cognito.
--
-- ListIdentityPoolUsage can only be called with developer credentials. You cannot make this API call with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.ListIdentityPoolUsage
  ( -- * Creating a request
    ListIdentityPoolUsage (..),
    mkListIdentityPoolUsage,

    -- ** Request lenses
    lipuNextToken,
    lipuMaxResults,

    -- * Destructuring the response
    ListIdentityPoolUsageResponse (..),
    mkListIdentityPoolUsageResponse,

    -- ** Response lenses
    lipursIdentityPoolUsages,
    lipursCount,
    lipursNextToken,
    lipursMaxResults,
    lipursResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request for usage information on an identity pool.
--
-- /See:/ 'mkListIdentityPoolUsage' smart constructor.
data ListIdentityPoolUsage = ListIdentityPoolUsage'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIdentityPoolUsage' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned.
-- * 'nextToken' - A pagination token for obtaining the next page of results.
mkListIdentityPoolUsage ::
  ListIdentityPoolUsage
mkListIdentityPoolUsage =
  ListIdentityPoolUsage'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipuNextToken :: Lens.Lens' ListIdentityPoolUsage (Lude.Maybe Lude.Text)
lipuNextToken = Lens.lens (nextToken :: ListIdentityPoolUsage -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentityPoolUsage)
{-# DEPRECATED lipuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipuMaxResults :: Lens.Lens' ListIdentityPoolUsage (Lude.Maybe Lude.Int)
lipuMaxResults = Lens.lens (maxResults :: ListIdentityPoolUsage -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListIdentityPoolUsage)
{-# DEPRECATED lipuMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListIdentityPoolUsage where
  type Rs ListIdentityPoolUsage = ListIdentityPoolUsageResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIdentityPoolUsageResponse'
            Lude.<$> (x Lude..?> "IdentityPoolUsages" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Count")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "MaxResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIdentityPoolUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListIdentityPoolUsage where
  toPath = Lude.const "/identitypools"

instance Lude.ToQuery ListIdentityPoolUsage where
  toQuery ListIdentityPoolUsage' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Returned for a successful ListIdentityPoolUsage request.
--
-- /See:/ 'mkListIdentityPoolUsageResponse' smart constructor.
data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse'
  { identityPoolUsages ::
      Lude.Maybe [IdentityPoolUsage],
    count :: Lude.Maybe Lude.Int,
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ListIdentityPoolUsageResponse' with the minimum fields required to make a request.
--
-- * 'count' - Total number of identities for the identity pool.
-- * 'identityPoolUsages' - Usage information for the identity pools.
-- * 'maxResults' - The maximum number of results to be returned.
-- * 'nextToken' - A pagination token for obtaining the next page of results.
-- * 'responseStatus' - The response status code.
mkListIdentityPoolUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIdentityPoolUsageResponse
mkListIdentityPoolUsageResponse pResponseStatus_ =
  ListIdentityPoolUsageResponse'
    { identityPoolUsages = Lude.Nothing,
      count = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Usage information for the identity pools.
--
-- /Note:/ Consider using 'identityPoolUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipursIdentityPoolUsages :: Lens.Lens' ListIdentityPoolUsageResponse (Lude.Maybe [IdentityPoolUsage])
lipursIdentityPoolUsages = Lens.lens (identityPoolUsages :: ListIdentityPoolUsageResponse -> Lude.Maybe [IdentityPoolUsage]) (\s a -> s {identityPoolUsages = a} :: ListIdentityPoolUsageResponse)
{-# DEPRECATED lipursIdentityPoolUsages "Use generic-lens or generic-optics with 'identityPoolUsages' instead." #-}

-- | Total number of identities for the identity pool.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipursCount :: Lens.Lens' ListIdentityPoolUsageResponse (Lude.Maybe Lude.Int)
lipursCount = Lens.lens (count :: ListIdentityPoolUsageResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: ListIdentityPoolUsageResponse)
{-# DEPRECATED lipursCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | A pagination token for obtaining the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipursNextToken :: Lens.Lens' ListIdentityPoolUsageResponse (Lude.Maybe Lude.Text)
lipursNextToken = Lens.lens (nextToken :: ListIdentityPoolUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIdentityPoolUsageResponse)
{-# DEPRECATED lipursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipursMaxResults :: Lens.Lens' ListIdentityPoolUsageResponse (Lude.Maybe Lude.Int)
lipursMaxResults = Lens.lens (maxResults :: ListIdentityPoolUsageResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListIdentityPoolUsageResponse)
{-# DEPRECATED lipursMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipursResponseStatus :: Lens.Lens' ListIdentityPoolUsageResponse Lude.Int
lipursResponseStatus = Lens.lens (responseStatus :: ListIdentityPoolUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIdentityPoolUsageResponse)
{-# DEPRECATED lipursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
