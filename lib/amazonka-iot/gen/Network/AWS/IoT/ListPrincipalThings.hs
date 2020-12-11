{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListPrincipalThings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things associated with the specified principal. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPrincipalThings
  ( -- * Creating a request
    ListPrincipalThings (..),
    mkListPrincipalThings,

    -- ** Request lenses
    lptNextToken,
    lptMaxResults,
    lptPrincipal,

    -- * Destructuring the response
    ListPrincipalThingsResponse (..),
    mkListPrincipalThingsResponse,

    -- ** Response lenses
    lptrsNextToken,
    lptrsThings,
    lptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListPrincipalThings operation.
--
-- /See:/ 'mkListPrincipalThings' smart constructor.
data ListPrincipalThings = ListPrincipalThings'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    principal :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPrincipalThings' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in this operation.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'principal' - The principal.
mkListPrincipalThings ::
  -- | 'principal'
  Lude.Text ->
  ListPrincipalThings
mkListPrincipalThings pPrincipal_ =
  ListPrincipalThings'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      principal = pPrincipal_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptNextToken :: Lens.Lens' ListPrincipalThings (Lude.Maybe Lude.Text)
lptNextToken = Lens.lens (nextToken :: ListPrincipalThings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPrincipalThings)
{-# DEPRECATED lptNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptMaxResults :: Lens.Lens' ListPrincipalThings (Lude.Maybe Lude.Natural)
lptMaxResults = Lens.lens (maxResults :: ListPrincipalThings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPrincipalThings)
{-# DEPRECATED lptMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The principal.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptPrincipal :: Lens.Lens' ListPrincipalThings Lude.Text
lptPrincipal = Lens.lens (principal :: ListPrincipalThings -> Lude.Text) (\s a -> s {principal = a} :: ListPrincipalThings)
{-# DEPRECATED lptPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

instance Page.AWSPager ListPrincipalThings where
  page rq rs
    | Page.stop (rs Lens.^. lptrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lptrsThings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lptNextToken Lens..~ rs Lens.^. lptrsNextToken

instance Lude.AWSRequest ListPrincipalThings where
  type Rs ListPrincipalThings = ListPrincipalThingsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPrincipalThingsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "things" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPrincipalThings where
  toHeaders ListPrincipalThings' {..} =
    Lude.mconcat ["x-amzn-principal" Lude.=# principal]

instance Lude.ToPath ListPrincipalThings where
  toPath = Lude.const "/principals/things"

instance Lude.ToQuery ListPrincipalThings where
  toQuery ListPrincipalThings' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | The output from the ListPrincipalThings operation.
--
-- /See:/ 'mkListPrincipalThingsResponse' smart constructor.
data ListPrincipalThingsResponse = ListPrincipalThingsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    things :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListPrincipalThingsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
-- * 'things' - The things.
mkListPrincipalThingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPrincipalThingsResponse
mkListPrincipalThingsResponse pResponseStatus_ =
  ListPrincipalThingsResponse'
    { nextToken = Lude.Nothing,
      things = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptrsNextToken :: Lens.Lens' ListPrincipalThingsResponse (Lude.Maybe Lude.Text)
lptrsNextToken = Lens.lens (nextToken :: ListPrincipalThingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPrincipalThingsResponse)
{-# DEPRECATED lptrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The things.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptrsThings :: Lens.Lens' ListPrincipalThingsResponse (Lude.Maybe [Lude.Text])
lptrsThings = Lens.lens (things :: ListPrincipalThingsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {things = a} :: ListPrincipalThingsResponse)
{-# DEPRECATED lptrsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptrsResponseStatus :: Lens.Lens' ListPrincipalThingsResponse Lude.Int
lptrsResponseStatus = Lens.lens (responseStatus :: ListPrincipalThingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPrincipalThingsResponse)
{-# DEPRECATED lptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
