{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingPrincipals
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals associated with the specified thing. A principal can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito identities or federated identities.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingPrincipals
  ( -- * Creating a request
    ListThingPrincipals (..),
    mkListThingPrincipals,

    -- ** Request lenses
    ltpNextToken,
    ltpThingName,
    ltpMaxResults,

    -- * Destructuring the response
    ListThingPrincipalsResponse (..),
    mkListThingPrincipalsResponse,

    -- ** Response lenses
    ltprsPrincipals,
    ltprsNextToken,
    ltprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListThingPrincipal operation.
--
-- /See:/ 'mkListThingPrincipals' smart constructor.
data ListThingPrincipals = ListThingPrincipals'
  { -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the thing.
    thingName :: Lude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingPrincipals' with the minimum fields required to make a request.
--
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'thingName' - The name of the thing.
-- * 'maxResults' - The maximum number of results to return in this operation.
mkListThingPrincipals ::
  -- | 'thingName'
  Lude.Text ->
  ListThingPrincipals
mkListThingPrincipals pThingName_ =
  ListThingPrincipals'
    { nextToken = Lude.Nothing,
      thingName = pThingName_,
      maxResults = Lude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpNextToken :: Lens.Lens' ListThingPrincipals (Lude.Maybe Lude.Text)
ltpNextToken = Lens.lens (nextToken :: ListThingPrincipals -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingPrincipals)
{-# DEPRECATED ltpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpThingName :: Lens.Lens' ListThingPrincipals Lude.Text
ltpThingName = Lens.lens (thingName :: ListThingPrincipals -> Lude.Text) (\s a -> s {thingName = a} :: ListThingPrincipals)
{-# DEPRECATED ltpThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpMaxResults :: Lens.Lens' ListThingPrincipals (Lude.Maybe Lude.Natural)
ltpMaxResults = Lens.lens (maxResults :: ListThingPrincipals -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingPrincipals)
{-# DEPRECATED ltpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListThingPrincipals where
  page rq rs
    | Page.stop (rs Lens.^. ltprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltprsPrincipals) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltpNextToken Lens..~ rs Lens.^. ltprsNextToken

instance Lude.AWSRequest ListThingPrincipals where
  type Rs ListThingPrincipals = ListThingPrincipalsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingPrincipalsResponse'
            Lude.<$> (x Lude..?> "principals" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingPrincipals where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingPrincipals where
  toPath ListThingPrincipals' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/principals"]

instance Lude.ToQuery ListThingPrincipals where
  toQuery ListThingPrincipals' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | The output from the ListThingPrincipals operation.
--
-- /See:/ 'mkListThingPrincipalsResponse' smart constructor.
data ListThingPrincipalsResponse = ListThingPrincipalsResponse'
  { -- | The principals associated with the thing.
    principals :: Lude.Maybe [Lude.Text],
    -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingPrincipalsResponse' with the minimum fields required to make a request.
--
-- * 'principals' - The principals associated with the thing.
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListThingPrincipalsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingPrincipalsResponse
mkListThingPrincipalsResponse pResponseStatus_ =
  ListThingPrincipalsResponse'
    { principals = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The principals associated with the thing.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsPrincipals :: Lens.Lens' ListThingPrincipalsResponse (Lude.Maybe [Lude.Text])
ltprsPrincipals = Lens.lens (principals :: ListThingPrincipalsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {principals = a} :: ListThingPrincipalsResponse)
{-# DEPRECATED ltprsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsNextToken :: Lens.Lens' ListThingPrincipalsResponse (Lude.Maybe Lude.Text)
ltprsNextToken = Lens.lens (nextToken :: ListThingPrincipalsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingPrincipalsResponse)
{-# DEPRECATED ltprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsResponseStatus :: Lens.Lens' ListThingPrincipalsResponse Lude.Int
ltprsResponseStatus = Lens.lens (responseStatus :: ListThingPrincipalsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingPrincipalsResponse)
{-# DEPRECATED ltprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
