{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your things. Use the __attributeName__ and __attributeValue__ parameters to filter your things. For example, calling @ListThings@ with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute __Color__ with the value __Red__ .
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThings
  ( -- * Creating a request
    ListThings (..),
    mkListThings,

    -- ** Request lenses
    ltAttributeValue,
    ltThingTypeName,
    ltNextToken,
    ltAttributeName,
    ltMaxResults,

    -- * Destructuring the response
    ListThingsResponse (..),
    mkListThingsResponse,

    -- ** Response lenses
    ltrsNextToken,
    ltrsThings,
    ltrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListThings operation.
--
-- /See:/ 'mkListThings' smart constructor.
data ListThings = ListThings'
  { attributeValue ::
      Lude.Maybe Lude.Text,
    thingTypeName :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    attributeName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThings' with the minimum fields required to make a request.
--
-- * 'attributeName' - The attribute name used to search for things.
-- * 'attributeValue' - The attribute value used to search for things.
-- * 'maxResults' - The maximum number of results to return in this operation.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'thingTypeName' - The name of the thing type used to search for things.
mkListThings ::
  ListThings
mkListThings =
  ListThings'
    { attributeValue = Lude.Nothing,
      thingTypeName = Lude.Nothing,
      nextToken = Lude.Nothing,
      attributeName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The attribute value used to search for things.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltAttributeValue :: Lens.Lens' ListThings (Lude.Maybe Lude.Text)
ltAttributeValue = Lens.lens (attributeValue :: ListThings -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: ListThings)
{-# DEPRECATED ltAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The name of the thing type used to search for things.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltThingTypeName :: Lens.Lens' ListThings (Lude.Maybe Lude.Text)
ltThingTypeName = Lens.lens (thingTypeName :: ListThings -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: ListThings)
{-# DEPRECATED ltThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListThings (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListThings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThings)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The attribute name used to search for things.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltAttributeName :: Lens.Lens' ListThings (Lude.Maybe Lude.Text)
ltAttributeName = Lens.lens (attributeName :: ListThings -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: ListThings)
{-# DEPRECATED ltAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListThings (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListThings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThings)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListThings where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsThings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListThings where
  type Rs ListThings = ListThingsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "things" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThings where
  toPath = Lude.const "/things"

instance Lude.ToQuery ListThings where
  toQuery ListThings' {..} =
    Lude.mconcat
      [ "attributeValue" Lude.=: attributeValue,
        "thingTypeName" Lude.=: thingTypeName,
        "nextToken" Lude.=: nextToken,
        "attributeName" Lude.=: attributeName,
        "maxResults" Lude.=: maxResults
      ]

-- | The output from the ListThings operation.
--
-- /See:/ 'mkListThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    things :: Lude.Maybe [ThingAttribute],
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

-- | Creates a value of 'ListThingsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results. Will not be returned if operation has returned all results.
-- * 'responseStatus' - The response status code.
-- * 'things' - The things.
mkListThingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingsResponse
mkListThingsResponse pResponseStatus_ =
  ListThingsResponse'
    { nextToken = Lude.Nothing,
      things = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListThingsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListThingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The things.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsThings :: Lens.Lens' ListThingsResponse (Lude.Maybe [ThingAttribute])
ltrsThings = Lens.lens (things :: ListThingsResponse -> Lude.Maybe [ThingAttribute]) (\s a -> s {things = a} :: ListThingsResponse)
{-# DEPRECATED ltrsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListThingsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListThingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
