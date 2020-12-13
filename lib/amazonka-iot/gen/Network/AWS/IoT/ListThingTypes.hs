{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing thing types.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingTypes
  ( -- * Creating a request
    ListThingTypes (..),
    mkListThingTypes,

    -- ** Request lenses
    lttThingTypeName,
    lttNextToken,
    lttMaxResults,

    -- * Destructuring the response
    ListThingTypesResponse (..),
    mkListThingTypesResponse,

    -- ** Response lenses
    lttrsThingTypes,
    lttrsNextToken,
    lttrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListThingTypes operation.
--
-- /See:/ 'mkListThingTypes' smart constructor.
data ListThingTypes = ListThingTypes'
  { -- | The name of the thing type.
    thingTypeName :: Lude.Maybe Lude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingTypes' with the minimum fields required to make a request.
--
-- * 'thingTypeName' - The name of the thing type.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'maxResults' - The maximum number of results to return in this operation.
mkListThingTypes ::
  ListThingTypes
mkListThingTypes =
  ListThingTypes'
    { thingTypeName = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttThingTypeName :: Lens.Lens' ListThingTypes (Lude.Maybe Lude.Text)
lttThingTypeName = Lens.lens (thingTypeName :: ListThingTypes -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: ListThingTypes)
{-# DEPRECATED lttThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttNextToken :: Lens.Lens' ListThingTypes (Lude.Maybe Lude.Text)
lttNextToken = Lens.lens (nextToken :: ListThingTypes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingTypes)
{-# DEPRECATED lttNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttMaxResults :: Lens.Lens' ListThingTypes (Lude.Maybe Lude.Natural)
lttMaxResults = Lens.lens (maxResults :: ListThingTypes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingTypes)
{-# DEPRECATED lttMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListThingTypes where
  page rq rs
    | Page.stop (rs Lens.^. lttrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lttrsThingTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lttNextToken Lens..~ rs Lens.^. lttrsNextToken

instance Lude.AWSRequest ListThingTypes where
  type Rs ListThingTypes = ListThingTypesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingTypesResponse'
            Lude.<$> (x Lude..?> "thingTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingTypes where
  toPath = Lude.const "/thing-types"

instance Lude.ToQuery ListThingTypes where
  toQuery ListThingTypes' {..} =
    Lude.mconcat
      [ "thingTypeName" Lude.=: thingTypeName,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | The output for the ListThingTypes operation.
--
-- /See:/ 'mkListThingTypesResponse' smart constructor.
data ListThingTypesResponse = ListThingTypesResponse'
  { -- | The thing types.
    thingTypes :: Lude.Maybe [ThingTypeDefinition],
    -- | The token for the next set of results. Will not be returned if operation has returned all results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingTypesResponse' with the minimum fields required to make a request.
--
-- * 'thingTypes' - The thing types.
-- * 'nextToken' - The token for the next set of results. Will not be returned if operation has returned all results.
-- * 'responseStatus' - The response status code.
mkListThingTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingTypesResponse
mkListThingTypesResponse pResponseStatus_ =
  ListThingTypesResponse'
    { thingTypes = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The thing types.
--
-- /Note:/ Consider using 'thingTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttrsThingTypes :: Lens.Lens' ListThingTypesResponse (Lude.Maybe [ThingTypeDefinition])
lttrsThingTypes = Lens.lens (thingTypes :: ListThingTypesResponse -> Lude.Maybe [ThingTypeDefinition]) (\s a -> s {thingTypes = a} :: ListThingTypesResponse)
{-# DEPRECATED lttrsThingTypes "Use generic-lens or generic-optics with 'thingTypes' instead." #-}

-- | The token for the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttrsNextToken :: Lens.Lens' ListThingTypesResponse (Lude.Maybe Lude.Text)
lttrsNextToken = Lens.lens (nextToken :: ListThingTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingTypesResponse)
{-# DEPRECATED lttrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttrsResponseStatus :: Lens.Lens' ListThingTypesResponse Lude.Int
lttrsResponseStatus = Lens.lens (responseStatus :: ListThingTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingTypesResponse)
{-# DEPRECATED lttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
