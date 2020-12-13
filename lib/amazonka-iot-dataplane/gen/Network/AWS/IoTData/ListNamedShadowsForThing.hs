{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.ListNamedShadowsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shadows for the specified thing.
module Network.AWS.IoTData.ListNamedShadowsForThing
  ( -- * Creating a request
    ListNamedShadowsForThing (..),
    mkListNamedShadowsForThing,

    -- ** Request lenses
    lnsftNextToken,
    lnsftThingName,
    lnsftPageSize,

    -- * Destructuring the response
    ListNamedShadowsForThingResponse (..),
    mkListNamedShadowsForThingResponse,

    -- ** Response lenses
    lnsftrsResults,
    lnsftrsNextToken,
    lnsftrsTimestamp,
    lnsftrsResponseStatus,
  )
where

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListNamedShadowsForThing' smart constructor.
data ListNamedShadowsForThing = ListNamedShadowsForThing'
  { -- | The token to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the thing.
    thingName :: Lude.Text,
    -- | The result page size.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNamedShadowsForThing' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next set of results.
-- * 'thingName' - The name of the thing.
-- * 'pageSize' - The result page size.
mkListNamedShadowsForThing ::
  -- | 'thingName'
  Lude.Text ->
  ListNamedShadowsForThing
mkListNamedShadowsForThing pThingName_ =
  ListNamedShadowsForThing'
    { nextToken = Lude.Nothing,
      thingName = pThingName_,
      pageSize = Lude.Nothing
    }

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftNextToken :: Lens.Lens' ListNamedShadowsForThing (Lude.Maybe Lude.Text)
lnsftNextToken = Lens.lens (nextToken :: ListNamedShadowsForThing -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNamedShadowsForThing)
{-# DEPRECATED lnsftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftThingName :: Lens.Lens' ListNamedShadowsForThing Lude.Text
lnsftThingName = Lens.lens (thingName :: ListNamedShadowsForThing -> Lude.Text) (\s a -> s {thingName = a} :: ListNamedShadowsForThing)
{-# DEPRECATED lnsftThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The result page size.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftPageSize :: Lens.Lens' ListNamedShadowsForThing (Lude.Maybe Lude.Natural)
lnsftPageSize = Lens.lens (pageSize :: ListNamedShadowsForThing -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListNamedShadowsForThing)
{-# DEPRECATED lnsftPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest ListNamedShadowsForThing where
  type Rs ListNamedShadowsForThing = ListNamedShadowsForThingResponse
  request = Req.get ioTDataService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListNamedShadowsForThingResponse'
            Lude.<$> (x Lude..?> "results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "timestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListNamedShadowsForThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListNamedShadowsForThing where
  toPath ListNamedShadowsForThing' {..} =
    Lude.mconcat
      [ "/api/things/shadow/ListNamedShadowsForThing/",
        Lude.toBS thingName
      ]

instance Lude.ToQuery ListNamedShadowsForThing where
  toQuery ListNamedShadowsForThing' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "pageSize" Lude.=: pageSize]

-- | /See:/ 'mkListNamedShadowsForThingResponse' smart constructor.
data ListNamedShadowsForThingResponse = ListNamedShadowsForThingResponse'
  { -- | The list of shadows for the specified thing.
    results :: Lude.Maybe [Lude.Text],
    -- | The token for the next set of results, or null if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Epoch date and time the response was generated by AWS IoT.
    timestamp :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNamedShadowsForThingResponse' with the minimum fields required to make a request.
--
-- * 'results' - The list of shadows for the specified thing.
-- * 'nextToken' - The token for the next set of results, or null if there are no additional results.
-- * 'timestamp' - The Epoch date and time the response was generated by AWS IoT.
-- * 'responseStatus' - The response status code.
mkListNamedShadowsForThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListNamedShadowsForThingResponse
mkListNamedShadowsForThingResponse pResponseStatus_ =
  ListNamedShadowsForThingResponse'
    { results = Lude.Nothing,
      nextToken = Lude.Nothing,
      timestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of shadows for the specified thing.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrsResults :: Lens.Lens' ListNamedShadowsForThingResponse (Lude.Maybe [Lude.Text])
lnsftrsResults = Lens.lens (results :: ListNamedShadowsForThingResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {results = a} :: ListNamedShadowsForThingResponse)
{-# DEPRECATED lnsftrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | The token for the next set of results, or null if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrsNextToken :: Lens.Lens' ListNamedShadowsForThingResponse (Lude.Maybe Lude.Text)
lnsftrsNextToken = Lens.lens (nextToken :: ListNamedShadowsForThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNamedShadowsForThingResponse)
{-# DEPRECATED lnsftrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Epoch date and time the response was generated by AWS IoT.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrsTimestamp :: Lens.Lens' ListNamedShadowsForThingResponse (Lude.Maybe Lude.Integer)
lnsftrsTimestamp = Lens.lens (timestamp :: ListNamedShadowsForThingResponse -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: ListNamedShadowsForThingResponse)
{-# DEPRECATED lnsftrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnsftrsResponseStatus :: Lens.Lens' ListNamedShadowsForThingResponse Lude.Int
lnsftrsResponseStatus = Lens.lens (responseStatus :: ListNamedShadowsForThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListNamedShadowsForThingResponse)
{-# DEPRECATED lnsftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
