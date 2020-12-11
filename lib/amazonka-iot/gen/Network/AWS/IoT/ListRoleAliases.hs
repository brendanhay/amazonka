{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListRoleAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the role aliases registered in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListRoleAliases
  ( -- * Creating a request
    ListRoleAliases (..),
    mkListRoleAliases,

    -- ** Request lenses
    lraMarker,
    lraAscendingOrder,
    lraPageSize,

    -- * Destructuring the response
    ListRoleAliasesResponse (..),
    mkListRoleAliasesResponse,

    -- ** Response lenses
    lrarsRoleAliases,
    lrarsNextMarker,
    lrarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRoleAliases' smart constructor.
data ListRoleAliases = ListRoleAliases'
  { marker ::
      Lude.Maybe Lude.Text,
    ascendingOrder :: Lude.Maybe Lude.Bool,
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRoleAliases' with the minimum fields required to make a request.
--
-- * 'ascendingOrder' - Return the list of role aliases in ascending alphabetical order.
-- * 'marker' - A marker used to get the next set of results.
-- * 'pageSize' - The maximum number of results to return at one time.
mkListRoleAliases ::
  ListRoleAliases
mkListRoleAliases =
  ListRoleAliases'
    { marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraMarker :: Lens.Lens' ListRoleAliases (Lude.Maybe Lude.Text)
lraMarker = Lens.lens (marker :: ListRoleAliases -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRoleAliases)
{-# DEPRECATED lraMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Return the list of role aliases in ascending alphabetical order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraAscendingOrder :: Lens.Lens' ListRoleAliases (Lude.Maybe Lude.Bool)
lraAscendingOrder = Lens.lens (ascendingOrder :: ListRoleAliases -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListRoleAliases)
{-# DEPRECATED lraAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraPageSize :: Lens.Lens' ListRoleAliases (Lude.Maybe Lude.Natural)
lraPageSize = Lens.lens (pageSize :: ListRoleAliases -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListRoleAliases)
{-# DEPRECATED lraPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListRoleAliases where
  page rq rs
    | Page.stop (rs Lens.^. lrarsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lrarsRoleAliases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lraMarker Lens..~ rs Lens.^. lrarsNextMarker

instance Lude.AWSRequest ListRoleAliases where
  type Rs ListRoleAliases = ListRoleAliasesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRoleAliasesResponse'
            Lude.<$> (x Lude..?> "roleAliases" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRoleAliases where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListRoleAliases where
  toPath = Lude.const "/role-aliases"

instance Lude.ToQuery ListRoleAliases where
  toQuery ListRoleAliases' {..} =
    Lude.mconcat
      [ "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkListRoleAliasesResponse' smart constructor.
data ListRoleAliasesResponse = ListRoleAliasesResponse'
  { roleAliases ::
      Lude.Maybe [Lude.Text],
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRoleAliasesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - A marker used to get the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'roleAliases' - The role aliases.
mkListRoleAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRoleAliasesResponse
mkListRoleAliasesResponse pResponseStatus_ =
  ListRoleAliasesResponse'
    { roleAliases = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The role aliases.
--
-- /Note:/ Consider using 'roleAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrarsRoleAliases :: Lens.Lens' ListRoleAliasesResponse (Lude.Maybe [Lude.Text])
lrarsRoleAliases = Lens.lens (roleAliases :: ListRoleAliasesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {roleAliases = a} :: ListRoleAliasesResponse)
{-# DEPRECATED lrarsRoleAliases "Use generic-lens or generic-optics with 'roleAliases' instead." #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrarsNextMarker :: Lens.Lens' ListRoleAliasesResponse (Lude.Maybe Lude.Text)
lrarsNextMarker = Lens.lens (nextMarker :: ListRoleAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListRoleAliasesResponse)
{-# DEPRECATED lrarsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrarsResponseStatus :: Lens.Lens' ListRoleAliasesResponse Lude.Int
lrarsResponseStatus = Lens.lens (responseStatus :: ListRoleAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRoleAliasesResponse)
{-# DEPRECATED lrarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
