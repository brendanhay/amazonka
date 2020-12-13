{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.ListAssociatedStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the stack with which the specified fleet is associated.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.ListAssociatedStacks
  ( -- * Creating a request
    ListAssociatedStacks (..),
    mkListAssociatedStacks,

    -- ** Request lenses
    lasNextToken,
    lasFleetName,

    -- * Destructuring the response
    ListAssociatedStacksResponse (..),
    mkListAssociatedStacksResponse,

    -- ** Response lenses
    lasrsNextToken,
    lasrsNames,
    lasrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssociatedStacks' smart constructor.
data ListAssociatedStacks = ListAssociatedStacks'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the fleet.
    fleetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssociatedStacks' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'fleetName' - The name of the fleet.
mkListAssociatedStacks ::
  -- | 'fleetName'
  Lude.Text ->
  ListAssociatedStacks
mkListAssociatedStacks pFleetName_ =
  ListAssociatedStacks'
    { nextToken = Lude.Nothing,
      fleetName = pFleetName_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListAssociatedStacks (Lude.Maybe Lude.Text)
lasNextToken = Lens.lens (nextToken :: ListAssociatedStacks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociatedStacks)
{-# DEPRECATED lasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasFleetName :: Lens.Lens' ListAssociatedStacks Lude.Text
lasFleetName = Lens.lens (fleetName :: ListAssociatedStacks -> Lude.Text) (\s a -> s {fleetName = a} :: ListAssociatedStacks)
{-# DEPRECATED lasFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

instance Page.AWSPager ListAssociatedStacks where
  page rq rs
    | Page.stop (rs Lens.^. lasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasrsNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lasNextToken Lens..~ rs Lens.^. lasrsNextToken

instance Lude.AWSRequest ListAssociatedStacks where
  type Rs ListAssociatedStacks = ListAssociatedStacksResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssociatedStacksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Names" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssociatedStacks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.ListAssociatedStacks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssociatedStacks where
  toJSON ListAssociatedStacks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("FleetName" Lude..= fleetName)
          ]
      )

instance Lude.ToPath ListAssociatedStacks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssociatedStacks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssociatedStacksResponse' smart constructor.
data ListAssociatedStacksResponse = ListAssociatedStacksResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the stack.
    names :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssociatedStacksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'names' - The name of the stack.
-- * 'responseStatus' - The response status code.
mkListAssociatedStacksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssociatedStacksResponse
mkListAssociatedStacksResponse pResponseStatus_ =
  ListAssociatedStacksResponse'
    { nextToken = Lude.Nothing,
      names = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsNextToken :: Lens.Lens' ListAssociatedStacksResponse (Lude.Maybe Lude.Text)
lasrsNextToken = Lens.lens (nextToken :: ListAssociatedStacksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociatedStacksResponse)
{-# DEPRECATED lasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsNames :: Lens.Lens' ListAssociatedStacksResponse (Lude.Maybe [Lude.Text])
lasrsNames = Lens.lens (names :: ListAssociatedStacksResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: ListAssociatedStacksResponse)
{-# DEPRECATED lasrsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsResponseStatus :: Lens.Lens' ListAssociatedStacksResponse Lude.Int
lasrsResponseStatus = Lens.lens (responseStatus :: ListAssociatedStacksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssociatedStacksResponse)
{-# DEPRECATED lasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
