{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.ListAssociatedFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the fleet that is associated with the specified stack.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.ListAssociatedFleets
  ( -- * Creating a request
    ListAssociatedFleets (..),
    mkListAssociatedFleets,

    -- ** Request lenses
    lafNextToken,
    lafStackName,

    -- * Destructuring the response
    ListAssociatedFleetsResponse (..),
    mkListAssociatedFleetsResponse,

    -- ** Response lenses
    lafrsNextToken,
    lafrsNames,
    lafrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAssociatedFleets' smart constructor.
data ListAssociatedFleets = ListAssociatedFleets'
  { nextToken ::
      Lude.Maybe Lude.Text,
    stackName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAssociatedFleets' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'stackName' - The name of the stack.
mkListAssociatedFleets ::
  -- | 'stackName'
  Lude.Text ->
  ListAssociatedFleets
mkListAssociatedFleets pStackName_ =
  ListAssociatedFleets'
    { nextToken = Lude.Nothing,
      stackName = pStackName_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafNextToken :: Lens.Lens' ListAssociatedFleets (Lude.Maybe Lude.Text)
lafNextToken = Lens.lens (nextToken :: ListAssociatedFleets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociatedFleets)
{-# DEPRECATED lafNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafStackName :: Lens.Lens' ListAssociatedFleets Lude.Text
lafStackName = Lens.lens (stackName :: ListAssociatedFleets -> Lude.Text) (\s a -> s {stackName = a} :: ListAssociatedFleets)
{-# DEPRECATED lafStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Page.AWSPager ListAssociatedFleets where
  page rq rs
    | Page.stop (rs Lens.^. lafrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lafrsNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lafNextToken Lens..~ rs Lens.^. lafrsNextToken

instance Lude.AWSRequest ListAssociatedFleets where
  type Rs ListAssociatedFleets = ListAssociatedFleetsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAssociatedFleetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Names" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAssociatedFleets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.ListAssociatedFleets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAssociatedFleets where
  toJSON ListAssociatedFleets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("StackName" Lude..= stackName)
          ]
      )

instance Lude.ToPath ListAssociatedFleets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAssociatedFleets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAssociatedFleetsResponse' smart constructor.
data ListAssociatedFleetsResponse = ListAssociatedFleetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    names :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListAssociatedFleetsResponse' with the minimum fields required to make a request.
--
-- * 'names' - The name of the fleet.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
mkListAssociatedFleetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAssociatedFleetsResponse
mkListAssociatedFleetsResponse pResponseStatus_ =
  ListAssociatedFleetsResponse'
    { nextToken = Lude.Nothing,
      names = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrsNextToken :: Lens.Lens' ListAssociatedFleetsResponse (Lude.Maybe Lude.Text)
lafrsNextToken = Lens.lens (nextToken :: ListAssociatedFleetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAssociatedFleetsResponse)
{-# DEPRECATED lafrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrsNames :: Lens.Lens' ListAssociatedFleetsResponse (Lude.Maybe [Lude.Text])
lafrsNames = Lens.lens (names :: ListAssociatedFleetsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: ListAssociatedFleetsResponse)
{-# DEPRECATED lafrsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrsResponseStatus :: Lens.Lens' ListAssociatedFleetsResponse Lude.Int
lafrsResponseStatus = Lens.lens (responseStatus :: ListAssociatedFleetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAssociatedFleetsResponse)
{-# DEPRECATED lafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
