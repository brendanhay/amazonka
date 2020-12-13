{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInventoryDeletions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific delete inventory operation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInventoryDeletions
  ( -- * Creating a request
    DescribeInventoryDeletions (..),
    mkDescribeInventoryDeletions,

    -- ** Request lenses
    didNextToken,
    didMaxResults,
    didDeletionId,

    -- * Destructuring the response
    DescribeInventoryDeletionsResponse (..),
    mkDescribeInventoryDeletionsResponse,

    -- ** Response lenses
    didrsInventoryDeletions,
    didrsNextToken,
    didrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeInventoryDeletions' smart constructor.
data DescribeInventoryDeletions = DescribeInventoryDeletions'
  { -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
    deletionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInventoryDeletions' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'deletionId' - Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
mkDescribeInventoryDeletions ::
  DescribeInventoryDeletions
mkDescribeInventoryDeletions =
  DescribeInventoryDeletions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      deletionId = Lude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didNextToken :: Lens.Lens' DescribeInventoryDeletions (Lude.Maybe Lude.Text)
didNextToken = Lens.lens (nextToken :: DescribeInventoryDeletions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInventoryDeletions)
{-# DEPRECATED didNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didMaxResults :: Lens.Lens' DescribeInventoryDeletions (Lude.Maybe Lude.Natural)
didMaxResults = Lens.lens (maxResults :: DescribeInventoryDeletions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInventoryDeletions)
{-# DEPRECATED didMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
--
-- /Note:/ Consider using 'deletionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didDeletionId :: Lens.Lens' DescribeInventoryDeletions (Lude.Maybe Lude.Text)
didDeletionId = Lens.lens (deletionId :: DescribeInventoryDeletions -> Lude.Maybe Lude.Text) (\s a -> s {deletionId = a} :: DescribeInventoryDeletions)
{-# DEPRECATED didDeletionId "Use generic-lens or generic-optics with 'deletionId' instead." #-}

instance Page.AWSPager DescribeInventoryDeletions where
  page rq rs
    | Page.stop (rs Lens.^. didrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. didrsInventoryDeletions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& didNextToken Lens..~ rs Lens.^. didrsNextToken

instance Lude.AWSRequest DescribeInventoryDeletions where
  type
    Rs DescribeInventoryDeletions =
      DescribeInventoryDeletionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInventoryDeletionsResponse'
            Lude.<$> (x Lude..?> "InventoryDeletions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInventoryDeletions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeInventoryDeletions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInventoryDeletions where
  toJSON DescribeInventoryDeletions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("DeletionId" Lude..=) Lude.<$> deletionId
          ]
      )

instance Lude.ToPath DescribeInventoryDeletions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInventoryDeletions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInventoryDeletionsResponse' smart constructor.
data DescribeInventoryDeletionsResponse = DescribeInventoryDeletionsResponse'
  { -- | A list of status items for deleted inventory.
    inventoryDeletions :: Lude.Maybe [InventoryDeletionStatusItem],
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInventoryDeletionsResponse' with the minimum fields required to make a request.
--
-- * 'inventoryDeletions' - A list of status items for deleted inventory.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeInventoryDeletionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInventoryDeletionsResponse
mkDescribeInventoryDeletionsResponse pResponseStatus_ =
  DescribeInventoryDeletionsResponse'
    { inventoryDeletions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of status items for deleted inventory.
--
-- /Note:/ Consider using 'inventoryDeletions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsInventoryDeletions :: Lens.Lens' DescribeInventoryDeletionsResponse (Lude.Maybe [InventoryDeletionStatusItem])
didrsInventoryDeletions = Lens.lens (inventoryDeletions :: DescribeInventoryDeletionsResponse -> Lude.Maybe [InventoryDeletionStatusItem]) (\s a -> s {inventoryDeletions = a} :: DescribeInventoryDeletionsResponse)
{-# DEPRECATED didrsInventoryDeletions "Use generic-lens or generic-optics with 'inventoryDeletions' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsNextToken :: Lens.Lens' DescribeInventoryDeletionsResponse (Lude.Maybe Lude.Text)
didrsNextToken = Lens.lens (nextToken :: DescribeInventoryDeletionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInventoryDeletionsResponse)
{-# DEPRECATED didrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didrsResponseStatus :: Lens.Lens' DescribeInventoryDeletionsResponse Lude.Int
didrsResponseStatus = Lens.lens (responseStatus :: DescribeInventoryDeletionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInventoryDeletionsResponse)
{-# DEPRECATED didrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
