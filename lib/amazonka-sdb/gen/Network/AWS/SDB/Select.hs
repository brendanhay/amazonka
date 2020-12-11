{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Select
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Select@ operation returns a set of attributes for @ItemNames@ that match the select expression. @Select@ is similar to the standard SQL SELECT statement.
--
-- The total size of the response cannot exceed 1 MB in total size. Amazon SimpleDB automatically adjusts the number of items returned per page to enforce this limit. For example, if the client asks to retrieve 2500 items, but each individual item is 10 kB in size, the system returns 100 items and an appropriate @NextToken@ so the client can access the next page of results.
-- For information on how to construct select expressions, see Using Select to Create Amazon SimpleDB Queries in the Developer Guide.
--
-- This operation returns paginated results.
module Network.AWS.SDB.Select
  ( -- * Creating a request
    Select (..),
    mkSelect,

    -- ** Request lenses
    sConsistentRead,
    sNextToken,
    sSelectExpression,

    -- * Destructuring the response
    SelectResponse (..),
    mkSelectResponse,

    -- ** Response lenses
    srsItems,
    srsNextToken,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkSelect' smart constructor.
data Select = Select'
  { consistentRead :: Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    selectExpression :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Select' with the minimum fields required to make a request.
--
-- * 'consistentRead' - @true@
-- * 'nextToken' - @ItemNames@
-- * 'selectExpression' - The expression used to query the domain.
mkSelect ::
  -- | 'selectExpression'
  Lude.Text ->
  Select
mkSelect pSelectExpression_ =
  Select'
    { consistentRead = Lude.Nothing,
      nextToken = Lude.Nothing,
      selectExpression = pSelectExpression_
    }

-- | @true@
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConsistentRead :: Lens.Lens' Select (Lude.Maybe Lude.Bool)
sConsistentRead = Lens.lens (consistentRead :: Select -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: Select)
{-# DEPRECATED sConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | @ItemNames@
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNextToken :: Lens.Lens' Select (Lude.Maybe Lude.Text)
sNextToken = Lens.lens (nextToken :: Select -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: Select)
{-# DEPRECATED sNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The expression used to query the domain.
--
-- /Note:/ Consider using 'selectExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSelectExpression :: Lens.Lens' Select Lude.Text
sSelectExpression = Lens.lens (selectExpression :: Select -> Lude.Text) (\s a -> s {selectExpression = a} :: Select)
{-# DEPRECATED sSelectExpression "Use generic-lens or generic-optics with 'selectExpression' instead." #-}

instance Page.AWSPager Select where
  page rq rs
    | Page.stop (rs Lens.^. srsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. srsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& sNextToken Lens..~ rs Lens.^. srsNextToken

instance Lude.AWSRequest Select where
  type Rs Select = SelectResponse
  request = Req.postQuery sdbService
  response =
    Res.receiveXMLWrapper
      "SelectResult"
      ( \s h x ->
          SelectResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "Item") x)
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Select where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath Select where
  toPath = Lude.const "/"

instance Lude.ToQuery Select where
  toQuery Select' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("Select" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "ConsistentRead" Lude.=: consistentRead,
        "NextToken" Lude.=: nextToken,
        "SelectExpression" Lude.=: selectExpression
      ]

-- | /See:/ 'mkSelectResponse' smart constructor.
data SelectResponse = SelectResponse'
  { items :: Lude.Maybe [Item],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SelectResponse' with the minimum fields required to make a request.
--
-- * 'items' - A list of items that match the select expression.
-- * 'nextToken' - @MaxNumberOfItems@
-- * 'responseStatus' - The response status code.
mkSelectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SelectResponse
mkSelectResponse pResponseStatus_ =
  SelectResponse'
    { items = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of items that match the select expression.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsItems :: Lens.Lens' SelectResponse (Lude.Maybe [Item])
srsItems = Lens.lens (items :: SelectResponse -> Lude.Maybe [Item]) (\s a -> s {items = a} :: SelectResponse)
{-# DEPRECATED srsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | @MaxNumberOfItems@
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsNextToken :: Lens.Lens' SelectResponse (Lude.Maybe Lude.Text)
srsNextToken = Lens.lens (nextToken :: SelectResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SelectResponse)
{-# DEPRECATED srsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SelectResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SelectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SelectResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
