{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DeleteAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more attributes associated with an item. If all attributes of the item are deleted, the item is deleted.
--
-- @DeleteAttributes@ is an idempotent operation; running it multiple times on the same item or attribute does not result in an error response.
-- Because Amazon SimpleDB makes multiple copies of item data and uses an eventual consistency update model, performing a 'GetAttributes' or 'Select' operation (read) immediately after a @DeleteAttributes@ or 'PutAttributes' operation (write) might not return updated item data.
module Network.AWS.SDB.DeleteAttributes
  ( -- * Creating a request
    DeleteAttributes (..),
    mkDeleteAttributes,

    -- ** Request lenses
    daAttributes,
    daExpected,
    daDomainName,
    daItemName,

    -- * Destructuring the response
    DeleteAttributesResponse (..),
    mkDeleteAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { attributes ::
      Lude.Maybe [Attribute],
    expected :: Lude.Maybe UpdateCondition,
    domainName :: Lude.Text,
    itemName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - A list of Attributes. Similar to columns on a spreadsheet, attributes represent categories of data that can be assigned to items.
-- * 'domainName' - The name of the domain in which to perform the operation.
-- * 'expected' - The update condition which, if specified, determines whether the specified attributes will be deleted or not. The update condition must be satisfied in order for this request to be processed and the attributes to be deleted.
-- * 'itemName' - The name of the item. Similar to rows on a spreadsheet, items represent individual objects that contain one or more value-attribute pairs.
mkDeleteAttributes ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'itemName'
  Lude.Text ->
  DeleteAttributes
mkDeleteAttributes pDomainName_ pItemName_ =
  DeleteAttributes'
    { attributes = Lude.Nothing,
      expected = Lude.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_
    }

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes represent categories of data that can be assigned to items.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttributes :: Lens.Lens' DeleteAttributes (Lude.Maybe [Attribute])
daAttributes = Lens.lens (attributes :: DeleteAttributes -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: DeleteAttributes)
{-# DEPRECATED daAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The update condition which, if specified, determines whether the specified attributes will be deleted or not. The update condition must be satisfied in order for this request to be processed and the attributes to be deleted.
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daExpected :: Lens.Lens' DeleteAttributes (Lude.Maybe UpdateCondition)
daExpected = Lens.lens (expected :: DeleteAttributes -> Lude.Maybe UpdateCondition) (\s a -> s {expected = a} :: DeleteAttributes)
{-# DEPRECATED daExpected "Use generic-lens or generic-optics with 'expected' instead." #-}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDomainName :: Lens.Lens' DeleteAttributes Lude.Text
daDomainName = Lens.lens (domainName :: DeleteAttributes -> Lude.Text) (\s a -> s {domainName = a} :: DeleteAttributes)
{-# DEPRECATED daDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the item. Similar to rows on a spreadsheet, items represent individual objects that contain one or more value-attribute pairs.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daItemName :: Lens.Lens' DeleteAttributes Lude.Text
daItemName = Lens.lens (itemName :: DeleteAttributes -> Lude.Text) (\s a -> s {itemName = a} :: DeleteAttributes)
{-# DEPRECATED daItemName "Use generic-lens or generic-optics with 'itemName' instead." #-}

instance Lude.AWSRequest DeleteAttributes where
  type Rs DeleteAttributes = DeleteAttributesResponse
  request = Req.postQuery sdbService
  response = Res.receiveNull DeleteAttributesResponse'

instance Lude.ToHeaders DeleteAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAttributes where
  toQuery DeleteAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Attribute" Lude.<$> attributes),
        "Expected" Lude.=: expected,
        "DomainName" Lude.=: domainName,
        "ItemName" Lude.=: itemName
      ]

-- | /See:/ 'mkDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAttributesResponse' with the minimum fields required to make a request.
mkDeleteAttributesResponse ::
  DeleteAttributesResponse
mkDeleteAttributesResponse = DeleteAttributesResponse'
