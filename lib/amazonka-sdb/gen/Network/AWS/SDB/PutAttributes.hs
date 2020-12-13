{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.PutAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The PutAttributes operation creates or replaces attributes in an item. The client may specify new attributes using a combination of the @Attribute.X.Name@ and @Attribute.X.Value@ parameters. The client specifies the first attribute by the parameters @Attribute.0.Name@ and @Attribute.0.Value@ , the second attribute by the parameters @Attribute.1.Name@ and @Attribute.1.Value@ , and so on.
--
-- Attributes are uniquely identified in an item by their name/value combination. For example, a single item can have the attributes @{ "first_name", "first_value" }@ and @{ "first_name", second_value" }@ . However, it cannot have two attribute instances where both the @Attribute.X.Name@ and @Attribute.X.Value@ are the same.
-- Optionally, the requestor can supply the @Replace@ parameter for each individual attribute. Setting this value to @true@ causes the new attribute value to replace the existing attribute value(s). For example, if an item has the attributes @{ 'a', '1' }@ , @{ 'b', '2'}@ and @{ 'b', '3' }@ and the requestor calls @PutAttributes@ using the attributes @{ 'b', '4' }@ with the @Replace@ parameter set to true, the final attributes of the item are changed to @{ 'a', '1' }@ and @{ 'b', '4' }@ , which replaces the previous values of the 'b' attribute with the new value.
-- You cannot specify an empty string as an attribute name.
-- Because Amazon SimpleDB makes multiple copies of client data and uses an eventual consistency update model, an immediate 'GetAttributes' or 'Select' operation (read) immediately after a 'PutAttributes' or 'DeleteAttributes' operation (write) might not return the updated data.
-- The following limitations are enforced for this operation:
--     * 256 total attribute name-value pairs per item
--
--     * One billion attributes per domain
--
--     * 10 GB of total user data storage per domain
module Network.AWS.SDB.PutAttributes
  ( -- * Creating a request
    PutAttributes (..),
    mkPutAttributes,

    -- ** Request lenses
    paItemName,
    paDomainName,
    paAttributes,
    paExpected,

    -- * Destructuring the response
    PutAttributesResponse (..),
    mkPutAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { -- | The name of the item.
    itemName :: Lude.Text,
    -- | The name of the domain in which to perform the operation.
    domainName :: Lude.Text,
    -- | The list of attributes.
    attributes :: [ReplaceableAttribute],
    -- | The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
    expected :: Lude.Maybe UpdateCondition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAttributes' with the minimum fields required to make a request.
--
-- * 'itemName' - The name of the item.
-- * 'domainName' - The name of the domain in which to perform the operation.
-- * 'attributes' - The list of attributes.
-- * 'expected' - The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
mkPutAttributes ::
  -- | 'itemName'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  PutAttributes
mkPutAttributes pItemName_ pDomainName_ =
  PutAttributes'
    { itemName = pItemName_,
      domainName = pDomainName_,
      attributes = Lude.mempty,
      expected = Lude.Nothing
    }

-- | The name of the item.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paItemName :: Lens.Lens' PutAttributes Lude.Text
paItemName = Lens.lens (itemName :: PutAttributes -> Lude.Text) (\s a -> s {itemName = a} :: PutAttributes)
{-# DEPRECATED paItemName "Use generic-lens or generic-optics with 'itemName' instead." #-}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDomainName :: Lens.Lens' PutAttributes Lude.Text
paDomainName = Lens.lens (domainName :: PutAttributes -> Lude.Text) (\s a -> s {domainName = a} :: PutAttributes)
{-# DEPRECATED paDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The list of attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributes :: Lens.Lens' PutAttributes [ReplaceableAttribute]
paAttributes = Lens.lens (attributes :: PutAttributes -> [ReplaceableAttribute]) (\s a -> s {attributes = a} :: PutAttributes)
{-# DEPRECATED paAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paExpected :: Lens.Lens' PutAttributes (Lude.Maybe UpdateCondition)
paExpected = Lens.lens (expected :: PutAttributes -> Lude.Maybe UpdateCondition) (\s a -> s {expected = a} :: PutAttributes)
{-# DEPRECATED paExpected "Use generic-lens or generic-optics with 'expected' instead." #-}

instance Lude.AWSRequest PutAttributes where
  type Rs PutAttributes = PutAttributesResponse
  request = Req.postQuery sdbService
  response = Res.receiveNull PutAttributesResponse'

instance Lude.ToHeaders PutAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAttributes where
  toQuery PutAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "ItemName" Lude.=: itemName,
        "DomainName" Lude.=: domainName,
        Lude.toQueryList "Attribute" attributes,
        "Expected" Lude.=: expected
      ]

-- | /See:/ 'mkPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAttributesResponse' with the minimum fields required to make a request.
mkPutAttributesResponse ::
  PutAttributesResponse
mkPutAttributesResponse = PutAttributesResponse'
