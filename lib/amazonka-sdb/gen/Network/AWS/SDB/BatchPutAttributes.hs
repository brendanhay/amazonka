{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.BatchPutAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchPutAttributes@ operation creates or replaces attributes within one or more items. By using this operation, the client can perform multiple 'PutAttribute' operation with a single call. This helps yield savings in round trips and latencies, enabling Amazon SimpleDB to optimize requests and generally produce better throughput.
--
-- The client may specify the item name with the @Item.X.ItemName@ parameter. The client may specify new attributes using a combination of the @Item.X.Attribute.Y.Name@ and @Item.X.Attribute.Y.Value@ parameters. The client may specify the first attribute for the first item using the parameters @Item.0.Attribute.0.Name@ and @Item.0.Attribute.0.Value@ , and for the second attribute for the first item by the parameters @Item.0.Attribute.1.Name@ and @Item.0.Attribute.1.Value@ , and so on.
-- Attributes are uniquely identified within an item by their name/value combination. For example, a single item can have the attributes @{ "first_name", "first_value" }@ and @{ "first_name", "second_value" }@ . However, it cannot have two attribute instances where both the @Item.X.Attribute.Y.Name@ and @Item.X.Attribute.Y.Value@ are the same.
-- Optionally, the requester can supply the @Replace@ parameter for each individual value. Setting this value to @true@ will cause the new attribute values to replace the existing attribute values. For example, if an item @I@ has the attributes @{ 'a', '1' }, { 'b', '2'}@ and @{ 'b', '3' }@ and the requester does a BatchPutAttributes of @{'I', 'b', '4' }@ with the Replace parameter set to true, the final attributes of the item will be @{ 'a', '1' }@ and @{ 'b', '4' }@ , replacing the previous values of the 'b' attribute with the new value.
-- /Important:/ This operation is vulnerable to exceeding the maximum URL size when making a REST request using the HTTP GET method. This operation does not support conditions using @Expected.X.Name@ , @Expected.X.Value@ , or @Expected.X.Exists@ . You can execute multiple @BatchPutAttributes@ operations and other operations in parallel. However, large numbers of concurrent @BatchPutAttributes@ calls can result in Service Unavailable (503) responses.
-- The following limitations are enforced for this operation:
--     * 256 attribute name-value pairs per item
--
--     * 1 MB request size
--
--     * 1 billion attributes per domain
--
--     * 10 GB of total user data storage per domain
--
--     * 25 item limit per @BatchPutAttributes@ operation
module Network.AWS.SDB.BatchPutAttributes
  ( -- * Creating a request
    BatchPutAttributes (..),
    mkBatchPutAttributes,

    -- ** Request lenses
    bpaItems,
    bpaDomainName,

    -- * Destructuring the response
    BatchPutAttributesResponse (..),
    mkBatchPutAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkBatchPutAttributes' smart constructor.
data BatchPutAttributes = BatchPutAttributes'
  { -- | A list of items on which to perform the operation.
    items :: [ReplaceableItem],
    -- | The name of the domain in which the attributes are being stored.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutAttributes' with the minimum fields required to make a request.
--
-- * 'items' - A list of items on which to perform the operation.
-- * 'domainName' - The name of the domain in which the attributes are being stored.
mkBatchPutAttributes ::
  -- | 'domainName'
  Lude.Text ->
  BatchPutAttributes
mkBatchPutAttributes pDomainName_ =
  BatchPutAttributes'
    { items = Lude.mempty,
      domainName = pDomainName_
    }

-- | A list of items on which to perform the operation.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaItems :: Lens.Lens' BatchPutAttributes [ReplaceableItem]
bpaItems = Lens.lens (items :: BatchPutAttributes -> [ReplaceableItem]) (\s a -> s {items = a} :: BatchPutAttributes)
{-# DEPRECATED bpaItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The name of the domain in which the attributes are being stored.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaDomainName :: Lens.Lens' BatchPutAttributes Lude.Text
bpaDomainName = Lens.lens (domainName :: BatchPutAttributes -> Lude.Text) (\s a -> s {domainName = a} :: BatchPutAttributes)
{-# DEPRECATED bpaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest BatchPutAttributes where
  type Rs BatchPutAttributes = BatchPutAttributesResponse
  request = Req.postQuery sdbService
  response = Res.receiveNull BatchPutAttributesResponse'

instance Lude.ToHeaders BatchPutAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchPutAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchPutAttributes where
  toQuery BatchPutAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BatchPutAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        Lude.toQueryList "Item" items,
        "DomainName" Lude.=: domainName
      ]

-- | /See:/ 'mkBatchPutAttributesResponse' smart constructor.
data BatchPutAttributesResponse = BatchPutAttributesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutAttributesResponse' with the minimum fields required to make a request.
mkBatchPutAttributesResponse ::
  BatchPutAttributesResponse
mkBatchPutAttributesResponse = BatchPutAttributesResponse'
