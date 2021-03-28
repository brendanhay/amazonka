{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.SDB.BatchPutAttributes
    (
    -- * Creating a request
      BatchPutAttributes (..)
    , mkBatchPutAttributes
    -- ** Request lenses
    , bpaDomainName
    , bpaItems

    -- * Destructuring the response
    , BatchPutAttributesResponse (..)
    , mkBatchPutAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkBatchPutAttributes' smart constructor.
data BatchPutAttributes = BatchPutAttributes'
  { domainName :: Core.Text
    -- ^ The name of the domain in which the attributes are being stored.
  , items :: [Types.ReplaceableItem]
    -- ^ A list of items on which to perform the operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutAttributes' value with any optional fields omitted.
mkBatchPutAttributes
    :: Core.Text -- ^ 'domainName'
    -> BatchPutAttributes
mkBatchPutAttributes domainName
  = BatchPutAttributes'{domainName, items = Core.mempty}

-- | The name of the domain in which the attributes are being stored.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaDomainName :: Lens.Lens' BatchPutAttributes Core.Text
bpaDomainName = Lens.field @"domainName"
{-# INLINEABLE bpaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | A list of items on which to perform the operation.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaItems :: Lens.Lens' BatchPutAttributes [Types.ReplaceableItem]
bpaItems = Lens.field @"items"
{-# INLINEABLE bpaItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToQuery BatchPutAttributes where
        toQuery BatchPutAttributes{..}
          = Core.toQueryPair "Action" ("BatchPutAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryList "Item" items

instance Core.ToHeaders BatchPutAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest BatchPutAttributes where
        type Rs BatchPutAttributes = BatchPutAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull BatchPutAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchPutAttributesResponse' smart constructor.
data BatchPutAttributesResponse = BatchPutAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutAttributesResponse' value with any optional fields omitted.
mkBatchPutAttributesResponse
    :: BatchPutAttributesResponse
mkBatchPutAttributesResponse = BatchPutAttributesResponse'
