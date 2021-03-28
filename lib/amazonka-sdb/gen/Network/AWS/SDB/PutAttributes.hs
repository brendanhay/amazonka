{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.SDB.PutAttributes
    (
    -- * Creating a request
      PutAttributes (..)
    , mkPutAttributes
    -- ** Request lenses
    , paDomainName
    , paItemName
    , paAttributes
    , paExpected

    -- * Destructuring the response
    , PutAttributesResponse (..)
    , mkPutAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { domainName :: Core.Text
    -- ^ The name of the domain in which to perform the operation.
  , itemName :: Core.Text
    -- ^ The name of the item.
  , attributes :: [Types.ReplaceableAttribute]
    -- ^ The list of attributes.
  , expected :: Core.Maybe Types.UpdateCondition
    -- ^ The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAttributes' value with any optional fields omitted.
mkPutAttributes
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'itemName'
    -> PutAttributes
mkPutAttributes domainName itemName
  = PutAttributes'{domainName, itemName, attributes = Core.mempty,
                   expected = Core.Nothing}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDomainName :: Lens.Lens' PutAttributes Core.Text
paDomainName = Lens.field @"domainName"
{-# INLINEABLE paDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paItemName :: Lens.Lens' PutAttributes Core.Text
paItemName = Lens.field @"itemName"
{-# INLINEABLE paItemName #-}
{-# DEPRECATED itemName "Use generic-lens or generic-optics with 'itemName' instead"  #-}

-- | The list of attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributes :: Lens.Lens' PutAttributes [Types.ReplaceableAttribute]
paAttributes = Lens.field @"attributes"
{-# INLINEABLE paAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paExpected :: Lens.Lens' PutAttributes (Core.Maybe Types.UpdateCondition)
paExpected = Lens.field @"expected"
{-# INLINEABLE paExpected #-}
{-# DEPRECATED expected "Use generic-lens or generic-optics with 'expected' instead"  #-}

instance Core.ToQuery PutAttributes where
        toQuery PutAttributes{..}
          = Core.toQueryPair "Action" ("PutAttributes" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "ItemName" itemName
              Core.<> Core.toQueryList "Attribute" attributes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Expected") expected

instance Core.ToHeaders PutAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutAttributes where
        type Rs PutAttributes = PutAttributesResponse
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
        parseResponse = Response.receiveNull PutAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAttributesResponse' value with any optional fields omitted.
mkPutAttributesResponse
    :: PutAttributesResponse
mkPutAttributesResponse = PutAttributesResponse'
