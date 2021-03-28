{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteAttributes (..)
    , mkDeleteAttributes
    -- ** Request lenses
    , daDomainName
    , daItemName
    , daAttributes
    , daExpected

    -- * Destructuring the response
    , DeleteAttributesResponse (..)
    , mkDeleteAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { domainName :: Core.Text
    -- ^ The name of the domain in which to perform the operation.
  , itemName :: Core.Text
    -- ^ The name of the item. Similar to rows on a spreadsheet, items represent individual objects that contain one or more value-attribute pairs.
  , attributes :: Core.Maybe [Types.Attribute]
    -- ^ A list of Attributes. Similar to columns on a spreadsheet, attributes represent categories of data that can be assigned to items.
  , expected :: Core.Maybe Types.UpdateCondition
    -- ^ The update condition which, if specified, determines whether the specified attributes will be deleted or not. The update condition must be satisfied in order for this request to be processed and the attributes to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAttributes' value with any optional fields omitted.
mkDeleteAttributes
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'itemName'
    -> DeleteAttributes
mkDeleteAttributes domainName itemName
  = DeleteAttributes'{domainName, itemName,
                      attributes = Core.Nothing, expected = Core.Nothing}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDomainName :: Lens.Lens' DeleteAttributes Core.Text
daDomainName = Lens.field @"domainName"
{-# INLINEABLE daDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The name of the item. Similar to rows on a spreadsheet, items represent individual objects that contain one or more value-attribute pairs.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daItemName :: Lens.Lens' DeleteAttributes Core.Text
daItemName = Lens.field @"itemName"
{-# INLINEABLE daItemName #-}
{-# DEPRECATED itemName "Use generic-lens or generic-optics with 'itemName' instead"  #-}

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes represent categories of data that can be assigned to items.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttributes :: Lens.Lens' DeleteAttributes (Core.Maybe [Types.Attribute])
daAttributes = Lens.field @"attributes"
{-# INLINEABLE daAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The update condition which, if specified, determines whether the specified attributes will be deleted or not. The update condition must be satisfied in order for this request to be processed and the attributes to be deleted.
--
-- /Note:/ Consider using 'expected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daExpected :: Lens.Lens' DeleteAttributes (Core.Maybe Types.UpdateCondition)
daExpected = Lens.field @"expected"
{-# INLINEABLE daExpected #-}
{-# DEPRECATED expected "Use generic-lens or generic-optics with 'expected' instead"  #-}

instance Core.ToQuery DeleteAttributes where
        toQuery DeleteAttributes{..}
          = Core.toQueryPair "Action" ("DeleteAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "ItemName" itemName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Attribute") attributes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Expected") expected

instance Core.ToHeaders DeleteAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAttributes where
        type Rs DeleteAttributes = DeleteAttributesResponse
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
        parseResponse = Response.receiveNull DeleteAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAttributesResponse' value with any optional fields omitted.
mkDeleteAttributesResponse
    :: DeleteAttributesResponse
mkDeleteAttributesResponse = DeleteAttributesResponse'
