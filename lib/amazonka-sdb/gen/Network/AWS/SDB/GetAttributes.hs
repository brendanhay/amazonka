{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.GetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the attributes associated with the specified item. Optionally, the attributes returned can be limited to one or more attributes by specifying an attribute name parameter. 
--
-- If the item does not exist on the replica that was accessed for this operation, an empty set is returned. The system does not return an error as it cannot guarantee the item does not exist on other replicas. 
module Network.AWS.SDB.GetAttributes
    (
    -- * Creating a request
      GetAttributes (..)
    , mkGetAttributes
    -- ** Request lenses
    , gaDomainName
    , gaItemName
    , gaAttributeNames
    , gaConsistentRead

    -- * Destructuring the response
    , GetAttributesResponse (..)
    , mkGetAttributesResponse
    -- ** Response lenses
    , garrsAttributes
    , garrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkGetAttributes' smart constructor.
data GetAttributes = GetAttributes'
  { domainName :: Core.Text
    -- ^ The name of the domain in which to perform the operation.
  , itemName :: Core.Text
    -- ^ The name of the item.
  , attributeNames :: Core.Maybe [Core.Text]
    -- ^ The names of the attributes.
  , consistentRead :: Core.Maybe Core.Bool
    -- ^ @true@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAttributes' value with any optional fields omitted.
mkGetAttributes
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'itemName'
    -> GetAttributes
mkGetAttributes domainName itemName
  = GetAttributes'{domainName, itemName,
                   attributeNames = Core.Nothing, consistentRead = Core.Nothing}

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDomainName :: Lens.Lens' GetAttributes Core.Text
gaDomainName = Lens.field @"domainName"
{-# INLINEABLE gaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaItemName :: Lens.Lens' GetAttributes Core.Text
gaItemName = Lens.field @"itemName"
{-# INLINEABLE gaItemName #-}
{-# DEPRECATED itemName "Use generic-lens or generic-optics with 'itemName' instead"  #-}

-- | The names of the attributes.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAttributeNames :: Lens.Lens' GetAttributes (Core.Maybe [Core.Text])
gaAttributeNames = Lens.field @"attributeNames"
{-# INLINEABLE gaAttributeNames #-}
{-# DEPRECATED attributeNames "Use generic-lens or generic-optics with 'attributeNames' instead"  #-}

-- | @true@ 
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaConsistentRead :: Lens.Lens' GetAttributes (Core.Maybe Core.Bool)
gaConsistentRead = Lens.field @"consistentRead"
{-# INLINEABLE gaConsistentRead #-}
{-# DEPRECATED consistentRead "Use generic-lens or generic-optics with 'consistentRead' instead"  #-}

instance Core.ToQuery GetAttributes where
        toQuery GetAttributes{..}
          = Core.toQueryPair "Action" ("GetAttributes" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryPair "ItemName" itemName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AttributeName")
                attributeNames
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ConsistentRead")
                consistentRead

instance Core.ToHeaders GetAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetAttributes where
        type Rs GetAttributes = GetAttributesResponse
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
        parseResponse
          = Response.receiveXMLWrapper "GetAttributesResult"
              (\ s h x ->
                 GetAttributesResponse' Core.<$>
                   (x Core..@? "Attribute") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAttributesResponse' smart constructor.
data GetAttributesResponse = GetAttributesResponse'
  { attributes :: Core.Maybe [Types.Attribute]
    -- ^ The list of attributes returned by the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAttributesResponse' value with any optional fields omitted.
mkGetAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAttributesResponse
mkGetAttributesResponse responseStatus
  = GetAttributesResponse'{attributes = Core.Nothing, responseStatus}

-- | The list of attributes returned by the operation.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAttributes :: Lens.Lens' GetAttributesResponse (Core.Maybe [Types.Attribute])
garrsAttributes = Lens.field @"attributes"
{-# INLINEABLE garrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAttributesResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
