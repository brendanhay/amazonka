{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DomainMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the domain, including when the domain was created, the number of items and attributes in the domain, and the size of the attribute names and values. 
module Network.AWS.SDB.DomainMetadata
    (
    -- * Creating a request
      DomainMetadata (..)
    , mkDomainMetadata
    -- ** Request lenses
    , dmDomainName

    -- * Destructuring the response
    , DomainMetadataResponse (..)
    , mkDomainMetadataResponse
    -- ** Response lenses
    , dmrrsAttributeNameCount
    , dmrrsAttributeNamesSizeBytes
    , dmrrsAttributeValueCount
    , dmrrsAttributeValuesSizeBytes
    , dmrrsItemCount
    , dmrrsItemNamesSizeBytes
    , dmrrsTimestamp
    , dmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkDomainMetadata' smart constructor.
newtype DomainMetadata = DomainMetadata'
  { domainName :: Core.Text
    -- ^ The name of the domain for which to display the metadata of.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DomainMetadata' value with any optional fields omitted.
mkDomainMetadata
    :: Core.Text -- ^ 'domainName'
    -> DomainMetadata
mkDomainMetadata domainName = DomainMetadata'{domainName}

-- | The name of the domain for which to display the metadata of.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDomainName :: Lens.Lens' DomainMetadata Core.Text
dmDomainName = Lens.field @"domainName"
{-# INLINEABLE dmDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DomainMetadata where
        toQuery DomainMetadata{..}
          = Core.toQueryPair "Action" ("DomainMetadata" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName

instance Core.ToHeaders DomainMetadata where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DomainMetadata where
        type Rs DomainMetadata = DomainMetadataResponse
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
          = Response.receiveXMLWrapper "DomainMetadataResult"
              (\ s h x ->
                 DomainMetadataResponse' Core.<$>
                   (x Core..@? "AttributeNameCount") Core.<*>
                     x Core..@? "AttributeNamesSizeBytes"
                     Core.<*> x Core..@? "AttributeValueCount"
                     Core.<*> x Core..@? "AttributeValuesSizeBytes"
                     Core.<*> x Core..@? "ItemCount"
                     Core.<*> x Core..@? "ItemNamesSizeBytes"
                     Core.<*> x Core..@? "Timestamp"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDomainMetadataResponse' smart constructor.
data DomainMetadataResponse = DomainMetadataResponse'
  { attributeNameCount :: Core.Maybe Core.Int
    -- ^ The number of unique attribute names in the domain.
  , attributeNamesSizeBytes :: Core.Maybe Core.Integer
    -- ^ The total size of all unique attribute names in the domain, in bytes.
  , attributeValueCount :: Core.Maybe Core.Int
    -- ^ The number of all attribute name/value pairs in the domain.
  , attributeValuesSizeBytes :: Core.Maybe Core.Integer
    -- ^ The total size of all attribute values in the domain, in bytes.
  , itemCount :: Core.Maybe Core.Int
    -- ^ The number of all items in the domain.
  , itemNamesSizeBytes :: Core.Maybe Core.Integer
    -- ^ The total size of all item names in the domain, in bytes.
  , timestamp :: Core.Maybe Core.Int
    -- ^ The data and time when metadata was calculated, in Epoch (UNIX) seconds.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainMetadataResponse' value with any optional fields omitted.
mkDomainMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DomainMetadataResponse
mkDomainMetadataResponse responseStatus
  = DomainMetadataResponse'{attributeNameCount = Core.Nothing,
                            attributeNamesSizeBytes = Core.Nothing,
                            attributeValueCount = Core.Nothing,
                            attributeValuesSizeBytes = Core.Nothing, itemCount = Core.Nothing,
                            itemNamesSizeBytes = Core.Nothing, timestamp = Core.Nothing,
                            responseStatus}

-- | The number of unique attribute names in the domain.
--
-- /Note:/ Consider using 'attributeNameCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeNameCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsAttributeNameCount = Lens.field @"attributeNameCount"
{-# INLINEABLE dmrrsAttributeNameCount #-}
{-# DEPRECATED attributeNameCount "Use generic-lens or generic-optics with 'attributeNameCount' instead"  #-}

-- | The total size of all unique attribute names in the domain, in bytes.
--
-- /Note:/ Consider using 'attributeNamesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
dmrrsAttributeNamesSizeBytes = Lens.field @"attributeNamesSizeBytes"
{-# INLINEABLE dmrrsAttributeNamesSizeBytes #-}
{-# DEPRECATED attributeNamesSizeBytes "Use generic-lens or generic-optics with 'attributeNamesSizeBytes' instead"  #-}

-- | The number of all attribute name/value pairs in the domain.
--
-- /Note:/ Consider using 'attributeValueCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeValueCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsAttributeValueCount = Lens.field @"attributeValueCount"
{-# INLINEABLE dmrrsAttributeValueCount #-}
{-# DEPRECATED attributeValueCount "Use generic-lens or generic-optics with 'attributeValueCount' instead"  #-}

-- | The total size of all attribute values in the domain, in bytes.
--
-- /Note:/ Consider using 'attributeValuesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeValuesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
dmrrsAttributeValuesSizeBytes = Lens.field @"attributeValuesSizeBytes"
{-# INLINEABLE dmrrsAttributeValuesSizeBytes #-}
{-# DEPRECATED attributeValuesSizeBytes "Use generic-lens or generic-optics with 'attributeValuesSizeBytes' instead"  #-}

-- | The number of all items in the domain.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsItemCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsItemCount = Lens.field @"itemCount"
{-# INLINEABLE dmrrsItemCount #-}
{-# DEPRECATED itemCount "Use generic-lens or generic-optics with 'itemCount' instead"  #-}

-- | The total size of all item names in the domain, in bytes.
--
-- /Note:/ Consider using 'itemNamesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsItemNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
dmrrsItemNamesSizeBytes = Lens.field @"itemNamesSizeBytes"
{-# INLINEABLE dmrrsItemNamesSizeBytes #-}
{-# DEPRECATED itemNamesSizeBytes "Use generic-lens or generic-optics with 'itemNamesSizeBytes' instead"  #-}

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsTimestamp :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsTimestamp = Lens.field @"timestamp"
{-# INLINEABLE dmrrsTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DomainMetadataResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
