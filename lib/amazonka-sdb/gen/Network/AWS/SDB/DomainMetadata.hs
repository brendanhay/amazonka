{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DomainMetadata (..),
    mkDomainMetadata,

    -- ** Request lenses
    dmDomainName,

    -- * Destructuring the response
    DomainMetadataResponse (..),
    mkDomainMetadataResponse,

    -- ** Response lenses
    dmrrsAttributeNameCount,
    dmrrsAttributeNamesSizeBytes,
    dmrrsAttributeValueCount,
    dmrrsAttributeValuesSizeBytes,
    dmrrsItemCount,
    dmrrsItemNamesSizeBytes,
    dmrrsTimestamp,
    dmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkDomainMetadata' smart constructor.
newtype DomainMetadata = DomainMetadata'
  { -- | The name of the domain for which to display the metadata of.
    domainName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DomainMetadata' value with any optional fields omitted.
mkDomainMetadata ::
  -- | 'domainName'
  Types.String ->
  DomainMetadata
mkDomainMetadata domainName = DomainMetadata' {domainName}

-- | The name of the domain for which to display the metadata of.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDomainName :: Lens.Lens' DomainMetadata Types.String
dmDomainName = Lens.field @"domainName"
{-# DEPRECATED dmDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest DomainMetadata where
  type Rs DomainMetadata = DomainMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DomainMetadata")
                Core.<> (Core.pure ("Version", "2009-04-15"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DomainMetadataResult"
      ( \s h x ->
          DomainMetadataResponse'
            Core.<$> (x Core..@? "AttributeNameCount")
            Core.<*> (x Core..@? "AttributeNamesSizeBytes")
            Core.<*> (x Core..@? "AttributeValueCount")
            Core.<*> (x Core..@? "AttributeValuesSizeBytes")
            Core.<*> (x Core..@? "ItemCount")
            Core.<*> (x Core..@? "ItemNamesSizeBytes")
            Core.<*> (x Core..@? "Timestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDomainMetadataResponse' smart constructor.
data DomainMetadataResponse = DomainMetadataResponse'
  { -- | The number of unique attribute names in the domain.
    attributeNameCount :: Core.Maybe Core.Int,
    -- | The total size of all unique attribute names in the domain, in bytes.
    attributeNamesSizeBytes :: Core.Maybe Core.Integer,
    -- | The number of all attribute name/value pairs in the domain.
    attributeValueCount :: Core.Maybe Core.Int,
    -- | The total size of all attribute values in the domain, in bytes.
    attributeValuesSizeBytes :: Core.Maybe Core.Integer,
    -- | The number of all items in the domain.
    itemCount :: Core.Maybe Core.Int,
    -- | The total size of all item names in the domain, in bytes.
    itemNamesSizeBytes :: Core.Maybe Core.Integer,
    -- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
    timestamp :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainMetadataResponse' value with any optional fields omitted.
mkDomainMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DomainMetadataResponse
mkDomainMetadataResponse responseStatus =
  DomainMetadataResponse'
    { attributeNameCount = Core.Nothing,
      attributeNamesSizeBytes = Core.Nothing,
      attributeValueCount = Core.Nothing,
      attributeValuesSizeBytes = Core.Nothing,
      itemCount = Core.Nothing,
      itemNamesSizeBytes = Core.Nothing,
      timestamp = Core.Nothing,
      responseStatus
    }

-- | The number of unique attribute names in the domain.
--
-- /Note:/ Consider using 'attributeNameCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeNameCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsAttributeNameCount = Lens.field @"attributeNameCount"
{-# DEPRECATED dmrrsAttributeNameCount "Use generic-lens or generic-optics with 'attributeNameCount' instead." #-}

-- | The total size of all unique attribute names in the domain, in bytes.
--
-- /Note:/ Consider using 'attributeNamesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
dmrrsAttributeNamesSizeBytes = Lens.field @"attributeNamesSizeBytes"
{-# DEPRECATED dmrrsAttributeNamesSizeBytes "Use generic-lens or generic-optics with 'attributeNamesSizeBytes' instead." #-}

-- | The number of all attribute name/value pairs in the domain.
--
-- /Note:/ Consider using 'attributeValueCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeValueCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsAttributeValueCount = Lens.field @"attributeValueCount"
{-# DEPRECATED dmrrsAttributeValueCount "Use generic-lens or generic-optics with 'attributeValueCount' instead." #-}

-- | The total size of all attribute values in the domain, in bytes.
--
-- /Note:/ Consider using 'attributeValuesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAttributeValuesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
dmrrsAttributeValuesSizeBytes = Lens.field @"attributeValuesSizeBytes"
{-# DEPRECATED dmrrsAttributeValuesSizeBytes "Use generic-lens or generic-optics with 'attributeValuesSizeBytes' instead." #-}

-- | The number of all items in the domain.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsItemCount :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsItemCount = Lens.field @"itemCount"
{-# DEPRECATED dmrrsItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The total size of all item names in the domain, in bytes.
--
-- /Note:/ Consider using 'itemNamesSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsItemNamesSizeBytes :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Integer)
dmrrsItemNamesSizeBytes = Lens.field @"itemNamesSizeBytes"
{-# DEPRECATED dmrrsItemNamesSizeBytes "Use generic-lens or generic-optics with 'itemNamesSizeBytes' instead." #-}

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsTimestamp :: Lens.Lens' DomainMetadataResponse (Core.Maybe Core.Int)
dmrrsTimestamp = Lens.field @"timestamp"
{-# DEPRECATED dmrrsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DomainMetadataResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
