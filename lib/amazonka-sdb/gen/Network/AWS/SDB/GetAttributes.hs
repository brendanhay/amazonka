{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetAttributes (..),
    mkGetAttributes,

    -- ** Request lenses
    gaDomainName,
    gaItemName,
    gaAttributeNames,
    gaConsistentRead,

    -- * Destructuring the response
    GetAttributesResponse (..),
    mkGetAttributesResponse,

    -- ** Response lenses
    garrsAttributes,
    garrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkGetAttributes' smart constructor.
data GetAttributes = GetAttributes'
  { -- | The name of the domain in which to perform the operation.
    domainName :: Types.String,
    -- | The name of the item.
    itemName :: Types.String,
    -- | The names of the attributes.
    attributeNames :: Core.Maybe [Types.String],
    -- | @true@
    consistentRead :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAttributes' value with any optional fields omitted.
mkGetAttributes ::
  -- | 'domainName'
  Types.String ->
  -- | 'itemName'
  Types.String ->
  GetAttributes
mkGetAttributes domainName itemName =
  GetAttributes'
    { domainName,
      itemName,
      attributeNames = Core.Nothing,
      consistentRead = Core.Nothing
    }

-- | The name of the domain in which to perform the operation.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaDomainName :: Lens.Lens' GetAttributes Types.String
gaDomainName = Lens.field @"domainName"
{-# DEPRECATED gaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'itemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaItemName :: Lens.Lens' GetAttributes Types.String
gaItemName = Lens.field @"itemName"
{-# DEPRECATED gaItemName "Use generic-lens or generic-optics with 'itemName' instead." #-}

-- | The names of the attributes.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAttributeNames :: Lens.Lens' GetAttributes (Core.Maybe [Types.String])
gaAttributeNames = Lens.field @"attributeNames"
{-# DEPRECATED gaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | @true@
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaConsistentRead :: Lens.Lens' GetAttributes (Core.Maybe Core.Bool)
gaConsistentRead = Lens.field @"consistentRead"
{-# DEPRECATED gaConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

instance Core.AWSRequest GetAttributes where
  type Rs GetAttributes = GetAttributesResponse
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
            ( Core.pure ("Action", "GetAttributes")
                Core.<> (Core.pure ("Version", "2009-04-15"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryValue "ItemName" itemName)
                Core.<> (Core.toQueryList "AttributeName" Core.<$> attributeNames)
                Core.<> (Core.toQueryValue "ConsistentRead" Core.<$> consistentRead)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetAttributesResult"
      ( \s h x ->
          GetAttributesResponse'
            Core.<$> (x Core..@? "Attribute") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAttributesResponse' smart constructor.
data GetAttributesResponse = GetAttributesResponse'
  { -- | The list of attributes returned by the operation.
    attributes :: Core.Maybe [Types.Attribute],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAttributesResponse' value with any optional fields omitted.
mkGetAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAttributesResponse
mkGetAttributesResponse responseStatus =
  GetAttributesResponse' {attributes = Core.Nothing, responseStatus}

-- | The list of attributes returned by the operation.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAttributes :: Lens.Lens' GetAttributesResponse (Core.Maybe [Types.Attribute])
garrsAttributes = Lens.field @"attributes"
{-# DEPRECATED garrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAttributesResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
