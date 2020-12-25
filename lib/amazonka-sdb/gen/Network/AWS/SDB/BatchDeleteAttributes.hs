{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs multiple DeleteAttributes operations in a single call, which reduces round trips and latencies. This enables Amazon SimpleDB to optimize requests, which generally yields better throughput.
--
-- The following limitations are enforced for this operation:
--     * 1 MB request size
--
--     * 25 item limit per BatchDeleteAttributes operation
module Network.AWS.SDB.BatchDeleteAttributes
  ( -- * Creating a request
    BatchDeleteAttributes (..),
    mkBatchDeleteAttributes,

    -- ** Request lenses
    bdaDomainName,
    bdaItems,

    -- * Destructuring the response
    BatchDeleteAttributesResponse (..),
    mkBatchDeleteAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkBatchDeleteAttributes' smart constructor.
data BatchDeleteAttributes = BatchDeleteAttributes'
  { -- | The name of the domain in which the attributes are being deleted.
    domainName :: Types.String,
    -- | A list of items on which to perform the operation.
    items :: [Types.DeletableItem]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteAttributes' value with any optional fields omitted.
mkBatchDeleteAttributes ::
  -- | 'domainName'
  Types.String ->
  BatchDeleteAttributes
mkBatchDeleteAttributes domainName =
  BatchDeleteAttributes' {domainName, items = Core.mempty}

-- | The name of the domain in which the attributes are being deleted.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdaDomainName :: Lens.Lens' BatchDeleteAttributes Types.String
bdaDomainName = Lens.field @"domainName"
{-# DEPRECATED bdaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A list of items on which to perform the operation.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdaItems :: Lens.Lens' BatchDeleteAttributes [Types.DeletableItem]
bdaItems = Lens.field @"items"
{-# DEPRECATED bdaItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Core.AWSRequest BatchDeleteAttributes where
  type Rs BatchDeleteAttributes = BatchDeleteAttributesResponse
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
            ( Core.pure ("Action", "BatchDeleteAttributes")
                Core.<> (Core.pure ("Version", "2009-04-15"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
                Core.<> (Core.toQueryList "Item" items)
            )
      }
  response = Response.receiveNull BatchDeleteAttributesResponse'

-- | /See:/ 'mkBatchDeleteAttributesResponse' smart constructor.
data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteAttributesResponse' value with any optional fields omitted.
mkBatchDeleteAttributesResponse ::
  BatchDeleteAttributesResponse
mkBatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
