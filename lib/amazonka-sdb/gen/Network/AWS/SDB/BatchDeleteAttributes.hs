{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.SDB.BatchDeleteAttributes
    (
    -- * Creating a request
      BatchDeleteAttributes (..)
    , mkBatchDeleteAttributes
    -- ** Request lenses
    , bdaDomainName
    , bdaItems

    -- * Destructuring the response
    , BatchDeleteAttributesResponse (..)
    , mkBatchDeleteAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkBatchDeleteAttributes' smart constructor.
data BatchDeleteAttributes = BatchDeleteAttributes'
  { domainName :: Core.Text
    -- ^ The name of the domain in which the attributes are being deleted.
  , items :: [Types.DeletableItem]
    -- ^ A list of items on which to perform the operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteAttributes' value with any optional fields omitted.
mkBatchDeleteAttributes
    :: Core.Text -- ^ 'domainName'
    -> BatchDeleteAttributes
mkBatchDeleteAttributes domainName
  = BatchDeleteAttributes'{domainName, items = Core.mempty}

-- | The name of the domain in which the attributes are being deleted.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdaDomainName :: Lens.Lens' BatchDeleteAttributes Core.Text
bdaDomainName = Lens.field @"domainName"
{-# INLINEABLE bdaDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | A list of items on which to perform the operation.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdaItems :: Lens.Lens' BatchDeleteAttributes [Types.DeletableItem]
bdaItems = Lens.field @"items"
{-# INLINEABLE bdaItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToQuery BatchDeleteAttributes where
        toQuery BatchDeleteAttributes{..}
          = Core.toQueryPair "Action" ("BatchDeleteAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2009-04-15" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<> Core.toQueryList "Item" items

instance Core.ToHeaders BatchDeleteAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest BatchDeleteAttributes where
        type Rs BatchDeleteAttributes = BatchDeleteAttributesResponse
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
        parseResponse = Response.receiveNull BatchDeleteAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDeleteAttributesResponse' smart constructor.
data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteAttributesResponse' value with any optional fields omitted.
mkBatchDeleteAttributesResponse
    :: BatchDeleteAttributesResponse
mkBatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
