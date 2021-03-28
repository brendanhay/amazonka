{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP address filter.
--
-- For information about managing IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteReceiptFilter
    (
    -- * Creating a request
      DeleteReceiptFilter (..)
    , mkDeleteReceiptFilter
    -- ** Request lenses
    , drfFilterName

    -- * Destructuring the response
    , DeleteReceiptFilterResponse (..)
    , mkDeleteReceiptFilterResponse
    -- ** Response lenses
    , drfrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteReceiptFilter' smart constructor.
newtype DeleteReceiptFilter = DeleteReceiptFilter'
  { filterName :: Types.ReceiptFilterName
    -- ^ The name of the IP address filter to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReceiptFilter' value with any optional fields omitted.
mkDeleteReceiptFilter
    :: Types.ReceiptFilterName -- ^ 'filterName'
    -> DeleteReceiptFilter
mkDeleteReceiptFilter filterName = DeleteReceiptFilter'{filterName}

-- | The name of the IP address filter to delete.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfFilterName :: Lens.Lens' DeleteReceiptFilter Types.ReceiptFilterName
drfFilterName = Lens.field @"filterName"
{-# INLINEABLE drfFilterName #-}
{-# DEPRECATED filterName "Use generic-lens or generic-optics with 'filterName' instead"  #-}

instance Core.ToQuery DeleteReceiptFilter where
        toQuery DeleteReceiptFilter{..}
          = Core.toQueryPair "Action" ("DeleteReceiptFilter" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "FilterName" filterName

instance Core.ToHeaders DeleteReceiptFilter where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteReceiptFilter where
        type Rs DeleteReceiptFilter = DeleteReceiptFilterResponse
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
          = Response.receiveXMLWrapper "DeleteReceiptFilterResult"
              (\ s h x ->
                 DeleteReceiptFilterResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteReceiptFilterResponse' smart constructor.
newtype DeleteReceiptFilterResponse = DeleteReceiptFilterResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReceiptFilterResponse' value with any optional fields omitted.
mkDeleteReceiptFilterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReceiptFilterResponse
mkDeleteReceiptFilterResponse responseStatus
  = DeleteReceiptFilterResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsResponseStatus :: Lens.Lens' DeleteReceiptFilterResponse Core.Int
drfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
