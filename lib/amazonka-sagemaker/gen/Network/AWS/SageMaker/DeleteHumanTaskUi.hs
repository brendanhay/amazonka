{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a human task user interface (worker task template).
--
-- To see a list of human task user interfaces (work task templates) in your account, use . When you delete a worker task template, it no longer appears when you call @ListHumanTaskUis@ .
module Network.AWS.SageMaker.DeleteHumanTaskUi
    (
    -- * Creating a request
      DeleteHumanTaskUi (..)
    , mkDeleteHumanTaskUi
    -- ** Request lenses
    , dhtuHumanTaskUiName

    -- * Destructuring the response
    , DeleteHumanTaskUiResponse (..)
    , mkDeleteHumanTaskUiResponse
    -- ** Response lenses
    , dhturrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteHumanTaskUi' smart constructor.
newtype DeleteHumanTaskUi = DeleteHumanTaskUi'
  { humanTaskUiName :: Types.HumanTaskUiName
    -- ^ The name of the human task user interface (work task template) you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHumanTaskUi' value with any optional fields omitted.
mkDeleteHumanTaskUi
    :: Types.HumanTaskUiName -- ^ 'humanTaskUiName'
    -> DeleteHumanTaskUi
mkDeleteHumanTaskUi humanTaskUiName
  = DeleteHumanTaskUi'{humanTaskUiName}

-- | The name of the human task user interface (work task template) you want to delete.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuHumanTaskUiName :: Lens.Lens' DeleteHumanTaskUi Types.HumanTaskUiName
dhtuHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# INLINEABLE dhtuHumanTaskUiName #-}
{-# DEPRECATED humanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead"  #-}

instance Core.ToQuery DeleteHumanTaskUi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHumanTaskUi where
        toHeaders DeleteHumanTaskUi{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteHumanTaskUi") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteHumanTaskUi where
        toJSON DeleteHumanTaskUi{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HumanTaskUiName" Core..= humanTaskUiName)])

instance Core.AWSRequest DeleteHumanTaskUi where
        type Rs DeleteHumanTaskUi = DeleteHumanTaskUiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteHumanTaskUiResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteHumanTaskUiResponse' smart constructor.
newtype DeleteHumanTaskUiResponse = DeleteHumanTaskUiResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHumanTaskUiResponse' value with any optional fields omitted.
mkDeleteHumanTaskUiResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteHumanTaskUiResponse
mkDeleteHumanTaskUiResponse responseStatus
  = DeleteHumanTaskUiResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhturrsResponseStatus :: Lens.Lens' DeleteHumanTaskUiResponse Core.Int
dhturrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhturrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
