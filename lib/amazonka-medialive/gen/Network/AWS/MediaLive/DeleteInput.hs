{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the input end point
module Network.AWS.MediaLive.DeleteInput
    (
    -- * Creating a request
      DeleteInput (..)
    , mkDeleteInput
    -- ** Request lenses
    , diInputId

    -- * Destructuring the response
    , DeleteInputResponse (..)
    , mkDeleteInputResponse
    -- ** Response lenses
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteInputRequest
--
-- /See:/ 'mkDeleteInput' smart constructor.
newtype DeleteInput = DeleteInput'
  { inputId :: Core.Text
    -- ^ Unique ID of the input
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInput' value with any optional fields omitted.
mkDeleteInput
    :: Core.Text -- ^ 'inputId'
    -> DeleteInput
mkDeleteInput inputId = DeleteInput'{inputId}

-- | Unique ID of the input
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInputId :: Lens.Lens' DeleteInput Core.Text
diInputId = Lens.field @"inputId"
{-# INLINEABLE diInputId #-}
{-# DEPRECATED inputId "Use generic-lens or generic-optics with 'inputId' instead"  #-}

instance Core.ToQuery DeleteInput where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInput where
        toHeaders DeleteInput{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteInput where
        type Rs DeleteInput = DeleteInputResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/prod/inputs/" Core.<> Core.toText inputId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteInputResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DeleteInputResponse
--
-- /See:/ 'mkDeleteInputResponse' smart constructor.
newtype DeleteInputResponse = DeleteInputResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInputResponse' value with any optional fields omitted.
mkDeleteInputResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInputResponse
mkDeleteInputResponse responseStatus
  = DeleteInputResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInputResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
