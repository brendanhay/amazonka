{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.RejectInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reject the transfer of the specified input device to your AWS account.
module Network.AWS.MediaLive.RejectInputDeviceTransfer
    (
    -- * Creating a request
      RejectInputDeviceTransfer (..)
    , mkRejectInputDeviceTransfer
    -- ** Request lenses
    , ridtInputDeviceId

    -- * Destructuring the response
    , RejectInputDeviceTransferResponse (..)
    , mkRejectInputDeviceTransferResponse
    -- ** Response lenses
    , ridtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for RejectInputDeviceTransferRequest
--
-- /See:/ 'mkRejectInputDeviceTransfer' smart constructor.
newtype RejectInputDeviceTransfer = RejectInputDeviceTransfer'
  { inputDeviceId :: Core.Text
    -- ^ The unique ID of the input device to reject. For example, hd-123456789abcdef.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectInputDeviceTransfer' value with any optional fields omitted.
mkRejectInputDeviceTransfer
    :: Core.Text -- ^ 'inputDeviceId'
    -> RejectInputDeviceTransfer
mkRejectInputDeviceTransfer inputDeviceId
  = RejectInputDeviceTransfer'{inputDeviceId}

-- | The unique ID of the input device to reject. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridtInputDeviceId :: Lens.Lens' RejectInputDeviceTransfer Core.Text
ridtInputDeviceId = Lens.field @"inputDeviceId"
{-# INLINEABLE ridtInputDeviceId #-}
{-# DEPRECATED inputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead"  #-}

instance Core.ToQuery RejectInputDeviceTransfer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectInputDeviceTransfer where
        toHeaders RejectInputDeviceTransfer{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RejectInputDeviceTransfer where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest RejectInputDeviceTransfer where
        type Rs RejectInputDeviceTransfer =
             RejectInputDeviceTransferResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/prod/inputDevices/" Core.<> Core.toText inputDeviceId Core.<>
                             "/reject",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RejectInputDeviceTransferResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for RejectInputDeviceTransferResponse
--
-- /See:/ 'mkRejectInputDeviceTransferResponse' smart constructor.
newtype RejectInputDeviceTransferResponse = RejectInputDeviceTransferResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectInputDeviceTransferResponse' value with any optional fields omitted.
mkRejectInputDeviceTransferResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectInputDeviceTransferResponse
mkRejectInputDeviceTransferResponse responseStatus
  = RejectInputDeviceTransferResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridtrrsResponseStatus :: Lens.Lens' RejectInputDeviceTransferResponse Core.Int
ridtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ridtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
