{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.NotifyWorkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @NotifyWorkers@ operation sends an email to one or more Workers that you specify with the Worker ID. You can specify up to 100 Worker IDs to send the same message with a single call to the NotifyWorkers operation. The NotifyWorkers operation will send a notification email to a Worker only if you have previously approved or rejected work from the Worker. 
module Network.AWS.MechanicalTurk.NotifyWorkers
    (
    -- * Creating a request
      NotifyWorkers (..)
    , mkNotifyWorkers
    -- ** Request lenses
    , nwSubject
    , nwMessageText
    , nwWorkerIds

    -- * Destructuring the response
    , NotifyWorkersResponse (..)
    , mkNotifyWorkersResponse
    -- ** Response lenses
    , nwrrsNotifyWorkersFailureStatuses
    , nwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkNotifyWorkers' smart constructor.
data NotifyWorkers = NotifyWorkers'
  { subject :: Core.Text
    -- ^ The subject line of the email message to send. Can include up to 200 characters.
  , messageText :: Core.Text
    -- ^ The text of the email message to send. Can include up to 4,096 characters
  , workerIds :: [Types.CustomerId]
    -- ^ A list of Worker IDs you wish to notify. You can notify upto 100 Workers at a time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyWorkers' value with any optional fields omitted.
mkNotifyWorkers
    :: Core.Text -- ^ 'subject'
    -> Core.Text -- ^ 'messageText'
    -> NotifyWorkers
mkNotifyWorkers subject messageText
  = NotifyWorkers'{subject, messageText, workerIds = Core.mempty}

-- | The subject line of the email message to send. Can include up to 200 characters.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwSubject :: Lens.Lens' NotifyWorkers Core.Text
nwSubject = Lens.field @"subject"
{-# INLINEABLE nwSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The text of the email message to send. Can include up to 4,096 characters
--
-- /Note:/ Consider using 'messageText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwMessageText :: Lens.Lens' NotifyWorkers Core.Text
nwMessageText = Lens.field @"messageText"
{-# INLINEABLE nwMessageText #-}
{-# DEPRECATED messageText "Use generic-lens or generic-optics with 'messageText' instead"  #-}

-- | A list of Worker IDs you wish to notify. You can notify upto 100 Workers at a time.
--
-- /Note:/ Consider using 'workerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwWorkerIds :: Lens.Lens' NotifyWorkers [Types.CustomerId]
nwWorkerIds = Lens.field @"workerIds"
{-# INLINEABLE nwWorkerIds #-}
{-# DEPRECATED workerIds "Use generic-lens or generic-optics with 'workerIds' instead"  #-}

instance Core.ToQuery NotifyWorkers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders NotifyWorkers where
        toHeaders NotifyWorkers{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.NotifyWorkers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON NotifyWorkers where
        toJSON NotifyWorkers{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Subject" Core..= subject),
                  Core.Just ("MessageText" Core..= messageText),
                  Core.Just ("WorkerIds" Core..= workerIds)])

instance Core.AWSRequest NotifyWorkers where
        type Rs NotifyWorkers = NotifyWorkersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 NotifyWorkersResponse' Core.<$>
                   (x Core..:? "NotifyWorkersFailureStatuses") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkNotifyWorkersResponse' smart constructor.
data NotifyWorkersResponse = NotifyWorkersResponse'
  { notifyWorkersFailureStatuses :: Core.Maybe [Types.NotifyWorkersFailureStatus]
    -- ^ When MTurk sends notifications to the list of Workers, it returns back any failures it encounters in this list of NotifyWorkersFailureStatus objects. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyWorkersResponse' value with any optional fields omitted.
mkNotifyWorkersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> NotifyWorkersResponse
mkNotifyWorkersResponse responseStatus
  = NotifyWorkersResponse'{notifyWorkersFailureStatuses =
                             Core.Nothing,
                           responseStatus}

-- | When MTurk sends notifications to the list of Workers, it returns back any failures it encounters in this list of NotifyWorkersFailureStatus objects. 
--
-- /Note:/ Consider using 'notifyWorkersFailureStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwrrsNotifyWorkersFailureStatuses :: Lens.Lens' NotifyWorkersResponse (Core.Maybe [Types.NotifyWorkersFailureStatus])
nwrrsNotifyWorkersFailureStatuses = Lens.field @"notifyWorkersFailureStatuses"
{-# INLINEABLE nwrrsNotifyWorkersFailureStatuses #-}
{-# DEPRECATED notifyWorkersFailureStatuses "Use generic-lens or generic-optics with 'notifyWorkersFailureStatuses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwrrsResponseStatus :: Lens.Lens' NotifyWorkersResponse Core.Int
nwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE nwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
