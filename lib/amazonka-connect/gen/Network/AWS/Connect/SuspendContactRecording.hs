{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.SuspendContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, this API suspends recording the call. For example, you might suspend the call recording while collecting sensitive information, such as a credit card number. Then use ResumeContactRecording to restart recording. 
--
-- The period of time that the recording is suspended is filled with silence in the final recording. 
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.SuspendContactRecording
    (
    -- * Creating a request
      SuspendContactRecording (..)
    , mkSuspendContactRecording
    -- ** Request lenses
    , sInstanceId
    , sContactId
    , sInitialContactId

    -- * Destructuring the response
    , SuspendContactRecordingResponse (..)
    , mkSuspendContactRecordingResponse
    -- ** Response lenses
    , srsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSuspendContactRecording' smart constructor.
data SuspendContactRecording = SuspendContactRecording'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , contactId :: Types.ContactId
    -- ^ The identifier of the contact.
  , initialContactId :: Types.InitialContactId
    -- ^ The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendContactRecording' value with any optional fields omitted.
mkSuspendContactRecording
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.ContactId -- ^ 'contactId'
    -> Types.InitialContactId -- ^ 'initialContactId'
    -> SuspendContactRecording
mkSuspendContactRecording instanceId contactId initialContactId
  = SuspendContactRecording'{instanceId, contactId, initialContactId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceId :: Lens.Lens' SuspendContactRecording Types.InstanceId
sInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sContactId :: Lens.Lens' SuspendContactRecording Types.ContactId
sContactId = Lens.field @"contactId"
{-# INLINEABLE sContactId #-}
{-# DEPRECATED contactId "Use generic-lens or generic-optics with 'contactId' instead"  #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInitialContactId :: Lens.Lens' SuspendContactRecording Types.InitialContactId
sInitialContactId = Lens.field @"initialContactId"
{-# INLINEABLE sInitialContactId #-}
{-# DEPRECATED initialContactId "Use generic-lens or generic-optics with 'initialContactId' instead"  #-}

instance Core.ToQuery SuspendContactRecording where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SuspendContactRecording where
        toHeaders SuspendContactRecording{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SuspendContactRecording where
        toJSON SuspendContactRecording{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("ContactId" Core..= contactId),
                  Core.Just ("InitialContactId" Core..= initialContactId)])

instance Core.AWSRequest SuspendContactRecording where
        type Rs SuspendContactRecording = SuspendContactRecordingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/contact/suspend-recording",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 SuspendContactRecordingResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSuspendContactRecordingResponse' smart constructor.
newtype SuspendContactRecordingResponse = SuspendContactRecordingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendContactRecordingResponse' value with any optional fields omitted.
mkSuspendContactRecordingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SuspendContactRecordingResponse
mkSuspendContactRecordingResponse responseStatus
  = SuspendContactRecordingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SuspendContactRecordingResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
