{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ResumeContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, and the recording has been suspended using SuspendContactRecording, this API resumes recording the call.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.ResumeContactRecording
    (
    -- * Creating a request
      ResumeContactRecording (..)
    , mkResumeContactRecording
    -- ** Request lenses
    , rcrInstanceId
    , rcrContactId
    , rcrInitialContactId

    -- * Destructuring the response
    , ResumeContactRecordingResponse (..)
    , mkResumeContactRecordingResponse
    -- ** Response lenses
    , rcrrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResumeContactRecording' smart constructor.
data ResumeContactRecording = ResumeContactRecording'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , contactId :: Types.ContactId
    -- ^ The identifier of the contact.
  , initialContactId :: Types.InitialContactId
    -- ^ The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeContactRecording' value with any optional fields omitted.
mkResumeContactRecording
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.ContactId -- ^ 'contactId'
    -> Types.InitialContactId -- ^ 'initialContactId'
    -> ResumeContactRecording
mkResumeContactRecording instanceId contactId initialContactId
  = ResumeContactRecording'{instanceId, contactId, initialContactId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrInstanceId :: Lens.Lens' ResumeContactRecording Types.InstanceId
rcrInstanceId = Lens.field @"instanceId"
{-# INLINEABLE rcrInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrContactId :: Lens.Lens' ResumeContactRecording Types.ContactId
rcrContactId = Lens.field @"contactId"
{-# INLINEABLE rcrContactId #-}
{-# DEPRECATED contactId "Use generic-lens or generic-optics with 'contactId' instead"  #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrInitialContactId :: Lens.Lens' ResumeContactRecording Types.InitialContactId
rcrInitialContactId = Lens.field @"initialContactId"
{-# INLINEABLE rcrInitialContactId #-}
{-# DEPRECATED initialContactId "Use generic-lens or generic-optics with 'initialContactId' instead"  #-}

instance Core.ToQuery ResumeContactRecording where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResumeContactRecording where
        toHeaders ResumeContactRecording{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResumeContactRecording where
        toJSON ResumeContactRecording{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("ContactId" Core..= contactId),
                  Core.Just ("InitialContactId" Core..= initialContactId)])

instance Core.AWSRequest ResumeContactRecording where
        type Rs ResumeContactRecording = ResumeContactRecordingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/contact/resume-recording",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ResumeContactRecordingResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResumeContactRecordingResponse' smart constructor.
newtype ResumeContactRecordingResponse = ResumeContactRecordingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeContactRecordingResponse' value with any optional fields omitted.
mkResumeContactRecordingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResumeContactRecordingResponse
mkResumeContactRecordingResponse responseStatus
  = ResumeContactRecordingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrrsResponseStatus :: Lens.Lens' ResumeContactRecordingResponse Core.Int
rcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
