{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ResumeContactRecording (..),
    mkResumeContactRecording,

    -- ** Request lenses
    rcrInstanceId,
    rcrContactId,
    rcrInitialContactId,

    -- * Destructuring the response
    ResumeContactRecordingResponse (..),
    mkResumeContactRecordingResponse,

    -- ** Response lenses
    rcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResumeContactRecording' smart constructor.
data ResumeContactRecording = ResumeContactRecording'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The identifier of the contact.
    contactId :: Types.ContactId,
    -- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
    initialContactId :: Types.InitialContactId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeContactRecording' value with any optional fields omitted.
mkResumeContactRecording ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'contactId'
  Types.ContactId ->
  -- | 'initialContactId'
  Types.InitialContactId ->
  ResumeContactRecording
mkResumeContactRecording instanceId contactId initialContactId =
  ResumeContactRecording' {instanceId, contactId, initialContactId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrInstanceId :: Lens.Lens' ResumeContactRecording Types.InstanceId
rcrInstanceId = Lens.field @"instanceId"
{-# DEPRECATED rcrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrContactId :: Lens.Lens' ResumeContactRecording Types.ContactId
rcrContactId = Lens.field @"contactId"
{-# DEPRECATED rcrContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrInitialContactId :: Lens.Lens' ResumeContactRecording Types.InitialContactId
rcrInitialContactId = Lens.field @"initialContactId"
{-# DEPRECATED rcrInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

instance Core.FromJSON ResumeContactRecording where
  toJSON ResumeContactRecording {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("ContactId" Core..= contactId),
            Core.Just ("InitialContactId" Core..= initialContactId)
          ]
      )

instance Core.AWSRequest ResumeContactRecording where
  type Rs ResumeContactRecording = ResumeContactRecordingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/contact/resume-recording",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResumeContactRecordingResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResumeContactRecordingResponse' smart constructor.
newtype ResumeContactRecordingResponse = ResumeContactRecordingResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeContactRecordingResponse' value with any optional fields omitted.
mkResumeContactRecordingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResumeContactRecordingResponse
mkResumeContactRecordingResponse responseStatus =
  ResumeContactRecordingResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrrsResponseStatus :: Lens.Lens' ResumeContactRecordingResponse Core.Int
rcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
