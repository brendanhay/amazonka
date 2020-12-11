{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rcrrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResumeContactRecording' smart constructor.
data ResumeContactRecording = ResumeContactRecording'
  { instanceId ::
      Lude.Text,
    contactId :: Lude.Text,
    initialContactId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeContactRecording' with the minimum fields required to make a request.
--
-- * 'contactId' - The identifier of the contact.
-- * 'initialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkResumeContactRecording ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactId'
  Lude.Text ->
  -- | 'initialContactId'
  Lude.Text ->
  ResumeContactRecording
mkResumeContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_ =
    ResumeContactRecording'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrInstanceId :: Lens.Lens' ResumeContactRecording Lude.Text
rcrInstanceId = Lens.lens (instanceId :: ResumeContactRecording -> Lude.Text) (\s a -> s {instanceId = a} :: ResumeContactRecording)
{-# DEPRECATED rcrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrContactId :: Lens.Lens' ResumeContactRecording Lude.Text
rcrContactId = Lens.lens (contactId :: ResumeContactRecording -> Lude.Text) (\s a -> s {contactId = a} :: ResumeContactRecording)
{-# DEPRECATED rcrContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrInitialContactId :: Lens.Lens' ResumeContactRecording Lude.Text
rcrInitialContactId = Lens.lens (initialContactId :: ResumeContactRecording -> Lude.Text) (\s a -> s {initialContactId = a} :: ResumeContactRecording)
{-# DEPRECATED rcrInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

instance Lude.AWSRequest ResumeContactRecording where
  type Rs ResumeContactRecording = ResumeContactRecordingResponse
  request = Req.postJSON connectService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ResumeContactRecordingResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResumeContactRecording where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResumeContactRecording where
  toJSON ResumeContactRecording' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("ContactId" Lude..= contactId),
            Lude.Just ("InitialContactId" Lude..= initialContactId)
          ]
      )

instance Lude.ToPath ResumeContactRecording where
  toPath = Lude.const "/contact/resume-recording"

instance Lude.ToQuery ResumeContactRecording where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResumeContactRecordingResponse' smart constructor.
newtype ResumeContactRecordingResponse = ResumeContactRecordingResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeContactRecordingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkResumeContactRecordingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResumeContactRecordingResponse
mkResumeContactRecordingResponse pResponseStatus_ =
  ResumeContactRecordingResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' ResumeContactRecordingResponse Lude.Int
rcrrsResponseStatus = Lens.lens (responseStatus :: ResumeContactRecordingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResumeContactRecordingResponse)
{-# DEPRECATED rcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
