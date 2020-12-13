{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SuspendContactRecording (..),
    mkSuspendContactRecording,

    -- ** Request lenses
    sInstanceId,
    sContactId,
    sInitialContactId,

    -- * Destructuring the response
    SuspendContactRecordingResponse (..),
    mkSuspendContactRecordingResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSuspendContactRecording' smart constructor.
data SuspendContactRecording = SuspendContactRecording'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the contact.
    contactId :: Lude.Text,
    -- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
    initialContactId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendContactRecording' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'contactId' - The identifier of the contact.
-- * 'initialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
mkSuspendContactRecording ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactId'
  Lude.Text ->
  -- | 'initialContactId'
  Lude.Text ->
  SuspendContactRecording
mkSuspendContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_ =
    SuspendContactRecording'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceId :: Lens.Lens' SuspendContactRecording Lude.Text
sInstanceId = Lens.lens (instanceId :: SuspendContactRecording -> Lude.Text) (\s a -> s {instanceId = a} :: SuspendContactRecording)
{-# DEPRECATED sInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sContactId :: Lens.Lens' SuspendContactRecording Lude.Text
sContactId = Lens.lens (contactId :: SuspendContactRecording -> Lude.Text) (\s a -> s {contactId = a} :: SuspendContactRecording)
{-# DEPRECATED sContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInitialContactId :: Lens.Lens' SuspendContactRecording Lude.Text
sInitialContactId = Lens.lens (initialContactId :: SuspendContactRecording -> Lude.Text) (\s a -> s {initialContactId = a} :: SuspendContactRecording)
{-# DEPRECATED sInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

instance Lude.AWSRequest SuspendContactRecording where
  type Rs SuspendContactRecording = SuspendContactRecordingResponse
  request = Req.postJSON connectService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SuspendContactRecordingResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SuspendContactRecording where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SuspendContactRecording where
  toJSON SuspendContactRecording' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("ContactId" Lude..= contactId),
            Lude.Just ("InitialContactId" Lude..= initialContactId)
          ]
      )

instance Lude.ToPath SuspendContactRecording where
  toPath = Lude.const "/contact/suspend-recording"

instance Lude.ToQuery SuspendContactRecording where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSuspendContactRecordingResponse' smart constructor.
newtype SuspendContactRecordingResponse = SuspendContactRecordingResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendContactRecordingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSuspendContactRecordingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SuspendContactRecordingResponse
mkSuspendContactRecordingResponse pResponseStatus_ =
  SuspendContactRecordingResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SuspendContactRecordingResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SuspendContactRecordingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SuspendContactRecordingResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
