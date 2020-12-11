{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StopContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, this API stops recording the call. StopContactRecording is a one-time action. If you use StopContactRecording to stop recording an ongoing call, you can't use StartContactRecording to restart it. For scenarios where the recording has started and you want to suspend it for sensitive information (for example, to collect a credit card number), and then restart it, use SuspendContactRecording and ResumeContactRecording.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.StopContactRecording
  ( -- * Creating a request
    StopContactRecording (..),
    mkStopContactRecording,

    -- ** Request lenses
    stoInstanceId,
    stoContactId,
    stoInitialContactId,

    -- * Destructuring the response
    StopContactRecordingResponse (..),
    mkStopContactRecordingResponse,

    -- ** Response lenses
    scrrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopContactRecording' smart constructor.
data StopContactRecording = StopContactRecording'
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

-- | Creates a value of 'StopContactRecording' with the minimum fields required to make a request.
--
-- * 'contactId' - The identifier of the contact.
-- * 'initialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkStopContactRecording ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactId'
  Lude.Text ->
  -- | 'initialContactId'
  Lude.Text ->
  StopContactRecording
mkStopContactRecording pInstanceId_ pContactId_ pInitialContactId_ =
  StopContactRecording'
    { instanceId = pInstanceId_,
      contactId = pContactId_,
      initialContactId = pInitialContactId_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stoInstanceId :: Lens.Lens' StopContactRecording Lude.Text
stoInstanceId = Lens.lens (instanceId :: StopContactRecording -> Lude.Text) (\s a -> s {instanceId = a} :: StopContactRecording)
{-# DEPRECATED stoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stoContactId :: Lens.Lens' StopContactRecording Lude.Text
stoContactId = Lens.lens (contactId :: StopContactRecording -> Lude.Text) (\s a -> s {contactId = a} :: StopContactRecording)
{-# DEPRECATED stoContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stoInitialContactId :: Lens.Lens' StopContactRecording Lude.Text
stoInitialContactId = Lens.lens (initialContactId :: StopContactRecording -> Lude.Text) (\s a -> s {initialContactId = a} :: StopContactRecording)
{-# DEPRECATED stoInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

instance Lude.AWSRequest StopContactRecording where
  type Rs StopContactRecording = StopContactRecordingResponse
  request = Req.postJSON connectService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopContactRecordingResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopContactRecording where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopContactRecording where
  toJSON StopContactRecording' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("ContactId" Lude..= contactId),
            Lude.Just ("InitialContactId" Lude..= initialContactId)
          ]
      )

instance Lude.ToPath StopContactRecording where
  toPath = Lude.const "/contact/stop-recording"

instance Lude.ToQuery StopContactRecording where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopContactRecordingResponse' smart constructor.
newtype StopContactRecordingResponse = StopContactRecordingResponse'
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

-- | Creates a value of 'StopContactRecordingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopContactRecordingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopContactRecordingResponse
mkStopContactRecordingResponse pResponseStatus_ =
  StopContactRecordingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' StopContactRecordingResponse Lude.Int
scrrsResponseStatus = Lens.lens (responseStatus :: StopContactRecordingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopContactRecordingResponse)
{-# DEPRECATED scrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
