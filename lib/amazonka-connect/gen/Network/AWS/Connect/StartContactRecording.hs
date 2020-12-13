{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StartContactRecording
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API starts recording the contact when the agent joins the call. StartContactRecording is a one-time action. For example, if you use StopContactRecording to stop recording an ongoing call, you can't use StartContactRecording to restart it. For scenarios where the recording has started and you want to suspend and resume it, such as when collecting sensitive information (for example, a credit card number), use SuspendContactRecording and ResumeContactRecording.
--
-- You can use this API to override the recording behavior configured in the <https://docs.aws.amazon.com/connect/latest/adminguide/set-recording-behavior.html Set recording behavior> block.
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.StartContactRecording
  ( -- * Creating a request
    StartContactRecording (..),
    mkStartContactRecording,

    -- ** Request lenses
    scrInstanceId,
    scrVoiceRecordingConfiguration,
    scrContactId,
    scrInitialContactId,

    -- * Destructuring the response
    StartContactRecordingResponse (..),
    mkStartContactRecordingResponse,

    -- ** Response lenses
    scrfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartContactRecording' smart constructor.
data StartContactRecording = StartContactRecording'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | Who is being recorded.
    voiceRecordingConfiguration :: VoiceRecordingConfiguration,
    -- | The identifier of the contact.
    contactId :: Lude.Text,
    -- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
    initialContactId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartContactRecording' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'voiceRecordingConfiguration' - Who is being recorded.
-- * 'contactId' - The identifier of the contact.
-- * 'initialContactId' - The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
mkStartContactRecording ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'voiceRecordingConfiguration'
  VoiceRecordingConfiguration ->
  -- | 'contactId'
  Lude.Text ->
  -- | 'initialContactId'
  Lude.Text ->
  StartContactRecording
mkStartContactRecording
  pInstanceId_
  pVoiceRecordingConfiguration_
  pContactId_
  pInitialContactId_ =
    StartContactRecording'
      { instanceId = pInstanceId_,
        voiceRecordingConfiguration = pVoiceRecordingConfiguration_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrInstanceId :: Lens.Lens' StartContactRecording Lude.Text
scrInstanceId = Lens.lens (instanceId :: StartContactRecording -> Lude.Text) (\s a -> s {instanceId = a} :: StartContactRecording)
{-# DEPRECATED scrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Who is being recorded.
--
-- /Note:/ Consider using 'voiceRecordingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrVoiceRecordingConfiguration :: Lens.Lens' StartContactRecording VoiceRecordingConfiguration
scrVoiceRecordingConfiguration = Lens.lens (voiceRecordingConfiguration :: StartContactRecording -> VoiceRecordingConfiguration) (\s a -> s {voiceRecordingConfiguration = a} :: StartContactRecording)
{-# DEPRECATED scrVoiceRecordingConfiguration "Use generic-lens or generic-optics with 'voiceRecordingConfiguration' instead." #-}

-- | The identifier of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrContactId :: Lens.Lens' StartContactRecording Lude.Text
scrContactId = Lens.lens (contactId :: StartContactRecording -> Lude.Text) (\s a -> s {contactId = a} :: StartContactRecording)
{-# DEPRECATED scrContactId "Use generic-lens or generic-optics with 'contactId' instead." #-}

-- | The identifier of the contact. This is the identifier of the contact associated with the first interaction with the contact center.
--
-- /Note:/ Consider using 'initialContactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrInitialContactId :: Lens.Lens' StartContactRecording Lude.Text
scrInitialContactId = Lens.lens (initialContactId :: StartContactRecording -> Lude.Text) (\s a -> s {initialContactId = a} :: StartContactRecording)
{-# DEPRECATED scrInitialContactId "Use generic-lens or generic-optics with 'initialContactId' instead." #-}

instance Lude.AWSRequest StartContactRecording where
  type Rs StartContactRecording = StartContactRecordingResponse
  request = Req.postJSON connectService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartContactRecordingResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartContactRecording where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartContactRecording where
  toJSON StartContactRecording' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just
              ( "VoiceRecordingConfiguration"
                  Lude..= voiceRecordingConfiguration
              ),
            Lude.Just ("ContactId" Lude..= contactId),
            Lude.Just ("InitialContactId" Lude..= initialContactId)
          ]
      )

instance Lude.ToPath StartContactRecording where
  toPath = Lude.const "/contact/start-recording"

instance Lude.ToQuery StartContactRecording where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartContactRecordingResponse' smart constructor.
newtype StartContactRecordingResponse = StartContactRecordingResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartContactRecordingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartContactRecordingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartContactRecordingResponse
mkStartContactRecordingResponse pResponseStatus_ =
  StartContactRecordingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrfrsResponseStatus :: Lens.Lens' StartContactRecordingResponse Lude.Int
scrfrsResponseStatus = Lens.lens (responseStatus :: StartContactRecordingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartContactRecordingResponse)
{-# DEPRECATED scrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
