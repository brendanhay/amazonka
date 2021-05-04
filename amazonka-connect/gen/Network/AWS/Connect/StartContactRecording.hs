{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StartContactRecording
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts recording the contact when the agent joins the call.
-- StartContactRecording is a one-time action. For example, if you use
-- StopContactRecording to stop recording an ongoing call, you can\'t use
-- StartContactRecording to restart it. For scenarios where the recording
-- has started and you want to suspend and resume it, such as when
-- collecting sensitive information (for example, a credit card number),
-- use SuspendContactRecording and ResumeContactRecording.
--
-- You can use this API to override the recording behavior configured in
-- the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/set-recording-behavior.html Set recording behavior>
-- block.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.StartContactRecording
  ( -- * Creating a Request
    StartContactRecording (..),
    newStartContactRecording,

    -- * Request Lenses
    startContactRecording_instanceId,
    startContactRecording_contactId,
    startContactRecording_initialContactId,
    startContactRecording_voiceRecordingConfiguration,

    -- * Destructuring the Response
    StartContactRecordingResponse (..),
    newStartContactRecordingResponse,

    -- * Response Lenses
    startContactRecordingResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartContactRecording' smart constructor.
data StartContactRecording = StartContactRecording'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    initialContactId :: Prelude.Text,
    -- | The person being recorded.
    voiceRecordingConfiguration :: VoiceRecordingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartContactRecording' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'startContactRecording_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactId', 'startContactRecording_contactId' - The identifier of the contact.
--
-- 'initialContactId', 'startContactRecording_initialContactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
--
-- 'voiceRecordingConfiguration', 'startContactRecording_voiceRecordingConfiguration' - The person being recorded.
newStartContactRecording ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'initialContactId'
  Prelude.Text ->
  -- | 'voiceRecordingConfiguration'
  VoiceRecordingConfiguration ->
  StartContactRecording
newStartContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_
  pVoiceRecordingConfiguration_ =
    StartContactRecording'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_,
        voiceRecordingConfiguration =
          pVoiceRecordingConfiguration_
      }

-- | The identifier of the Amazon Connect instance.
startContactRecording_instanceId :: Lens.Lens' StartContactRecording Prelude.Text
startContactRecording_instanceId = Lens.lens (\StartContactRecording' {instanceId} -> instanceId) (\s@StartContactRecording' {} a -> s {instanceId = a} :: StartContactRecording)

-- | The identifier of the contact.
startContactRecording_contactId :: Lens.Lens' StartContactRecording Prelude.Text
startContactRecording_contactId = Lens.lens (\StartContactRecording' {contactId} -> contactId) (\s@StartContactRecording' {} a -> s {contactId = a} :: StartContactRecording)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
startContactRecording_initialContactId :: Lens.Lens' StartContactRecording Prelude.Text
startContactRecording_initialContactId = Lens.lens (\StartContactRecording' {initialContactId} -> initialContactId) (\s@StartContactRecording' {} a -> s {initialContactId = a} :: StartContactRecording)

-- | The person being recorded.
startContactRecording_voiceRecordingConfiguration :: Lens.Lens' StartContactRecording VoiceRecordingConfiguration
startContactRecording_voiceRecordingConfiguration = Lens.lens (\StartContactRecording' {voiceRecordingConfiguration} -> voiceRecordingConfiguration) (\s@StartContactRecording' {} a -> s {voiceRecordingConfiguration = a} :: StartContactRecording)

instance Prelude.AWSRequest StartContactRecording where
  type
    Rs StartContactRecording =
      StartContactRecordingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartContactRecordingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartContactRecording

instance Prelude.NFData StartContactRecording

instance Prelude.ToHeaders StartContactRecording where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartContactRecording where
  toJSON StartContactRecording' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("ContactId" Prelude..= contactId),
            Prelude.Just
              ("InitialContactId" Prelude..= initialContactId),
            Prelude.Just
              ( "VoiceRecordingConfiguration"
                  Prelude..= voiceRecordingConfiguration
              )
          ]
      )

instance Prelude.ToPath StartContactRecording where
  toPath = Prelude.const "/contact/start-recording"

instance Prelude.ToQuery StartContactRecording where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContactRecordingResponse' smart constructor.
data StartContactRecordingResponse = StartContactRecordingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartContactRecordingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startContactRecordingResponse_httpStatus' - The response's http status code.
newStartContactRecordingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartContactRecordingResponse
newStartContactRecordingResponse pHttpStatus_ =
  StartContactRecordingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startContactRecordingResponse_httpStatus :: Lens.Lens' StartContactRecordingResponse Prelude.Int
startContactRecordingResponse_httpStatus = Lens.lens (\StartContactRecordingResponse' {httpStatus} -> httpStatus) (\s@StartContactRecordingResponse' {} a -> s {httpStatus = a} :: StartContactRecordingResponse)

instance Prelude.NFData StartContactRecordingResponse
