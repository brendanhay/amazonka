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
-- Module      : Network.AWS.Connect.StopContactRecording
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops recording a call when a contact is being recorded.
-- StopContactRecording is a one-time action. If you use
-- StopContactRecording to stop recording an ongoing call, you can\'t use
-- StartContactRecording to restart it. For scenarios where the recording
-- has started and you want to suspend it for sensitive information (for
-- example, to collect a credit card number), and then restart it, use
-- SuspendContactRecording and ResumeContactRecording.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.StopContactRecording
  ( -- * Creating a Request
    StopContactRecording (..),
    newStopContactRecording,

    -- * Request Lenses
    stopContactRecording_instanceId,
    stopContactRecording_contactId,
    stopContactRecording_initialContactId,

    -- * Destructuring the Response
    StopContactRecordingResponse (..),
    newStopContactRecordingResponse,

    -- * Response Lenses
    stopContactRecordingResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopContactRecording' smart constructor.
data StopContactRecording = StopContactRecording'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    initialContactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopContactRecording' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'stopContactRecording_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactId', 'stopContactRecording_contactId' - The identifier of the contact.
--
-- 'initialContactId', 'stopContactRecording_initialContactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
newStopContactRecording ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'initialContactId'
  Prelude.Text ->
  StopContactRecording
newStopContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_ =
    StopContactRecording'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance.
stopContactRecording_instanceId :: Lens.Lens' StopContactRecording Prelude.Text
stopContactRecording_instanceId = Lens.lens (\StopContactRecording' {instanceId} -> instanceId) (\s@StopContactRecording' {} a -> s {instanceId = a} :: StopContactRecording)

-- | The identifier of the contact.
stopContactRecording_contactId :: Lens.Lens' StopContactRecording Prelude.Text
stopContactRecording_contactId = Lens.lens (\StopContactRecording' {contactId} -> contactId) (\s@StopContactRecording' {} a -> s {contactId = a} :: StopContactRecording)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
stopContactRecording_initialContactId :: Lens.Lens' StopContactRecording Prelude.Text
stopContactRecording_initialContactId = Lens.lens (\StopContactRecording' {initialContactId} -> initialContactId) (\s@StopContactRecording' {} a -> s {initialContactId = a} :: StopContactRecording)

instance Prelude.AWSRequest StopContactRecording where
  type
    Rs StopContactRecording =
      StopContactRecordingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopContactRecordingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopContactRecording

instance Prelude.NFData StopContactRecording

instance Prelude.ToHeaders StopContactRecording where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopContactRecording where
  toJSON StopContactRecording' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("ContactId" Prelude..= contactId),
            Prelude.Just
              ("InitialContactId" Prelude..= initialContactId)
          ]
      )

instance Prelude.ToPath StopContactRecording where
  toPath = Prelude.const "/contact/stop-recording"

instance Prelude.ToQuery StopContactRecording where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopContactRecordingResponse' smart constructor.
data StopContactRecordingResponse = StopContactRecordingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopContactRecordingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopContactRecordingResponse_httpStatus' - The response's http status code.
newStopContactRecordingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopContactRecordingResponse
newStopContactRecordingResponse pHttpStatus_ =
  StopContactRecordingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopContactRecordingResponse_httpStatus :: Lens.Lens' StopContactRecordingResponse Prelude.Int
stopContactRecordingResponse_httpStatus = Lens.lens (\StopContactRecordingResponse' {httpStatus} -> httpStatus) (\s@StopContactRecordingResponse' {} a -> s {httpStatus = a} :: StopContactRecordingResponse)

instance Prelude.NFData StopContactRecordingResponse
