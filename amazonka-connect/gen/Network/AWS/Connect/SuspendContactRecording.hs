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
-- Module      : Network.AWS.Connect.SuspendContactRecording
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, this API suspends recording the call.
-- For example, you might suspend the call recording while collecting
-- sensitive information, such as a credit card number. Then use
-- ResumeContactRecording to restart recording.
--
-- The period of time that the recording is suspended is filled with
-- silence in the final recording.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.SuspendContactRecording
  ( -- * Creating a Request
    SuspendContactRecording (..),
    newSuspendContactRecording,

    -- * Request Lenses
    suspendContactRecording_instanceId,
    suspendContactRecording_contactId,
    suspendContactRecording_initialContactId,

    -- * Destructuring the Response
    SuspendContactRecordingResponse (..),
    newSuspendContactRecordingResponse,

    -- * Response Lenses
    suspendContactRecordingResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSuspendContactRecording' smart constructor.
data SuspendContactRecording = SuspendContactRecording'
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
-- Create a value of 'SuspendContactRecording' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'suspendContactRecording_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactId', 'suspendContactRecording_contactId' - The identifier of the contact.
--
-- 'initialContactId', 'suspendContactRecording_initialContactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
newSuspendContactRecording ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'initialContactId'
  Prelude.Text ->
  SuspendContactRecording
newSuspendContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_ =
    SuspendContactRecording'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance.
suspendContactRecording_instanceId :: Lens.Lens' SuspendContactRecording Prelude.Text
suspendContactRecording_instanceId = Lens.lens (\SuspendContactRecording' {instanceId} -> instanceId) (\s@SuspendContactRecording' {} a -> s {instanceId = a} :: SuspendContactRecording)

-- | The identifier of the contact.
suspendContactRecording_contactId :: Lens.Lens' SuspendContactRecording Prelude.Text
suspendContactRecording_contactId = Lens.lens (\SuspendContactRecording' {contactId} -> contactId) (\s@SuspendContactRecording' {} a -> s {contactId = a} :: SuspendContactRecording)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
suspendContactRecording_initialContactId :: Lens.Lens' SuspendContactRecording Prelude.Text
suspendContactRecording_initialContactId = Lens.lens (\SuspendContactRecording' {initialContactId} -> initialContactId) (\s@SuspendContactRecording' {} a -> s {initialContactId = a} :: SuspendContactRecording)

instance Prelude.AWSRequest SuspendContactRecording where
  type
    Rs SuspendContactRecording =
      SuspendContactRecordingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SuspendContactRecordingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SuspendContactRecording

instance Prelude.NFData SuspendContactRecording

instance Prelude.ToHeaders SuspendContactRecording where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SuspendContactRecording where
  toJSON SuspendContactRecording' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Prelude..= instanceId),
            Prelude.Just ("ContactId" Prelude..= contactId),
            Prelude.Just
              ("InitialContactId" Prelude..= initialContactId)
          ]
      )

instance Prelude.ToPath SuspendContactRecording where
  toPath = Prelude.const "/contact/suspend-recording"

instance Prelude.ToQuery SuspendContactRecording where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSuspendContactRecordingResponse' smart constructor.
data SuspendContactRecordingResponse = SuspendContactRecordingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SuspendContactRecordingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'suspendContactRecordingResponse_httpStatus' - The response's http status code.
newSuspendContactRecordingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SuspendContactRecordingResponse
newSuspendContactRecordingResponse pHttpStatus_ =
  SuspendContactRecordingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
suspendContactRecordingResponse_httpStatus :: Lens.Lens' SuspendContactRecordingResponse Prelude.Int
suspendContactRecordingResponse_httpStatus = Lens.lens (\SuspendContactRecordingResponse' {httpStatus} -> httpStatus) (\s@SuspendContactRecordingResponse' {} a -> s {httpStatus = a} :: SuspendContactRecordingResponse)

instance
  Prelude.NFData
    SuspendContactRecordingResponse
