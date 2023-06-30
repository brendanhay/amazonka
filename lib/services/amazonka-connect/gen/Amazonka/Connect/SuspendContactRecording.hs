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
-- Module      : Amazonka.Connect.SuspendContactRecording
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Connect.SuspendContactRecording
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSuspendContactRecording' smart constructor.
data SuspendContactRecording = SuspendContactRecording'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    initialContactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuspendContactRecording' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'suspendContactRecording_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
suspendContactRecording_instanceId :: Lens.Lens' SuspendContactRecording Prelude.Text
suspendContactRecording_instanceId = Lens.lens (\SuspendContactRecording' {instanceId} -> instanceId) (\s@SuspendContactRecording' {} a -> s {instanceId = a} :: SuspendContactRecording)

-- | The identifier of the contact.
suspendContactRecording_contactId :: Lens.Lens' SuspendContactRecording Prelude.Text
suspendContactRecording_contactId = Lens.lens (\SuspendContactRecording' {contactId} -> contactId) (\s@SuspendContactRecording' {} a -> s {contactId = a} :: SuspendContactRecording)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
suspendContactRecording_initialContactId :: Lens.Lens' SuspendContactRecording Prelude.Text
suspendContactRecording_initialContactId = Lens.lens (\SuspendContactRecording' {initialContactId} -> initialContactId) (\s@SuspendContactRecording' {} a -> s {initialContactId = a} :: SuspendContactRecording)

instance Core.AWSRequest SuspendContactRecording where
  type
    AWSResponse SuspendContactRecording =
      SuspendContactRecordingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SuspendContactRecordingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SuspendContactRecording where
  hashWithSalt _salt SuspendContactRecording' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` initialContactId

instance Prelude.NFData SuspendContactRecording where
  rnf SuspendContactRecording' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf initialContactId

instance Data.ToHeaders SuspendContactRecording where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SuspendContactRecording where
  toJSON SuspendContactRecording' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just
              ("InitialContactId" Data..= initialContactId)
          ]
      )

instance Data.ToPath SuspendContactRecording where
  toPath = Prelude.const "/contact/suspend-recording"

instance Data.ToQuery SuspendContactRecording where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSuspendContactRecordingResponse' smart constructor.
data SuspendContactRecordingResponse = SuspendContactRecordingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf SuspendContactRecordingResponse' {..} =
    Prelude.rnf httpStatus
