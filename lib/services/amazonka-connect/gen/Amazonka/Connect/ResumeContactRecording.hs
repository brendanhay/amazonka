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
-- Module      : Amazonka.Connect.ResumeContactRecording
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, and the recording has been suspended
-- using SuspendContactRecording, this API resumes recording the call.
--
-- Only voice recordings are supported at this time.
module Amazonka.Connect.ResumeContactRecording
  ( -- * Creating a Request
    ResumeContactRecording (..),
    newResumeContactRecording,

    -- * Request Lenses
    resumeContactRecording_instanceId,
    resumeContactRecording_contactId,
    resumeContactRecording_initialContactId,

    -- * Destructuring the Response
    ResumeContactRecordingResponse (..),
    newResumeContactRecordingResponse,

    -- * Response Lenses
    resumeContactRecordingResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResumeContactRecording' smart constructor.
data ResumeContactRecording = ResumeContactRecording'
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
-- Create a value of 'ResumeContactRecording' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'resumeContactRecording_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'resumeContactRecording_contactId' - The identifier of the contact.
--
-- 'initialContactId', 'resumeContactRecording_initialContactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
newResumeContactRecording ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'initialContactId'
  Prelude.Text ->
  ResumeContactRecording
newResumeContactRecording
  pInstanceId_
  pContactId_
  pInitialContactId_ =
    ResumeContactRecording'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
resumeContactRecording_instanceId :: Lens.Lens' ResumeContactRecording Prelude.Text
resumeContactRecording_instanceId = Lens.lens (\ResumeContactRecording' {instanceId} -> instanceId) (\s@ResumeContactRecording' {} a -> s {instanceId = a} :: ResumeContactRecording)

-- | The identifier of the contact.
resumeContactRecording_contactId :: Lens.Lens' ResumeContactRecording Prelude.Text
resumeContactRecording_contactId = Lens.lens (\ResumeContactRecording' {contactId} -> contactId) (\s@ResumeContactRecording' {} a -> s {contactId = a} :: ResumeContactRecording)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
resumeContactRecording_initialContactId :: Lens.Lens' ResumeContactRecording Prelude.Text
resumeContactRecording_initialContactId = Lens.lens (\ResumeContactRecording' {initialContactId} -> initialContactId) (\s@ResumeContactRecording' {} a -> s {initialContactId = a} :: ResumeContactRecording)

instance Core.AWSRequest ResumeContactRecording where
  type
    AWSResponse ResumeContactRecording =
      ResumeContactRecordingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResumeContactRecordingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeContactRecording where
  hashWithSalt _salt ResumeContactRecording' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` initialContactId

instance Prelude.NFData ResumeContactRecording where
  rnf ResumeContactRecording' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf initialContactId

instance Core.ToHeaders ResumeContactRecording where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResumeContactRecording where
  toJSON ResumeContactRecording' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Core..= instanceId),
            Prelude.Just ("ContactId" Core..= contactId),
            Prelude.Just
              ("InitialContactId" Core..= initialContactId)
          ]
      )

instance Core.ToPath ResumeContactRecording where
  toPath = Prelude.const "/contact/resume-recording"

instance Core.ToQuery ResumeContactRecording where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeContactRecordingResponse' smart constructor.
data ResumeContactRecordingResponse = ResumeContactRecordingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeContactRecordingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resumeContactRecordingResponse_httpStatus' - The response's http status code.
newResumeContactRecordingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResumeContactRecordingResponse
newResumeContactRecordingResponse pHttpStatus_ =
  ResumeContactRecordingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resumeContactRecordingResponse_httpStatus :: Lens.Lens' ResumeContactRecordingResponse Prelude.Int
resumeContactRecordingResponse_httpStatus = Lens.lens (\ResumeContactRecordingResponse' {httpStatus} -> httpStatus) (\s@ResumeContactRecordingResponse' {} a -> s {httpStatus = a} :: ResumeContactRecordingResponse)

instance
  Prelude.NFData
    ResumeContactRecordingResponse
  where
  rnf ResumeContactRecordingResponse' {..} =
    Prelude.rnf httpStatus
