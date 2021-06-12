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
-- Module      : Network.AWS.Connect.ResumeContactRecording
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When a contact is being recorded, and the recording has been suspended
-- using SuspendContactRecording, this API resumes recording the call.
--
-- Only voice recordings are supported at this time.
module Network.AWS.Connect.ResumeContactRecording
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResumeContactRecording' smart constructor.
data ResumeContactRecording = ResumeContactRecording'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier of the contact.
    contactId :: Core.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with the contact center.
    initialContactId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResumeContactRecording' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'resumeContactRecording_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactId', 'resumeContactRecording_contactId' - The identifier of the contact.
--
-- 'initialContactId', 'resumeContactRecording_initialContactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
newResumeContactRecording ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'contactId'
  Core.Text ->
  -- | 'initialContactId'
  Core.Text ->
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

-- | The identifier of the Amazon Connect instance.
resumeContactRecording_instanceId :: Lens.Lens' ResumeContactRecording Core.Text
resumeContactRecording_instanceId = Lens.lens (\ResumeContactRecording' {instanceId} -> instanceId) (\s@ResumeContactRecording' {} a -> s {instanceId = a} :: ResumeContactRecording)

-- | The identifier of the contact.
resumeContactRecording_contactId :: Lens.Lens' ResumeContactRecording Core.Text
resumeContactRecording_contactId = Lens.lens (\ResumeContactRecording' {contactId} -> contactId) (\s@ResumeContactRecording' {} a -> s {contactId = a} :: ResumeContactRecording)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with the contact center.
resumeContactRecording_initialContactId :: Lens.Lens' ResumeContactRecording Core.Text
resumeContactRecording_initialContactId = Lens.lens (\ResumeContactRecording' {initialContactId} -> initialContactId) (\s@ResumeContactRecording' {} a -> s {initialContactId = a} :: ResumeContactRecording)

instance Core.AWSRequest ResumeContactRecording where
  type
    AWSResponse ResumeContactRecording =
      ResumeContactRecordingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResumeContactRecordingResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResumeContactRecording

instance Core.NFData ResumeContactRecording

instance Core.ToHeaders ResumeContactRecording where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResumeContactRecording where
  toJSON ResumeContactRecording' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("ContactId" Core..= contactId),
            Core.Just
              ("InitialContactId" Core..= initialContactId)
          ]
      )

instance Core.ToPath ResumeContactRecording where
  toPath = Core.const "/contact/resume-recording"

instance Core.ToQuery ResumeContactRecording where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newResumeContactRecordingResponse' smart constructor.
data ResumeContactRecordingResponse = ResumeContactRecordingResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ResumeContactRecordingResponse
newResumeContactRecordingResponse pHttpStatus_ =
  ResumeContactRecordingResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resumeContactRecordingResponse_httpStatus :: Lens.Lens' ResumeContactRecordingResponse Core.Int
resumeContactRecordingResponse_httpStatus = Lens.lens (\ResumeContactRecordingResponse' {httpStatus} -> httpStatus) (\s@ResumeContactRecordingResponse' {} a -> s {httpStatus = a} :: ResumeContactRecordingResponse)

instance Core.NFData ResumeContactRecordingResponse
