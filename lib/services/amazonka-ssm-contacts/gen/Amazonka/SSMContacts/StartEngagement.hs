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
-- Module      : Amazonka.SSMContacts.StartEngagement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an engagement to a contact or escalation plan. The engagement
-- engages each contact specified in the incident.
module Amazonka.SSMContacts.StartEngagement
  ( -- * Creating a Request
    StartEngagement (..),
    newStartEngagement,

    -- * Request Lenses
    startEngagement_publicContent,
    startEngagement_idempotencyToken,
    startEngagement_publicSubject,
    startEngagement_incidentId,
    startEngagement_contactId,
    startEngagement_sender,
    startEngagement_subject,
    startEngagement_content,

    -- * Destructuring the Response
    StartEngagementResponse (..),
    newStartEngagementResponse,

    -- * Response Lenses
    startEngagementResponse_httpStatus,
    startEngagementResponse_engagementArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newStartEngagement' smart constructor.
data StartEngagement = StartEngagement'
  { -- | The insecure content of the message that was sent to the contact. Use
    -- this field for engagements to @SMS@.
    publicContent :: Prelude.Maybe Prelude.Text,
    -- | A token ensuring that the operation is called only once with the
    -- specified details.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | The insecure subject of the message that was sent to the contact. Use
    -- this field for engagements to @SMS@.
    publicSubject :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the incident that the engagement is part of.
    incidentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the contact being engaged.
    contactId :: Prelude.Text,
    -- | The user that started the engagement.
    sender :: Prelude.Text,
    -- | The secure subject of the message that was sent to the contact. Use this
    -- field for engagements to @VOICE@ or @EMAIL@.
    subject :: Prelude.Text,
    -- | The secure content of the message that was sent to the contact. Use this
    -- field for engagements to @VOICE@ or @EMAIL@.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicContent', 'startEngagement_publicContent' - The insecure content of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
--
-- 'idempotencyToken', 'startEngagement_idempotencyToken' - A token ensuring that the operation is called only once with the
-- specified details.
--
-- 'publicSubject', 'startEngagement_publicSubject' - The insecure subject of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
--
-- 'incidentId', 'startEngagement_incidentId' - The ARN of the incident that the engagement is part of.
--
-- 'contactId', 'startEngagement_contactId' - The Amazon Resource Name (ARN) of the contact being engaged.
--
-- 'sender', 'startEngagement_sender' - The user that started the engagement.
--
-- 'subject', 'startEngagement_subject' - The secure subject of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ or @EMAIL@.
--
-- 'content', 'startEngagement_content' - The secure content of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ or @EMAIL@.
newStartEngagement ::
  -- | 'contactId'
  Prelude.Text ->
  -- | 'sender'
  Prelude.Text ->
  -- | 'subject'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  StartEngagement
newStartEngagement
  pContactId_
  pSender_
  pSubject_
  pContent_ =
    StartEngagement'
      { publicContent = Prelude.Nothing,
        idempotencyToken = Prelude.Nothing,
        publicSubject = Prelude.Nothing,
        incidentId = Prelude.Nothing,
        contactId = pContactId_,
        sender = pSender_,
        subject = pSubject_,
        content = pContent_
      }

-- | The insecure content of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
startEngagement_publicContent :: Lens.Lens' StartEngagement (Prelude.Maybe Prelude.Text)
startEngagement_publicContent = Lens.lens (\StartEngagement' {publicContent} -> publicContent) (\s@StartEngagement' {} a -> s {publicContent = a} :: StartEngagement)

-- | A token ensuring that the operation is called only once with the
-- specified details.
startEngagement_idempotencyToken :: Lens.Lens' StartEngagement (Prelude.Maybe Prelude.Text)
startEngagement_idempotencyToken = Lens.lens (\StartEngagement' {idempotencyToken} -> idempotencyToken) (\s@StartEngagement' {} a -> s {idempotencyToken = a} :: StartEngagement)

-- | The insecure subject of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
startEngagement_publicSubject :: Lens.Lens' StartEngagement (Prelude.Maybe Prelude.Text)
startEngagement_publicSubject = Lens.lens (\StartEngagement' {publicSubject} -> publicSubject) (\s@StartEngagement' {} a -> s {publicSubject = a} :: StartEngagement)

-- | The ARN of the incident that the engagement is part of.
startEngagement_incidentId :: Lens.Lens' StartEngagement (Prelude.Maybe Prelude.Text)
startEngagement_incidentId = Lens.lens (\StartEngagement' {incidentId} -> incidentId) (\s@StartEngagement' {} a -> s {incidentId = a} :: StartEngagement)

-- | The Amazon Resource Name (ARN) of the contact being engaged.
startEngagement_contactId :: Lens.Lens' StartEngagement Prelude.Text
startEngagement_contactId = Lens.lens (\StartEngagement' {contactId} -> contactId) (\s@StartEngagement' {} a -> s {contactId = a} :: StartEngagement)

-- | The user that started the engagement.
startEngagement_sender :: Lens.Lens' StartEngagement Prelude.Text
startEngagement_sender = Lens.lens (\StartEngagement' {sender} -> sender) (\s@StartEngagement' {} a -> s {sender = a} :: StartEngagement)

-- | The secure subject of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ or @EMAIL@.
startEngagement_subject :: Lens.Lens' StartEngagement Prelude.Text
startEngagement_subject = Lens.lens (\StartEngagement' {subject} -> subject) (\s@StartEngagement' {} a -> s {subject = a} :: StartEngagement)

-- | The secure content of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ or @EMAIL@.
startEngagement_content :: Lens.Lens' StartEngagement Prelude.Text
startEngagement_content = Lens.lens (\StartEngagement' {content} -> content) (\s@StartEngagement' {} a -> s {content = a} :: StartEngagement)

instance Core.AWSRequest StartEngagement where
  type
    AWSResponse StartEngagement =
      StartEngagementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEngagementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "EngagementArn")
      )

instance Prelude.Hashable StartEngagement where
  hashWithSalt _salt StartEngagement' {..} =
    _salt `Prelude.hashWithSalt` publicContent
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` publicSubject
      `Prelude.hashWithSalt` incidentId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` sender
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` content

instance Prelude.NFData StartEngagement where
  rnf StartEngagement' {..} =
    Prelude.rnf publicContent
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf publicSubject
      `Prelude.seq` Prelude.rnf incidentId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf sender
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf content

instance Core.ToHeaders StartEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SSMContacts.StartEngagement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartEngagement where
  toJSON StartEngagement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PublicContent" Core..=) Prelude.<$> publicContent,
            ("IdempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("PublicSubject" Core..=) Prelude.<$> publicSubject,
            ("IncidentId" Core..=) Prelude.<$> incidentId,
            Prelude.Just ("ContactId" Core..= contactId),
            Prelude.Just ("Sender" Core..= sender),
            Prelude.Just ("Subject" Core..= subject),
            Prelude.Just ("Content" Core..= content)
          ]
      )

instance Core.ToPath StartEngagement where
  toPath = Prelude.const "/"

instance Core.ToQuery StartEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEngagementResponse' smart constructor.
data StartEngagementResponse = StartEngagementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the engagement.
    engagementArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEngagementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startEngagementResponse_httpStatus' - The response's http status code.
--
-- 'engagementArn', 'startEngagementResponse_engagementArn' - The ARN of the engagement.
newStartEngagementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'engagementArn'
  Prelude.Text ->
  StartEngagementResponse
newStartEngagementResponse
  pHttpStatus_
  pEngagementArn_ =
    StartEngagementResponse'
      { httpStatus = pHttpStatus_,
        engagementArn = pEngagementArn_
      }

-- | The response's http status code.
startEngagementResponse_httpStatus :: Lens.Lens' StartEngagementResponse Prelude.Int
startEngagementResponse_httpStatus = Lens.lens (\StartEngagementResponse' {httpStatus} -> httpStatus) (\s@StartEngagementResponse' {} a -> s {httpStatus = a} :: StartEngagementResponse)

-- | The ARN of the engagement.
startEngagementResponse_engagementArn :: Lens.Lens' StartEngagementResponse Prelude.Text
startEngagementResponse_engagementArn = Lens.lens (\StartEngagementResponse' {engagementArn} -> engagementArn) (\s@StartEngagementResponse' {} a -> s {engagementArn = a} :: StartEngagementResponse)

instance Prelude.NFData StartEngagementResponse where
  rnf StartEngagementResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf engagementArn
