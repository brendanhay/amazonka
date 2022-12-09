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
-- Module      : Amazonka.SSMContacts.DescribeEngagement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Incident Manager uses engagements to engage contacts and escalation
-- plans during an incident. Use this command to describe the engagement
-- that occurred during an incident.
module Amazonka.SSMContacts.DescribeEngagement
  ( -- * Creating a Request
    DescribeEngagement (..),
    newDescribeEngagement,

    -- * Request Lenses
    describeEngagement_engagementId,

    -- * Destructuring the Response
    DescribeEngagementResponse (..),
    newDescribeEngagementResponse,

    -- * Response Lenses
    describeEngagementResponse_incidentId,
    describeEngagementResponse_publicContent,
    describeEngagementResponse_publicSubject,
    describeEngagementResponse_startTime,
    describeEngagementResponse_stopTime,
    describeEngagementResponse_httpStatus,
    describeEngagementResponse_contactArn,
    describeEngagementResponse_engagementArn,
    describeEngagementResponse_sender,
    describeEngagementResponse_subject,
    describeEngagementResponse_content,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newDescribeEngagement' smart constructor.
data DescribeEngagement = DescribeEngagement'
  { -- | The Amazon Resource Name (ARN) of the engagement you want the details
    -- of.
    engagementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engagementId', 'describeEngagement_engagementId' - The Amazon Resource Name (ARN) of the engagement you want the details
-- of.
newDescribeEngagement ::
  -- | 'engagementId'
  Prelude.Text ->
  DescribeEngagement
newDescribeEngagement pEngagementId_ =
  DescribeEngagement' {engagementId = pEngagementId_}

-- | The Amazon Resource Name (ARN) of the engagement you want the details
-- of.
describeEngagement_engagementId :: Lens.Lens' DescribeEngagement Prelude.Text
describeEngagement_engagementId = Lens.lens (\DescribeEngagement' {engagementId} -> engagementId) (\s@DescribeEngagement' {} a -> s {engagementId = a} :: DescribeEngagement)

instance Core.AWSRequest DescribeEngagement where
  type
    AWSResponse DescribeEngagement =
      DescribeEngagementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEngagementResponse'
            Prelude.<$> (x Data..?> "IncidentId")
            Prelude.<*> (x Data..?> "PublicContent")
            Prelude.<*> (x Data..?> "PublicSubject")
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (x Data..?> "StopTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ContactArn")
            Prelude.<*> (x Data..:> "EngagementArn")
            Prelude.<*> (x Data..:> "Sender")
            Prelude.<*> (x Data..:> "Subject")
            Prelude.<*> (x Data..:> "Content")
      )

instance Prelude.Hashable DescribeEngagement where
  hashWithSalt _salt DescribeEngagement' {..} =
    _salt `Prelude.hashWithSalt` engagementId

instance Prelude.NFData DescribeEngagement where
  rnf DescribeEngagement' {..} =
    Prelude.rnf engagementId

instance Data.ToHeaders DescribeEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.DescribeEngagement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEngagement where
  toJSON DescribeEngagement' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("EngagementId" Data..= engagementId)]
      )

instance Data.ToPath DescribeEngagement where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEngagementResponse' smart constructor.
data DescribeEngagementResponse = DescribeEngagementResponse'
  { -- | The ARN of the incident in which the engagement occurred.
    incidentId :: Prelude.Maybe Prelude.Text,
    -- | The insecure content of the message that was sent to the contact. Use
    -- this field for engagements to @SMS@.
    publicContent :: Prelude.Maybe Prelude.Text,
    -- | The insecure subject of the message that was sent to the contact. Use
    -- this field for engagements to @SMS@.
    publicSubject :: Prelude.Maybe Prelude.Text,
    -- | The time that the engagement started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The time that the engagement ended.
    stopTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the escalation plan or contacts involved in the engagement.
    contactArn :: Prelude.Text,
    -- | The ARN of the engagement.
    engagementArn :: Prelude.Text,
    -- | The user that started the engagement.
    sender :: Prelude.Text,
    -- | The secure subject of the message that was sent to the contact. Use this
    -- field for engagements to @VOICE@ and @EMAIL@.
    subject :: Prelude.Text,
    -- | The secure content of the message that was sent to the contact. Use this
    -- field for engagements to @VOICE@ and @EMAIL@.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEngagementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'incidentId', 'describeEngagementResponse_incidentId' - The ARN of the incident in which the engagement occurred.
--
-- 'publicContent', 'describeEngagementResponse_publicContent' - The insecure content of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
--
-- 'publicSubject', 'describeEngagementResponse_publicSubject' - The insecure subject of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
--
-- 'startTime', 'describeEngagementResponse_startTime' - The time that the engagement started.
--
-- 'stopTime', 'describeEngagementResponse_stopTime' - The time that the engagement ended.
--
-- 'httpStatus', 'describeEngagementResponse_httpStatus' - The response's http status code.
--
-- 'contactArn', 'describeEngagementResponse_contactArn' - The ARN of the escalation plan or contacts involved in the engagement.
--
-- 'engagementArn', 'describeEngagementResponse_engagementArn' - The ARN of the engagement.
--
-- 'sender', 'describeEngagementResponse_sender' - The user that started the engagement.
--
-- 'subject', 'describeEngagementResponse_subject' - The secure subject of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
--
-- 'content', 'describeEngagementResponse_content' - The secure content of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
newDescribeEngagementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'engagementArn'
  Prelude.Text ->
  -- | 'sender'
  Prelude.Text ->
  -- | 'subject'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  DescribeEngagementResponse
newDescribeEngagementResponse
  pHttpStatus_
  pContactArn_
  pEngagementArn_
  pSender_
  pSubject_
  pContent_ =
    DescribeEngagementResponse'
      { incidentId =
          Prelude.Nothing,
        publicContent = Prelude.Nothing,
        publicSubject = Prelude.Nothing,
        startTime = Prelude.Nothing,
        stopTime = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        contactArn = pContactArn_,
        engagementArn = pEngagementArn_,
        sender = pSender_,
        subject = pSubject_,
        content = pContent_
      }

-- | The ARN of the incident in which the engagement occurred.
describeEngagementResponse_incidentId :: Lens.Lens' DescribeEngagementResponse (Prelude.Maybe Prelude.Text)
describeEngagementResponse_incidentId = Lens.lens (\DescribeEngagementResponse' {incidentId} -> incidentId) (\s@DescribeEngagementResponse' {} a -> s {incidentId = a} :: DescribeEngagementResponse)

-- | The insecure content of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
describeEngagementResponse_publicContent :: Lens.Lens' DescribeEngagementResponse (Prelude.Maybe Prelude.Text)
describeEngagementResponse_publicContent = Lens.lens (\DescribeEngagementResponse' {publicContent} -> publicContent) (\s@DescribeEngagementResponse' {} a -> s {publicContent = a} :: DescribeEngagementResponse)

-- | The insecure subject of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
describeEngagementResponse_publicSubject :: Lens.Lens' DescribeEngagementResponse (Prelude.Maybe Prelude.Text)
describeEngagementResponse_publicSubject = Lens.lens (\DescribeEngagementResponse' {publicSubject} -> publicSubject) (\s@DescribeEngagementResponse' {} a -> s {publicSubject = a} :: DescribeEngagementResponse)

-- | The time that the engagement started.
describeEngagementResponse_startTime :: Lens.Lens' DescribeEngagementResponse (Prelude.Maybe Prelude.UTCTime)
describeEngagementResponse_startTime = Lens.lens (\DescribeEngagementResponse' {startTime} -> startTime) (\s@DescribeEngagementResponse' {} a -> s {startTime = a} :: DescribeEngagementResponse) Prelude.. Lens.mapping Data._Time

-- | The time that the engagement ended.
describeEngagementResponse_stopTime :: Lens.Lens' DescribeEngagementResponse (Prelude.Maybe Prelude.UTCTime)
describeEngagementResponse_stopTime = Lens.lens (\DescribeEngagementResponse' {stopTime} -> stopTime) (\s@DescribeEngagementResponse' {} a -> s {stopTime = a} :: DescribeEngagementResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeEngagementResponse_httpStatus :: Lens.Lens' DescribeEngagementResponse Prelude.Int
describeEngagementResponse_httpStatus = Lens.lens (\DescribeEngagementResponse' {httpStatus} -> httpStatus) (\s@DescribeEngagementResponse' {} a -> s {httpStatus = a} :: DescribeEngagementResponse)

-- | The ARN of the escalation plan or contacts involved in the engagement.
describeEngagementResponse_contactArn :: Lens.Lens' DescribeEngagementResponse Prelude.Text
describeEngagementResponse_contactArn = Lens.lens (\DescribeEngagementResponse' {contactArn} -> contactArn) (\s@DescribeEngagementResponse' {} a -> s {contactArn = a} :: DescribeEngagementResponse)

-- | The ARN of the engagement.
describeEngagementResponse_engagementArn :: Lens.Lens' DescribeEngagementResponse Prelude.Text
describeEngagementResponse_engagementArn = Lens.lens (\DescribeEngagementResponse' {engagementArn} -> engagementArn) (\s@DescribeEngagementResponse' {} a -> s {engagementArn = a} :: DescribeEngagementResponse)

-- | The user that started the engagement.
describeEngagementResponse_sender :: Lens.Lens' DescribeEngagementResponse Prelude.Text
describeEngagementResponse_sender = Lens.lens (\DescribeEngagementResponse' {sender} -> sender) (\s@DescribeEngagementResponse' {} a -> s {sender = a} :: DescribeEngagementResponse)

-- | The secure subject of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
describeEngagementResponse_subject :: Lens.Lens' DescribeEngagementResponse Prelude.Text
describeEngagementResponse_subject = Lens.lens (\DescribeEngagementResponse' {subject} -> subject) (\s@DescribeEngagementResponse' {} a -> s {subject = a} :: DescribeEngagementResponse)

-- | The secure content of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
describeEngagementResponse_content :: Lens.Lens' DescribeEngagementResponse Prelude.Text
describeEngagementResponse_content = Lens.lens (\DescribeEngagementResponse' {content} -> content) (\s@DescribeEngagementResponse' {} a -> s {content = a} :: DescribeEngagementResponse)

instance Prelude.NFData DescribeEngagementResponse where
  rnf DescribeEngagementResponse' {..} =
    Prelude.rnf incidentId
      `Prelude.seq` Prelude.rnf publicContent
      `Prelude.seq` Prelude.rnf publicSubject
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf engagementArn
      `Prelude.seq` Prelude.rnf sender
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf content
