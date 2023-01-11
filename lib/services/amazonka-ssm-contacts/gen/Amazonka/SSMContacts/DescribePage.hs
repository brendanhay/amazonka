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
-- Module      : Amazonka.SSMContacts.DescribePage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details of the engagement to a contact channel.
module Amazonka.SSMContacts.DescribePage
  ( -- * Creating a Request
    DescribePage (..),
    newDescribePage,

    -- * Request Lenses
    describePage_pageId,

    -- * Destructuring the Response
    DescribePageResponse (..),
    newDescribePageResponse,

    -- * Response Lenses
    describePageResponse_deliveryTime,
    describePageResponse_incidentId,
    describePageResponse_publicContent,
    describePageResponse_publicSubject,
    describePageResponse_readTime,
    describePageResponse_sentTime,
    describePageResponse_httpStatus,
    describePageResponse_pageArn,
    describePageResponse_engagementArn,
    describePageResponse_contactArn,
    describePageResponse_sender,
    describePageResponse_subject,
    describePageResponse_content,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newDescribePage' smart constructor.
data DescribePage = DescribePage'
  { -- | The ID of the engagement to a contact channel.
    pageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageId', 'describePage_pageId' - The ID of the engagement to a contact channel.
newDescribePage ::
  -- | 'pageId'
  Prelude.Text ->
  DescribePage
newDescribePage pPageId_ =
  DescribePage' {pageId = pPageId_}

-- | The ID of the engagement to a contact channel.
describePage_pageId :: Lens.Lens' DescribePage Prelude.Text
describePage_pageId = Lens.lens (\DescribePage' {pageId} -> pageId) (\s@DescribePage' {} a -> s {pageId = a} :: DescribePage)

instance Core.AWSRequest DescribePage where
  type AWSResponse DescribePage = DescribePageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePageResponse'
            Prelude.<$> (x Data..?> "DeliveryTime")
            Prelude.<*> (x Data..?> "IncidentId")
            Prelude.<*> (x Data..?> "PublicContent")
            Prelude.<*> (x Data..?> "PublicSubject")
            Prelude.<*> (x Data..?> "ReadTime")
            Prelude.<*> (x Data..?> "SentTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "PageArn")
            Prelude.<*> (x Data..:> "EngagementArn")
            Prelude.<*> (x Data..:> "ContactArn")
            Prelude.<*> (x Data..:> "Sender")
            Prelude.<*> (x Data..:> "Subject")
            Prelude.<*> (x Data..:> "Content")
      )

instance Prelude.Hashable DescribePage where
  hashWithSalt _salt DescribePage' {..} =
    _salt `Prelude.hashWithSalt` pageId

instance Prelude.NFData DescribePage where
  rnf DescribePage' {..} = Prelude.rnf pageId

instance Data.ToHeaders DescribePage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.DescribePage" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePage where
  toJSON DescribePage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PageId" Data..= pageId)]
      )

instance Data.ToPath DescribePage where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePageResponse' smart constructor.
data DescribePageResponse = DescribePageResponse'
  { -- | The time that the contact channel received the engagement.
    deliveryTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the incident that engaged the contact channel.
    incidentId :: Prelude.Maybe Prelude.Text,
    -- | The insecure content of the message that was sent to the contact. Use
    -- this field for engagements to @SMS@.
    publicContent :: Prelude.Maybe Prelude.Text,
    -- | The insecure subject of the message that was sent to the contact. Use
    -- this field for engagements to @SMS@.
    publicSubject :: Prelude.Maybe Prelude.Text,
    -- | The time that the contact channel acknowledged the engagement.
    readTime :: Prelude.Maybe Data.POSIX,
    -- | The time the engagement was sent to the contact channel.
    sentTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the engagement to a contact channel.
    pageArn :: Prelude.Text,
    -- | The ARN of the engagement that engaged the contact channel.
    engagementArn :: Prelude.Text,
    -- | The ARN of the contact that was engaged.
    contactArn :: Prelude.Text,
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
-- Create a value of 'DescribePageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryTime', 'describePageResponse_deliveryTime' - The time that the contact channel received the engagement.
--
-- 'incidentId', 'describePageResponse_incidentId' - The ARN of the incident that engaged the contact channel.
--
-- 'publicContent', 'describePageResponse_publicContent' - The insecure content of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
--
-- 'publicSubject', 'describePageResponse_publicSubject' - The insecure subject of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
--
-- 'readTime', 'describePageResponse_readTime' - The time that the contact channel acknowledged the engagement.
--
-- 'sentTime', 'describePageResponse_sentTime' - The time the engagement was sent to the contact channel.
--
-- 'httpStatus', 'describePageResponse_httpStatus' - The response's http status code.
--
-- 'pageArn', 'describePageResponse_pageArn' - The Amazon Resource Name (ARN) of the engagement to a contact channel.
--
-- 'engagementArn', 'describePageResponse_engagementArn' - The ARN of the engagement that engaged the contact channel.
--
-- 'contactArn', 'describePageResponse_contactArn' - The ARN of the contact that was engaged.
--
-- 'sender', 'describePageResponse_sender' - The user that started the engagement.
--
-- 'subject', 'describePageResponse_subject' - The secure subject of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
--
-- 'content', 'describePageResponse_content' - The secure content of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
newDescribePageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'pageArn'
  Prelude.Text ->
  -- | 'engagementArn'
  Prelude.Text ->
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'sender'
  Prelude.Text ->
  -- | 'subject'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  DescribePageResponse
newDescribePageResponse
  pHttpStatus_
  pPageArn_
  pEngagementArn_
  pContactArn_
  pSender_
  pSubject_
  pContent_ =
    DescribePageResponse'
      { deliveryTime =
          Prelude.Nothing,
        incidentId = Prelude.Nothing,
        publicContent = Prelude.Nothing,
        publicSubject = Prelude.Nothing,
        readTime = Prelude.Nothing,
        sentTime = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        pageArn = pPageArn_,
        engagementArn = pEngagementArn_,
        contactArn = pContactArn_,
        sender = pSender_,
        subject = pSubject_,
        content = pContent_
      }

-- | The time that the contact channel received the engagement.
describePageResponse_deliveryTime :: Lens.Lens' DescribePageResponse (Prelude.Maybe Prelude.UTCTime)
describePageResponse_deliveryTime = Lens.lens (\DescribePageResponse' {deliveryTime} -> deliveryTime) (\s@DescribePageResponse' {} a -> s {deliveryTime = a} :: DescribePageResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the incident that engaged the contact channel.
describePageResponse_incidentId :: Lens.Lens' DescribePageResponse (Prelude.Maybe Prelude.Text)
describePageResponse_incidentId = Lens.lens (\DescribePageResponse' {incidentId} -> incidentId) (\s@DescribePageResponse' {} a -> s {incidentId = a} :: DescribePageResponse)

-- | The insecure content of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
describePageResponse_publicContent :: Lens.Lens' DescribePageResponse (Prelude.Maybe Prelude.Text)
describePageResponse_publicContent = Lens.lens (\DescribePageResponse' {publicContent} -> publicContent) (\s@DescribePageResponse' {} a -> s {publicContent = a} :: DescribePageResponse)

-- | The insecure subject of the message that was sent to the contact. Use
-- this field for engagements to @SMS@.
describePageResponse_publicSubject :: Lens.Lens' DescribePageResponse (Prelude.Maybe Prelude.Text)
describePageResponse_publicSubject = Lens.lens (\DescribePageResponse' {publicSubject} -> publicSubject) (\s@DescribePageResponse' {} a -> s {publicSubject = a} :: DescribePageResponse)

-- | The time that the contact channel acknowledged the engagement.
describePageResponse_readTime :: Lens.Lens' DescribePageResponse (Prelude.Maybe Prelude.UTCTime)
describePageResponse_readTime = Lens.lens (\DescribePageResponse' {readTime} -> readTime) (\s@DescribePageResponse' {} a -> s {readTime = a} :: DescribePageResponse) Prelude.. Lens.mapping Data._Time

-- | The time the engagement was sent to the contact channel.
describePageResponse_sentTime :: Lens.Lens' DescribePageResponse (Prelude.Maybe Prelude.UTCTime)
describePageResponse_sentTime = Lens.lens (\DescribePageResponse' {sentTime} -> sentTime) (\s@DescribePageResponse' {} a -> s {sentTime = a} :: DescribePageResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describePageResponse_httpStatus :: Lens.Lens' DescribePageResponse Prelude.Int
describePageResponse_httpStatus = Lens.lens (\DescribePageResponse' {httpStatus} -> httpStatus) (\s@DescribePageResponse' {} a -> s {httpStatus = a} :: DescribePageResponse)

-- | The Amazon Resource Name (ARN) of the engagement to a contact channel.
describePageResponse_pageArn :: Lens.Lens' DescribePageResponse Prelude.Text
describePageResponse_pageArn = Lens.lens (\DescribePageResponse' {pageArn} -> pageArn) (\s@DescribePageResponse' {} a -> s {pageArn = a} :: DescribePageResponse)

-- | The ARN of the engagement that engaged the contact channel.
describePageResponse_engagementArn :: Lens.Lens' DescribePageResponse Prelude.Text
describePageResponse_engagementArn = Lens.lens (\DescribePageResponse' {engagementArn} -> engagementArn) (\s@DescribePageResponse' {} a -> s {engagementArn = a} :: DescribePageResponse)

-- | The ARN of the contact that was engaged.
describePageResponse_contactArn :: Lens.Lens' DescribePageResponse Prelude.Text
describePageResponse_contactArn = Lens.lens (\DescribePageResponse' {contactArn} -> contactArn) (\s@DescribePageResponse' {} a -> s {contactArn = a} :: DescribePageResponse)

-- | The user that started the engagement.
describePageResponse_sender :: Lens.Lens' DescribePageResponse Prelude.Text
describePageResponse_sender = Lens.lens (\DescribePageResponse' {sender} -> sender) (\s@DescribePageResponse' {} a -> s {sender = a} :: DescribePageResponse)

-- | The secure subject of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
describePageResponse_subject :: Lens.Lens' DescribePageResponse Prelude.Text
describePageResponse_subject = Lens.lens (\DescribePageResponse' {subject} -> subject) (\s@DescribePageResponse' {} a -> s {subject = a} :: DescribePageResponse)

-- | The secure content of the message that was sent to the contact. Use this
-- field for engagements to @VOICE@ and @EMAIL@.
describePageResponse_content :: Lens.Lens' DescribePageResponse Prelude.Text
describePageResponse_content = Lens.lens (\DescribePageResponse' {content} -> content) (\s@DescribePageResponse' {} a -> s {content = a} :: DescribePageResponse)

instance Prelude.NFData DescribePageResponse where
  rnf DescribePageResponse' {..} =
    Prelude.rnf deliveryTime
      `Prelude.seq` Prelude.rnf incidentId
      `Prelude.seq` Prelude.rnf publicContent
      `Prelude.seq` Prelude.rnf publicSubject
      `Prelude.seq` Prelude.rnf readTime
      `Prelude.seq` Prelude.rnf sentTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pageArn
      `Prelude.seq` Prelude.rnf engagementArn
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf sender
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf content
