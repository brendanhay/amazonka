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
-- Module      : Network.AWS.SESv2.GetContactList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns contact list metadata. It does not return any information about
-- the contacts present in the list.
module Network.AWS.SESv2.GetContactList
  ( -- * Creating a Request
    GetContactList (..),
    newGetContactList,

    -- * Request Lenses
    getContactList_contactListName,

    -- * Destructuring the Response
    GetContactListResponse (..),
    newGetContactListResponse,

    -- * Response Lenses
    getContactListResponse_createdTimestamp,
    getContactListResponse_topics,
    getContactListResponse_tags,
    getContactListResponse_lastUpdatedTimestamp,
    getContactListResponse_description,
    getContactListResponse_contactListName,
    getContactListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | /See:/ 'newGetContactList' smart constructor.
data GetContactList = GetContactList'
  { -- | The name of the contact list.
    contactListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListName', 'getContactList_contactListName' - The name of the contact list.
newGetContactList ::
  -- | 'contactListName'
  Prelude.Text ->
  GetContactList
newGetContactList pContactListName_ =
  GetContactList'
    { contactListName =
        pContactListName_
    }

-- | The name of the contact list.
getContactList_contactListName :: Lens.Lens' GetContactList Prelude.Text
getContactList_contactListName = Lens.lens (\GetContactList' {contactListName} -> contactListName) (\s@GetContactList' {} a -> s {contactListName = a} :: GetContactList)

instance Core.AWSRequest GetContactList where
  type
    AWSResponse GetContactList =
      GetContactListResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactListResponse'
            Prelude.<$> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "Topics" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "ContactListName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContactList

instance Prelude.NFData GetContactList

instance Core.ToHeaders GetContactList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetContactList where
  toPath GetContactList' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Core.toBS contactListName
      ]

instance Core.ToQuery GetContactList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactListResponse' smart constructor.
data GetContactListResponse = GetContactListResponse'
  { -- | A timestamp noting when the contact list was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | An interest group, theme, or label within a list. A contact list can
    -- have multiple topics.
    topics :: Prelude.Maybe [Topic],
    -- | The tags associated with a contact list.
    tags :: Prelude.Maybe [Tag],
    -- | A timestamp noting the last time the contact list was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | A description of what the contact list is about.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact list.
    contactListName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'getContactListResponse_createdTimestamp' - A timestamp noting when the contact list was created.
--
-- 'topics', 'getContactListResponse_topics' - An interest group, theme, or label within a list. A contact list can
-- have multiple topics.
--
-- 'tags', 'getContactListResponse_tags' - The tags associated with a contact list.
--
-- 'lastUpdatedTimestamp', 'getContactListResponse_lastUpdatedTimestamp' - A timestamp noting the last time the contact list was updated.
--
-- 'description', 'getContactListResponse_description' - A description of what the contact list is about.
--
-- 'contactListName', 'getContactListResponse_contactListName' - The name of the contact list.
--
-- 'httpStatus', 'getContactListResponse_httpStatus' - The response's http status code.
newGetContactListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactListResponse
newGetContactListResponse pHttpStatus_ =
  GetContactListResponse'
    { createdTimestamp =
        Prelude.Nothing,
      topics = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      description = Prelude.Nothing,
      contactListName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A timestamp noting when the contact list was created.
getContactListResponse_createdTimestamp :: Lens.Lens' GetContactListResponse (Prelude.Maybe Prelude.UTCTime)
getContactListResponse_createdTimestamp = Lens.lens (\GetContactListResponse' {createdTimestamp} -> createdTimestamp) (\s@GetContactListResponse' {} a -> s {createdTimestamp = a} :: GetContactListResponse) Prelude.. Lens.mapping Core._Time

-- | An interest group, theme, or label within a list. A contact list can
-- have multiple topics.
getContactListResponse_topics :: Lens.Lens' GetContactListResponse (Prelude.Maybe [Topic])
getContactListResponse_topics = Lens.lens (\GetContactListResponse' {topics} -> topics) (\s@GetContactListResponse' {} a -> s {topics = a} :: GetContactListResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The tags associated with a contact list.
getContactListResponse_tags :: Lens.Lens' GetContactListResponse (Prelude.Maybe [Tag])
getContactListResponse_tags = Lens.lens (\GetContactListResponse' {tags} -> tags) (\s@GetContactListResponse' {} a -> s {tags = a} :: GetContactListResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A timestamp noting the last time the contact list was updated.
getContactListResponse_lastUpdatedTimestamp :: Lens.Lens' GetContactListResponse (Prelude.Maybe Prelude.UTCTime)
getContactListResponse_lastUpdatedTimestamp = Lens.lens (\GetContactListResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetContactListResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetContactListResponse) Prelude.. Lens.mapping Core._Time

-- | A description of what the contact list is about.
getContactListResponse_description :: Lens.Lens' GetContactListResponse (Prelude.Maybe Prelude.Text)
getContactListResponse_description = Lens.lens (\GetContactListResponse' {description} -> description) (\s@GetContactListResponse' {} a -> s {description = a} :: GetContactListResponse)

-- | The name of the contact list.
getContactListResponse_contactListName :: Lens.Lens' GetContactListResponse (Prelude.Maybe Prelude.Text)
getContactListResponse_contactListName = Lens.lens (\GetContactListResponse' {contactListName} -> contactListName) (\s@GetContactListResponse' {} a -> s {contactListName = a} :: GetContactListResponse)

-- | The response's http status code.
getContactListResponse_httpStatus :: Lens.Lens' GetContactListResponse Prelude.Int
getContactListResponse_httpStatus = Lens.lens (\GetContactListResponse' {httpStatus} -> httpStatus) (\s@GetContactListResponse' {} a -> s {httpStatus = a} :: GetContactListResponse)

instance Prelude.NFData GetContactListResponse
