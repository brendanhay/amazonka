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
-- Module      : Network.AWS.SESv2.GetContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a contact from a contact list.
module Network.AWS.SESv2.GetContact
  ( -- * Creating a Request
    GetContact (..),
    newGetContact,

    -- * Request Lenses
    getContact_contactListName,
    getContact_emailAddress,

    -- * Destructuring the Response
    GetContactResponse (..),
    newGetContactResponse,

    -- * Response Lenses
    getContactResponse_createdTimestamp,
    getContactResponse_topicDefaultPreferences,
    getContactResponse_unsubscribeAll,
    getContactResponse_attributesData,
    getContactResponse_topicPreferences,
    getContactResponse_lastUpdatedTimestamp,
    getContactResponse_contactListName,
    getContactResponse_emailAddress,
    getContactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | /See:/ 'newGetContact' smart constructor.
data GetContact = GetContact'
  { -- | The name of the contact list to which the contact belongs.
    contactListName :: Prelude.Text,
    -- | The contact\'s email addres.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListName', 'getContact_contactListName' - The name of the contact list to which the contact belongs.
--
-- 'emailAddress', 'getContact_emailAddress' - The contact\'s email addres.
newGetContact ::
  -- | 'contactListName'
  Prelude.Text ->
  -- | 'emailAddress'
  Prelude.Text ->
  GetContact
newGetContact pContactListName_ pEmailAddress_ =
  GetContact'
    { contactListName = pContactListName_,
      emailAddress = pEmailAddress_
    }

-- | The name of the contact list to which the contact belongs.
getContact_contactListName :: Lens.Lens' GetContact Prelude.Text
getContact_contactListName = Lens.lens (\GetContact' {contactListName} -> contactListName) (\s@GetContact' {} a -> s {contactListName = a} :: GetContact)

-- | The contact\'s email addres.
getContact_emailAddress :: Lens.Lens' GetContact Prelude.Text
getContact_emailAddress = Lens.lens (\GetContact' {emailAddress} -> emailAddress) (\s@GetContact' {} a -> s {emailAddress = a} :: GetContact)

instance Core.AWSRequest GetContact where
  type AWSResponse GetContact = GetContactResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Prelude.<$> (x Core..?> "CreatedTimestamp")
            Prelude.<*> ( x Core..?> "TopicDefaultPreferences"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "UnsubscribeAll")
            Prelude.<*> (x Core..?> "AttributesData")
            Prelude.<*> ( x Core..?> "TopicPreferences"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "ContactListName")
            Prelude.<*> (x Core..?> "EmailAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContact

instance Prelude.NFData GetContact

instance Core.ToHeaders GetContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetContact where
  toPath GetContact' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Core.toBS contactListName,
        "/contacts/",
        Core.toBS emailAddress
      ]

instance Core.ToQuery GetContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | A timestamp noting when the contact was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The default topic preferences applied to the contact.
    topicDefaultPreferences :: Prelude.Maybe [TopicPreference],
    -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool,
    -- | The attribute data attached to a contact.
    attributesData :: Prelude.Maybe Prelude.Text,
    -- | The contact\'s preference for being opted-in to or opted-out of a
    -- topic.>
    topicPreferences :: Prelude.Maybe [TopicPreference],
    -- | A timestamp noting the last time the contact\'s information was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the contact list to which the contact belongs.
    contactListName :: Prelude.Maybe Prelude.Text,
    -- | The contact\'s email addres.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'getContactResponse_createdTimestamp' - A timestamp noting when the contact was created.
--
-- 'topicDefaultPreferences', 'getContactResponse_topicDefaultPreferences' - The default topic preferences applied to the contact.
--
-- 'unsubscribeAll', 'getContactResponse_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
--
-- 'attributesData', 'getContactResponse_attributesData' - The attribute data attached to a contact.
--
-- 'topicPreferences', 'getContactResponse_topicPreferences' - The contact\'s preference for being opted-in to or opted-out of a
-- topic.>
--
-- 'lastUpdatedTimestamp', 'getContactResponse_lastUpdatedTimestamp' - A timestamp noting the last time the contact\'s information was updated.
--
-- 'contactListName', 'getContactResponse_contactListName' - The name of the contact list to which the contact belongs.
--
-- 'emailAddress', 'getContactResponse_emailAddress' - The contact\'s email addres.
--
-- 'httpStatus', 'getContactResponse_httpStatus' - The response's http status code.
newGetContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactResponse
newGetContactResponse pHttpStatus_ =
  GetContactResponse'
    { createdTimestamp =
        Prelude.Nothing,
      topicDefaultPreferences = Prelude.Nothing,
      unsubscribeAll = Prelude.Nothing,
      attributesData = Prelude.Nothing,
      topicPreferences = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      contactListName = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A timestamp noting when the contact was created.
getContactResponse_createdTimestamp :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.UTCTime)
getContactResponse_createdTimestamp = Lens.lens (\GetContactResponse' {createdTimestamp} -> createdTimestamp) (\s@GetContactResponse' {} a -> s {createdTimestamp = a} :: GetContactResponse) Prelude.. Lens.mapping Core._Time

-- | The default topic preferences applied to the contact.
getContactResponse_topicDefaultPreferences :: Lens.Lens' GetContactResponse (Prelude.Maybe [TopicPreference])
getContactResponse_topicDefaultPreferences = Lens.lens (\GetContactResponse' {topicDefaultPreferences} -> topicDefaultPreferences) (\s@GetContactResponse' {} a -> s {topicDefaultPreferences = a} :: GetContactResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
getContactResponse_unsubscribeAll :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Bool)
getContactResponse_unsubscribeAll = Lens.lens (\GetContactResponse' {unsubscribeAll} -> unsubscribeAll) (\s@GetContactResponse' {} a -> s {unsubscribeAll = a} :: GetContactResponse)

-- | The attribute data attached to a contact.
getContactResponse_attributesData :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_attributesData = Lens.lens (\GetContactResponse' {attributesData} -> attributesData) (\s@GetContactResponse' {} a -> s {attributesData = a} :: GetContactResponse)

-- | The contact\'s preference for being opted-in to or opted-out of a
-- topic.>
getContactResponse_topicPreferences :: Lens.Lens' GetContactResponse (Prelude.Maybe [TopicPreference])
getContactResponse_topicPreferences = Lens.lens (\GetContactResponse' {topicPreferences} -> topicPreferences) (\s@GetContactResponse' {} a -> s {topicPreferences = a} :: GetContactResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A timestamp noting the last time the contact\'s information was updated.
getContactResponse_lastUpdatedTimestamp :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.UTCTime)
getContactResponse_lastUpdatedTimestamp = Lens.lens (\GetContactResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetContactResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetContactResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the contact list to which the contact belongs.
getContactResponse_contactListName :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_contactListName = Lens.lens (\GetContactResponse' {contactListName} -> contactListName) (\s@GetContactResponse' {} a -> s {contactListName = a} :: GetContactResponse)

-- | The contact\'s email addres.
getContactResponse_emailAddress :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_emailAddress = Lens.lens (\GetContactResponse' {emailAddress} -> emailAddress) (\s@GetContactResponse' {} a -> s {emailAddress = a} :: GetContactResponse)

-- | The response's http status code.
getContactResponse_httpStatus :: Lens.Lens' GetContactResponse Prelude.Int
getContactResponse_httpStatus = Lens.lens (\GetContactResponse' {httpStatus} -> httpStatus) (\s@GetContactResponse' {} a -> s {httpStatus = a} :: GetContactResponse)

instance Prelude.NFData GetContactResponse
