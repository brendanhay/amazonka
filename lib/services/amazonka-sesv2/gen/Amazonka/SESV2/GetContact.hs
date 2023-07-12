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
-- Module      : Amazonka.SESV2.GetContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a contact from a contact list.
module Amazonka.SESV2.GetContact
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
    getContactResponse_attributesData,
    getContactResponse_contactListName,
    getContactResponse_createdTimestamp,
    getContactResponse_emailAddress,
    getContactResponse_lastUpdatedTimestamp,
    getContactResponse_topicDefaultPreferences,
    getContactResponse_topicPreferences,
    getContactResponse_unsubscribeAll,
    getContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Prelude.<$> (x Data..?> "AttributesData")
            Prelude.<*> (x Data..?> "ContactListName")
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "EmailAddress")
            Prelude.<*> (x Data..?> "LastUpdatedTimestamp")
            Prelude.<*> ( x
                            Data..?> "TopicDefaultPreferences"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "TopicPreferences"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "UnsubscribeAll")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContact where
  hashWithSalt _salt GetContact' {..} =
    _salt
      `Prelude.hashWithSalt` contactListName
      `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData GetContact where
  rnf GetContact' {..} =
    Prelude.rnf contactListName
      `Prelude.seq` Prelude.rnf emailAddress

instance Data.ToHeaders GetContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetContact where
  toPath GetContact' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Data.toBS contactListName,
        "/contacts/",
        Data.toBS emailAddress
      ]

instance Data.ToQuery GetContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | The attribute data attached to a contact.
    attributesData :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact list to which the contact belongs.
    contactListName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp noting when the contact was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The contact\'s email addres.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | A timestamp noting the last time the contact\'s information was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The default topic preferences applied to the contact.
    topicDefaultPreferences :: Prelude.Maybe [TopicPreference],
    -- | The contact\'s preference for being opted-in to or opted-out of a
    -- topic.>
    topicPreferences :: Prelude.Maybe [TopicPreference],
    -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool,
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
-- 'attributesData', 'getContactResponse_attributesData' - The attribute data attached to a contact.
--
-- 'contactListName', 'getContactResponse_contactListName' - The name of the contact list to which the contact belongs.
--
-- 'createdTimestamp', 'getContactResponse_createdTimestamp' - A timestamp noting when the contact was created.
--
-- 'emailAddress', 'getContactResponse_emailAddress' - The contact\'s email addres.
--
-- 'lastUpdatedTimestamp', 'getContactResponse_lastUpdatedTimestamp' - A timestamp noting the last time the contact\'s information was updated.
--
-- 'topicDefaultPreferences', 'getContactResponse_topicDefaultPreferences' - The default topic preferences applied to the contact.
--
-- 'topicPreferences', 'getContactResponse_topicPreferences' - The contact\'s preference for being opted-in to or opted-out of a
-- topic.>
--
-- 'unsubscribeAll', 'getContactResponse_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
--
-- 'httpStatus', 'getContactResponse_httpStatus' - The response's http status code.
newGetContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactResponse
newGetContactResponse pHttpStatus_ =
  GetContactResponse'
    { attributesData =
        Prelude.Nothing,
      contactListName = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      topicDefaultPreferences = Prelude.Nothing,
      topicPreferences = Prelude.Nothing,
      unsubscribeAll = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attribute data attached to a contact.
getContactResponse_attributesData :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_attributesData = Lens.lens (\GetContactResponse' {attributesData} -> attributesData) (\s@GetContactResponse' {} a -> s {attributesData = a} :: GetContactResponse)

-- | The name of the contact list to which the contact belongs.
getContactResponse_contactListName :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_contactListName = Lens.lens (\GetContactResponse' {contactListName} -> contactListName) (\s@GetContactResponse' {} a -> s {contactListName = a} :: GetContactResponse)

-- | A timestamp noting when the contact was created.
getContactResponse_createdTimestamp :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.UTCTime)
getContactResponse_createdTimestamp = Lens.lens (\GetContactResponse' {createdTimestamp} -> createdTimestamp) (\s@GetContactResponse' {} a -> s {createdTimestamp = a} :: GetContactResponse) Prelude.. Lens.mapping Data._Time

-- | The contact\'s email addres.
getContactResponse_emailAddress :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_emailAddress = Lens.lens (\GetContactResponse' {emailAddress} -> emailAddress) (\s@GetContactResponse' {} a -> s {emailAddress = a} :: GetContactResponse)

-- | A timestamp noting the last time the contact\'s information was updated.
getContactResponse_lastUpdatedTimestamp :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.UTCTime)
getContactResponse_lastUpdatedTimestamp = Lens.lens (\GetContactResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetContactResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetContactResponse) Prelude.. Lens.mapping Data._Time

-- | The default topic preferences applied to the contact.
getContactResponse_topicDefaultPreferences :: Lens.Lens' GetContactResponse (Prelude.Maybe [TopicPreference])
getContactResponse_topicDefaultPreferences = Lens.lens (\GetContactResponse' {topicDefaultPreferences} -> topicDefaultPreferences) (\s@GetContactResponse' {} a -> s {topicDefaultPreferences = a} :: GetContactResponse) Prelude.. Lens.mapping Lens.coerced

-- | The contact\'s preference for being opted-in to or opted-out of a
-- topic.>
getContactResponse_topicPreferences :: Lens.Lens' GetContactResponse (Prelude.Maybe [TopicPreference])
getContactResponse_topicPreferences = Lens.lens (\GetContactResponse' {topicPreferences} -> topicPreferences) (\s@GetContactResponse' {} a -> s {topicPreferences = a} :: GetContactResponse) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
getContactResponse_unsubscribeAll :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Bool)
getContactResponse_unsubscribeAll = Lens.lens (\GetContactResponse' {unsubscribeAll} -> unsubscribeAll) (\s@GetContactResponse' {} a -> s {unsubscribeAll = a} :: GetContactResponse)

-- | The response's http status code.
getContactResponse_httpStatus :: Lens.Lens' GetContactResponse Prelude.Int
getContactResponse_httpStatus = Lens.lens (\GetContactResponse' {httpStatus} -> httpStatus) (\s@GetContactResponse' {} a -> s {httpStatus = a} :: GetContactResponse)

instance Prelude.NFData GetContactResponse where
  rnf GetContactResponse' {..} =
    Prelude.rnf attributesData
      `Prelude.seq` Prelude.rnf contactListName
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf topicDefaultPreferences
      `Prelude.seq` Prelude.rnf topicPreferences
      `Prelude.seq` Prelude.rnf unsubscribeAll
      `Prelude.seq` Prelude.rnf httpStatus
