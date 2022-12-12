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
-- Module      : Amazonka.SESV2.CreateContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact, which is an end-user who is receiving the email, and
-- adds them to a contact list.
module Amazonka.SESV2.CreateContact
  ( -- * Creating a Request
    CreateContact (..),
    newCreateContact,

    -- * Request Lenses
    createContact_attributesData,
    createContact_topicPreferences,
    createContact_unsubscribeAll,
    createContact_contactListName,
    createContact_emailAddress,

    -- * Destructuring the Response
    CreateContactResponse (..),
    newCreateContactResponse,

    -- * Response Lenses
    createContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | /See:/ 'newCreateContact' smart constructor.
data CreateContact = CreateContact'
  { -- | The attribute data attached to a contact.
    attributesData :: Prelude.Maybe Prelude.Text,
    -- | The contact\'s preferences for being opted-in to or opted-out of topics.
    topicPreferences :: Prelude.Maybe [TopicPreference],
    -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool,
    -- | The name of the contact list to which the contact should be added.
    contactListName :: Prelude.Text,
    -- | The contact\'s email address.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributesData', 'createContact_attributesData' - The attribute data attached to a contact.
--
-- 'topicPreferences', 'createContact_topicPreferences' - The contact\'s preferences for being opted-in to or opted-out of topics.
--
-- 'unsubscribeAll', 'createContact_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
--
-- 'contactListName', 'createContact_contactListName' - The name of the contact list to which the contact should be added.
--
-- 'emailAddress', 'createContact_emailAddress' - The contact\'s email address.
newCreateContact ::
  -- | 'contactListName'
  Prelude.Text ->
  -- | 'emailAddress'
  Prelude.Text ->
  CreateContact
newCreateContact pContactListName_ pEmailAddress_ =
  CreateContact'
    { attributesData = Prelude.Nothing,
      topicPreferences = Prelude.Nothing,
      unsubscribeAll = Prelude.Nothing,
      contactListName = pContactListName_,
      emailAddress = pEmailAddress_
    }

-- | The attribute data attached to a contact.
createContact_attributesData :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_attributesData = Lens.lens (\CreateContact' {attributesData} -> attributesData) (\s@CreateContact' {} a -> s {attributesData = a} :: CreateContact)

-- | The contact\'s preferences for being opted-in to or opted-out of topics.
createContact_topicPreferences :: Lens.Lens' CreateContact (Prelude.Maybe [TopicPreference])
createContact_topicPreferences = Lens.lens (\CreateContact' {topicPreferences} -> topicPreferences) (\s@CreateContact' {} a -> s {topicPreferences = a} :: CreateContact) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
createContact_unsubscribeAll :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Bool)
createContact_unsubscribeAll = Lens.lens (\CreateContact' {unsubscribeAll} -> unsubscribeAll) (\s@CreateContact' {} a -> s {unsubscribeAll = a} :: CreateContact)

-- | The name of the contact list to which the contact should be added.
createContact_contactListName :: Lens.Lens' CreateContact Prelude.Text
createContact_contactListName = Lens.lens (\CreateContact' {contactListName} -> contactListName) (\s@CreateContact' {} a -> s {contactListName = a} :: CreateContact)

-- | The contact\'s email address.
createContact_emailAddress :: Lens.Lens' CreateContact Prelude.Text
createContact_emailAddress = Lens.lens (\CreateContact' {emailAddress} -> emailAddress) (\s@CreateContact' {} a -> s {emailAddress = a} :: CreateContact)

instance Core.AWSRequest CreateContact where
  type
    AWSResponse CreateContact =
      CreateContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContact where
  hashWithSalt _salt CreateContact' {..} =
    _salt `Prelude.hashWithSalt` attributesData
      `Prelude.hashWithSalt` topicPreferences
      `Prelude.hashWithSalt` unsubscribeAll
      `Prelude.hashWithSalt` contactListName
      `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData CreateContact where
  rnf CreateContact' {..} =
    Prelude.rnf attributesData
      `Prelude.seq` Prelude.rnf topicPreferences
      `Prelude.seq` Prelude.rnf unsubscribeAll
      `Prelude.seq` Prelude.rnf contactListName
      `Prelude.seq` Prelude.rnf emailAddress

instance Data.ToHeaders CreateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContact where
  toJSON CreateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributesData" Data..=)
              Prelude.<$> attributesData,
            ("TopicPreferences" Data..=)
              Prelude.<$> topicPreferences,
            ("UnsubscribeAll" Data..=)
              Prelude.<$> unsubscribeAll,
            Prelude.Just ("EmailAddress" Data..= emailAddress)
          ]
      )

instance Data.ToPath CreateContact where
  toPath CreateContact' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Data.toBS contactListName,
        "/contacts"
      ]

instance Data.ToQuery CreateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createContactResponse_httpStatus' - The response's http status code.
newCreateContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContactResponse
newCreateContactResponse pHttpStatus_ =
  CreateContactResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createContactResponse_httpStatus :: Lens.Lens' CreateContactResponse Prelude.Int
createContactResponse_httpStatus = Lens.lens (\CreateContactResponse' {httpStatus} -> httpStatus) (\s@CreateContactResponse' {} a -> s {httpStatus = a} :: CreateContactResponse)

instance Prelude.NFData CreateContactResponse where
  rnf CreateContactResponse' {..} =
    Prelude.rnf httpStatus
