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
-- Module      : Network.AWS.SESv2.CreateContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact, which is an end-user who is receiving the email, and
-- adds them to a contact list.
module Network.AWS.SESv2.CreateContact
  ( -- * Creating a Request
    CreateContact (..),
    newCreateContact,

    -- * Request Lenses
    createContact_unsubscribeAll,
    createContact_attributesData,
    createContact_topicPreferences,
    createContact_contactListName,
    createContact_emailAddress,

    -- * Destructuring the Response
    CreateContactResponse (..),
    newCreateContactResponse,

    -- * Response Lenses
    createContactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | /See:/ 'newCreateContact' smart constructor.
data CreateContact = CreateContact'
  { -- | A boolean value status noting if the contact is unsubscribed from all
    -- contact list topics.
    unsubscribeAll :: Prelude.Maybe Prelude.Bool,
    -- | The attribute data attached to a contact.
    attributesData :: Prelude.Maybe Prelude.Text,
    -- | The contact\'s preferences for being opted-in to or opted-out of topics.
    topicPreferences :: Prelude.Maybe [TopicPreference],
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
-- 'unsubscribeAll', 'createContact_unsubscribeAll' - A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
--
-- 'attributesData', 'createContact_attributesData' - The attribute data attached to a contact.
--
-- 'topicPreferences', 'createContact_topicPreferences' - The contact\'s preferences for being opted-in to or opted-out of topics.
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
    { unsubscribeAll = Prelude.Nothing,
      attributesData = Prelude.Nothing,
      topicPreferences = Prelude.Nothing,
      contactListName = pContactListName_,
      emailAddress = pEmailAddress_
    }

-- | A boolean value status noting if the contact is unsubscribed from all
-- contact list topics.
createContact_unsubscribeAll :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Bool)
createContact_unsubscribeAll = Lens.lens (\CreateContact' {unsubscribeAll} -> unsubscribeAll) (\s@CreateContact' {} a -> s {unsubscribeAll = a} :: CreateContact)

-- | The attribute data attached to a contact.
createContact_attributesData :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_attributesData = Lens.lens (\CreateContact' {attributesData} -> attributesData) (\s@CreateContact' {} a -> s {attributesData = a} :: CreateContact)

-- | The contact\'s preferences for being opted-in to or opted-out of topics.
createContact_topicPreferences :: Lens.Lens' CreateContact (Prelude.Maybe [TopicPreference])
createContact_topicPreferences = Lens.lens (\CreateContact' {topicPreferences} -> topicPreferences) (\s@CreateContact' {} a -> s {topicPreferences = a} :: CreateContact) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContact

instance Prelude.NFData CreateContact

instance Core.ToHeaders CreateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateContact where
  toJSON CreateContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UnsubscribeAll" Core..=)
              Prelude.<$> unsubscribeAll,
            ("AttributesData" Core..=)
              Prelude.<$> attributesData,
            ("TopicPreferences" Core..=)
              Prelude.<$> topicPreferences,
            Prelude.Just ("EmailAddress" Core..= emailAddress)
          ]
      )

instance Core.ToPath CreateContact where
  toPath CreateContact' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Core.toBS contactListName,
        "/contacts"
      ]

instance Core.ToQuery CreateContact where
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

instance Prelude.NFData CreateContactResponse
