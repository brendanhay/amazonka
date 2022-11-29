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
-- Module      : Amazonka.SESV2.CreateContactList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact list.
module Amazonka.SESV2.CreateContactList
  ( -- * Creating a Request
    CreateContactList (..),
    newCreateContactList,

    -- * Request Lenses
    createContactList_tags,
    createContactList_description,
    createContactList_topics,
    createContactList_contactListName,

    -- * Destructuring the Response
    CreateContactListResponse (..),
    newCreateContactListResponse,

    -- * Response Lenses
    createContactListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | /See:/ 'newCreateContactList' smart constructor.
data CreateContactList = CreateContactList'
  { -- | The tags associated with a contact list.
    tags :: Prelude.Maybe [Tag],
    -- | A description of what the contact list is about.
    description :: Prelude.Maybe Prelude.Text,
    -- | An interest group, theme, or label within a list. A contact list can
    -- have multiple topics.
    topics :: Prelude.Maybe [Topic],
    -- | The name of the contact list.
    contactListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContactList_tags' - The tags associated with a contact list.
--
-- 'description', 'createContactList_description' - A description of what the contact list is about.
--
-- 'topics', 'createContactList_topics' - An interest group, theme, or label within a list. A contact list can
-- have multiple topics.
--
-- 'contactListName', 'createContactList_contactListName' - The name of the contact list.
newCreateContactList ::
  -- | 'contactListName'
  Prelude.Text ->
  CreateContactList
newCreateContactList pContactListName_ =
  CreateContactList'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      topics = Prelude.Nothing,
      contactListName = pContactListName_
    }

-- | The tags associated with a contact list.
createContactList_tags :: Lens.Lens' CreateContactList (Prelude.Maybe [Tag])
createContactList_tags = Lens.lens (\CreateContactList' {tags} -> tags) (\s@CreateContactList' {} a -> s {tags = a} :: CreateContactList) Prelude.. Lens.mapping Lens.coerced

-- | A description of what the contact list is about.
createContactList_description :: Lens.Lens' CreateContactList (Prelude.Maybe Prelude.Text)
createContactList_description = Lens.lens (\CreateContactList' {description} -> description) (\s@CreateContactList' {} a -> s {description = a} :: CreateContactList)

-- | An interest group, theme, or label within a list. A contact list can
-- have multiple topics.
createContactList_topics :: Lens.Lens' CreateContactList (Prelude.Maybe [Topic])
createContactList_topics = Lens.lens (\CreateContactList' {topics} -> topics) (\s@CreateContactList' {} a -> s {topics = a} :: CreateContactList) Prelude.. Lens.mapping Lens.coerced

-- | The name of the contact list.
createContactList_contactListName :: Lens.Lens' CreateContactList Prelude.Text
createContactList_contactListName = Lens.lens (\CreateContactList' {contactListName} -> contactListName) (\s@CreateContactList' {} a -> s {contactListName = a} :: CreateContactList)

instance Core.AWSRequest CreateContactList where
  type
    AWSResponse CreateContactList =
      CreateContactListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateContactListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContactList where
  hashWithSalt _salt CreateContactList' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` topics
      `Prelude.hashWithSalt` contactListName

instance Prelude.NFData CreateContactList where
  rnf CreateContactList' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf topics
      `Prelude.seq` Prelude.rnf contactListName

instance Core.ToHeaders CreateContactList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateContactList where
  toJSON CreateContactList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("Topics" Core..=) Prelude.<$> topics,
            Prelude.Just
              ("ContactListName" Core..= contactListName)
          ]
      )

instance Core.ToPath CreateContactList where
  toPath = Prelude.const "/v2/email/contact-lists"

instance Core.ToQuery CreateContactList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactListResponse' smart constructor.
data CreateContactListResponse = CreateContactListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createContactListResponse_httpStatus' - The response's http status code.
newCreateContactListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContactListResponse
newCreateContactListResponse pHttpStatus_ =
  CreateContactListResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createContactListResponse_httpStatus :: Lens.Lens' CreateContactListResponse Prelude.Int
createContactListResponse_httpStatus = Lens.lens (\CreateContactListResponse' {httpStatus} -> httpStatus) (\s@CreateContactListResponse' {} a -> s {httpStatus = a} :: CreateContactListResponse)

instance Prelude.NFData CreateContactListResponse where
  rnf CreateContactListResponse' {..} =
    Prelude.rnf httpStatus
