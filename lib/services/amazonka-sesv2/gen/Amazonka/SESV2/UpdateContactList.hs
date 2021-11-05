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
-- Module      : Amazonka.SESV2.UpdateContactList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates contact list metadata. This operation does a complete
-- replacement.
module Amazonka.SESV2.UpdateContactList
  ( -- * Creating a Request
    UpdateContactList (..),
    newUpdateContactList,

    -- * Request Lenses
    updateContactList_topics,
    updateContactList_description,
    updateContactList_contactListName,

    -- * Destructuring the Response
    UpdateContactListResponse (..),
    newUpdateContactListResponse,

    -- * Response Lenses
    updateContactListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | /See:/ 'newUpdateContactList' smart constructor.
data UpdateContactList = UpdateContactList'
  { -- | An interest group, theme, or label within a list. A contact list can
    -- have multiple topics.
    topics :: Prelude.Maybe [Topic],
    -- | A description of what the contact list is about.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact list.
    contactListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topics', 'updateContactList_topics' - An interest group, theme, or label within a list. A contact list can
-- have multiple topics.
--
-- 'description', 'updateContactList_description' - A description of what the contact list is about.
--
-- 'contactListName', 'updateContactList_contactListName' - The name of the contact list.
newUpdateContactList ::
  -- | 'contactListName'
  Prelude.Text ->
  UpdateContactList
newUpdateContactList pContactListName_ =
  UpdateContactList'
    { topics = Prelude.Nothing,
      description = Prelude.Nothing,
      contactListName = pContactListName_
    }

-- | An interest group, theme, or label within a list. A contact list can
-- have multiple topics.
updateContactList_topics :: Lens.Lens' UpdateContactList (Prelude.Maybe [Topic])
updateContactList_topics = Lens.lens (\UpdateContactList' {topics} -> topics) (\s@UpdateContactList' {} a -> s {topics = a} :: UpdateContactList) Prelude.. Lens.mapping Lens.coerced

-- | A description of what the contact list is about.
updateContactList_description :: Lens.Lens' UpdateContactList (Prelude.Maybe Prelude.Text)
updateContactList_description = Lens.lens (\UpdateContactList' {description} -> description) (\s@UpdateContactList' {} a -> s {description = a} :: UpdateContactList)

-- | The name of the contact list.
updateContactList_contactListName :: Lens.Lens' UpdateContactList Prelude.Text
updateContactList_contactListName = Lens.lens (\UpdateContactList' {contactListName} -> contactListName) (\s@UpdateContactList' {} a -> s {contactListName = a} :: UpdateContactList)

instance Core.AWSRequest UpdateContactList where
  type
    AWSResponse UpdateContactList =
      UpdateContactListResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContactList

instance Prelude.NFData UpdateContactList

instance Core.ToHeaders UpdateContactList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContactList where
  toJSON UpdateContactList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Topics" Core..=) Prelude.<$> topics,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateContactList where
  toPath UpdateContactList' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Core.toBS contactListName
      ]

instance Core.ToQuery UpdateContactList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactListResponse' smart constructor.
data UpdateContactListResponse = UpdateContactListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactListResponse_httpStatus' - The response's http status code.
newUpdateContactListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContactListResponse
newUpdateContactListResponse pHttpStatus_ =
  UpdateContactListResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateContactListResponse_httpStatus :: Lens.Lens' UpdateContactListResponse Prelude.Int
updateContactListResponse_httpStatus = Lens.lens (\UpdateContactListResponse' {httpStatus} -> httpStatus) (\s@UpdateContactListResponse' {} a -> s {httpStatus = a} :: UpdateContactListResponse)

instance Prelude.NFData UpdateContactListResponse
