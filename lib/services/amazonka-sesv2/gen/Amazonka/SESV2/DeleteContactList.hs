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
-- Module      : Amazonka.SESV2.DeleteContactList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact list and all of the contacts on that list.
module Amazonka.SESV2.DeleteContactList
  ( -- * Creating a Request
    DeleteContactList (..),
    newDeleteContactList,

    -- * Request Lenses
    deleteContactList_contactListName,

    -- * Destructuring the Response
    DeleteContactListResponse (..),
    newDeleteContactListResponse,

    -- * Response Lenses
    deleteContactListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | /See:/ 'newDeleteContactList' smart constructor.
data DeleteContactList = DeleteContactList'
  { -- | The name of the contact list.
    contactListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListName', 'deleteContactList_contactListName' - The name of the contact list.
newDeleteContactList ::
  -- | 'contactListName'
  Prelude.Text ->
  DeleteContactList
newDeleteContactList pContactListName_ =
  DeleteContactList'
    { contactListName =
        pContactListName_
    }

-- | The name of the contact list.
deleteContactList_contactListName :: Lens.Lens' DeleteContactList Prelude.Text
deleteContactList_contactListName = Lens.lens (\DeleteContactList' {contactListName} -> contactListName) (\s@DeleteContactList' {} a -> s {contactListName = a} :: DeleteContactList)

instance Core.AWSRequest DeleteContactList where
  type
    AWSResponse DeleteContactList =
      DeleteContactListResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContactList where
  hashWithSalt _salt DeleteContactList' {..} =
    _salt `Prelude.hashWithSalt` contactListName

instance Prelude.NFData DeleteContactList where
  rnf DeleteContactList' {..} =
    Prelude.rnf contactListName

instance Data.ToHeaders DeleteContactList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteContactList where
  toPath DeleteContactList' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Data.toBS contactListName
      ]

instance Data.ToQuery DeleteContactList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactListResponse' smart constructor.
data DeleteContactListResponse = DeleteContactListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContactListResponse_httpStatus' - The response's http status code.
newDeleteContactListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContactListResponse
newDeleteContactListResponse pHttpStatus_ =
  DeleteContactListResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContactListResponse_httpStatus :: Lens.Lens' DeleteContactListResponse Prelude.Int
deleteContactListResponse_httpStatus = Lens.lens (\DeleteContactListResponse' {httpStatus} -> httpStatus) (\s@DeleteContactListResponse' {} a -> s {httpStatus = a} :: DeleteContactListResponse)

instance Prelude.NFData DeleteContactListResponse where
  rnf DeleteContactListResponse' {..} =
    Prelude.rnf httpStatus
