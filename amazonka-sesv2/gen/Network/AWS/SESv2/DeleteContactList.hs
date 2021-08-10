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
-- Module      : Network.AWS.SESv2.DeleteContactList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact list and all of the contacts on that list.
module Network.AWS.SESv2.DeleteContactList
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

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
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContactList

instance Prelude.NFData DeleteContactList

instance Core.ToHeaders DeleteContactList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteContactList where
  toPath DeleteContactList' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Core.toBS contactListName
      ]

instance Core.ToQuery DeleteContactList where
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

instance Prelude.NFData DeleteContactListResponse
