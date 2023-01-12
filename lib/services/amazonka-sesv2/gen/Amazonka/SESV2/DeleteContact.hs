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
-- Module      : Amazonka.SESV2.DeleteContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a contact from a contact list.
module Amazonka.SESV2.DeleteContact
  ( -- * Creating a Request
    DeleteContact (..),
    newDeleteContact,

    -- * Request Lenses
    deleteContact_contactListName,
    deleteContact_emailAddress,

    -- * Destructuring the Response
    DeleteContactResponse (..),
    newDeleteContactResponse,

    -- * Response Lenses
    deleteContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | /See:/ 'newDeleteContact' smart constructor.
data DeleteContact = DeleteContact'
  { -- | The name of the contact list from which the contact should be removed.
    contactListName :: Prelude.Text,
    -- | The contact\'s email address.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactListName', 'deleteContact_contactListName' - The name of the contact list from which the contact should be removed.
--
-- 'emailAddress', 'deleteContact_emailAddress' - The contact\'s email address.
newDeleteContact ::
  -- | 'contactListName'
  Prelude.Text ->
  -- | 'emailAddress'
  Prelude.Text ->
  DeleteContact
newDeleteContact pContactListName_ pEmailAddress_ =
  DeleteContact'
    { contactListName = pContactListName_,
      emailAddress = pEmailAddress_
    }

-- | The name of the contact list from which the contact should be removed.
deleteContact_contactListName :: Lens.Lens' DeleteContact Prelude.Text
deleteContact_contactListName = Lens.lens (\DeleteContact' {contactListName} -> contactListName) (\s@DeleteContact' {} a -> s {contactListName = a} :: DeleteContact)

-- | The contact\'s email address.
deleteContact_emailAddress :: Lens.Lens' DeleteContact Prelude.Text
deleteContact_emailAddress = Lens.lens (\DeleteContact' {emailAddress} -> emailAddress) (\s@DeleteContact' {} a -> s {emailAddress = a} :: DeleteContact)

instance Core.AWSRequest DeleteContact where
  type
    AWSResponse DeleteContact =
      DeleteContactResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContact where
  hashWithSalt _salt DeleteContact' {..} =
    _salt `Prelude.hashWithSalt` contactListName
      `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData DeleteContact where
  rnf DeleteContact' {..} =
    Prelude.rnf contactListName
      `Prelude.seq` Prelude.rnf emailAddress

instance Data.ToHeaders DeleteContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteContact where
  toPath DeleteContact' {..} =
    Prelude.mconcat
      [ "/v2/email/contact-lists/",
        Data.toBS contactListName,
        "/contacts/",
        Data.toBS emailAddress
      ]

instance Data.ToQuery DeleteContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactResponse' smart constructor.
data DeleteContactResponse = DeleteContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContactResponse_httpStatus' - The response's http status code.
newDeleteContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContactResponse
newDeleteContactResponse pHttpStatus_ =
  DeleteContactResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteContactResponse_httpStatus :: Lens.Lens' DeleteContactResponse Prelude.Int
deleteContactResponse_httpStatus = Lens.lens (\DeleteContactResponse' {httpStatus} -> httpStatus) (\s@DeleteContactResponse' {} a -> s {httpStatus = a} :: DeleteContactResponse)

instance Prelude.NFData DeleteContactResponse where
  rnf DeleteContactResponse' {..} =
    Prelude.rnf httpStatus
