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
-- Module      : Amazonka.SSMContacts.DeleteContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To remove a contact from Incident Manager, you can delete the contact.
-- Deleting a contact removes them from all escalation plans and related
-- response plans. Deleting an escalation plan removes it from all related
-- response plans. You will have to recreate the contact and its contact
-- channels before you can use it again.
module Amazonka.SSMContacts.DeleteContact
  ( -- * Creating a Request
    DeleteContact (..),
    newDeleteContact,

    -- * Request Lenses
    deleteContact_contactId,

    -- * Destructuring the Response
    DeleteContactResponse (..),
    newDeleteContactResponse,

    -- * Response Lenses
    deleteContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newDeleteContact' smart constructor.
data DeleteContact = DeleteContact'
  { -- | The Amazon Resource Name (ARN) of the contact that you\'re deleting.
    contactId :: Prelude.Text
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
-- 'contactId', 'deleteContact_contactId' - The Amazon Resource Name (ARN) of the contact that you\'re deleting.
newDeleteContact ::
  -- | 'contactId'
  Prelude.Text ->
  DeleteContact
newDeleteContact pContactId_ =
  DeleteContact' {contactId = pContactId_}

-- | The Amazon Resource Name (ARN) of the contact that you\'re deleting.
deleteContact_contactId :: Lens.Lens' DeleteContact Prelude.Text
deleteContact_contactId = Lens.lens (\DeleteContact' {contactId} -> contactId) (\s@DeleteContact' {} a -> s {contactId = a} :: DeleteContact)

instance Core.AWSRequest DeleteContact where
  type
    AWSResponse DeleteContact =
      DeleteContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContact where
  hashWithSalt _salt DeleteContact' {..} =
    _salt `Prelude.hashWithSalt` contactId

instance Prelude.NFData DeleteContact where
  rnf DeleteContact' {..} = Prelude.rnf contactId

instance Core.ToHeaders DeleteContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SSMContacts.DeleteContact" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteContact where
  toJSON DeleteContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContactId" Core..= contactId)]
      )

instance Core.ToPath DeleteContact where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteContact where
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
