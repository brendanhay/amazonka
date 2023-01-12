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
-- Module      : Amazonka.AlexaBusiness.DeleteContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact by the contact ARN.
module Amazonka.AlexaBusiness.DeleteContact
  ( -- * Creating a Request
    DeleteContact (..),
    newDeleteContact,

    -- * Request Lenses
    deleteContact_contactArn,

    -- * Destructuring the Response
    DeleteContactResponse (..),
    newDeleteContactResponse,

    -- * Response Lenses
    deleteContactResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContact' smart constructor.
data DeleteContact = DeleteContact'
  { -- | The ARN of the contact to delete.
    contactArn :: Prelude.Text
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
-- 'contactArn', 'deleteContact_contactArn' - The ARN of the contact to delete.
newDeleteContact ::
  -- | 'contactArn'
  Prelude.Text ->
  DeleteContact
newDeleteContact pContactArn_ =
  DeleteContact' {contactArn = pContactArn_}

-- | The ARN of the contact to delete.
deleteContact_contactArn :: Lens.Lens' DeleteContact Prelude.Text
deleteContact_contactArn = Lens.lens (\DeleteContact' {contactArn} -> contactArn) (\s@DeleteContact' {} a -> s {contactArn = a} :: DeleteContact)

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
    _salt `Prelude.hashWithSalt` contactArn

instance Prelude.NFData DeleteContact where
  rnf DeleteContact' {..} = Prelude.rnf contactArn

instance Data.ToHeaders DeleteContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteContact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteContact where
  toJSON DeleteContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContactArn" Data..= contactArn)]
      )

instance Data.ToPath DeleteContact where
  toPath = Prelude.const "/"

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
