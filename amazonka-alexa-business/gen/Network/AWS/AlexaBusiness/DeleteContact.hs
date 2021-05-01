{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.DeleteContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact by the contact ARN.
module Network.AWS.AlexaBusiness.DeleteContact
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteContact' smart constructor.
data DeleteContact = DeleteContact'
  { -- | The ARN of the contact to delete.
    contactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteContact where
  type Rs DeleteContact = DeleteContactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContact

instance Prelude.NFData DeleteContact

instance Prelude.ToHeaders DeleteContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteContact" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteContact where
  toJSON DeleteContact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContactArn" Prelude..= contactArn)]
      )

instance Prelude.ToPath DeleteContact where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactResponse' smart constructor.
data DeleteContactResponse = DeleteContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteContactResponse
