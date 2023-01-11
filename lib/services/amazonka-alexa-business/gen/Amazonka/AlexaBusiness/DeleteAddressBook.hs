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
-- Module      : Amazonka.AlexaBusiness.DeleteAddressBook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an address book by the address book ARN.
module Amazonka.AlexaBusiness.DeleteAddressBook
  ( -- * Creating a Request
    DeleteAddressBook (..),
    newDeleteAddressBook,

    -- * Request Lenses
    deleteAddressBook_addressBookArn,

    -- * Destructuring the Response
    DeleteAddressBookResponse (..),
    newDeleteAddressBookResponse,

    -- * Response Lenses
    deleteAddressBookResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAddressBook' smart constructor.
data DeleteAddressBook = DeleteAddressBook'
  { -- | The ARN of the address book to delete.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBookArn', 'deleteAddressBook_addressBookArn' - The ARN of the address book to delete.
newDeleteAddressBook ::
  -- | 'addressBookArn'
  Prelude.Text ->
  DeleteAddressBook
newDeleteAddressBook pAddressBookArn_ =
  DeleteAddressBook'
    { addressBookArn =
        pAddressBookArn_
    }

-- | The ARN of the address book to delete.
deleteAddressBook_addressBookArn :: Lens.Lens' DeleteAddressBook Prelude.Text
deleteAddressBook_addressBookArn = Lens.lens (\DeleteAddressBook' {addressBookArn} -> addressBookArn) (\s@DeleteAddressBook' {} a -> s {addressBookArn = a} :: DeleteAddressBook)

instance Core.AWSRequest DeleteAddressBook where
  type
    AWSResponse DeleteAddressBook =
      DeleteAddressBookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAddressBookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAddressBook where
  hashWithSalt _salt DeleteAddressBook' {..} =
    _salt `Prelude.hashWithSalt` addressBookArn

instance Prelude.NFData DeleteAddressBook where
  rnf DeleteAddressBook' {..} =
    Prelude.rnf addressBookArn

instance Data.ToHeaders DeleteAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteAddressBook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAddressBook where
  toJSON DeleteAddressBook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AddressBookArn" Data..= addressBookArn)
          ]
      )

instance Data.ToPath DeleteAddressBook where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAddressBookResponse' smart constructor.
data DeleteAddressBookResponse = DeleteAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAddressBookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAddressBookResponse_httpStatus' - The response's http status code.
newDeleteAddressBookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAddressBookResponse
newDeleteAddressBookResponse pHttpStatus_ =
  DeleteAddressBookResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAddressBookResponse_httpStatus :: Lens.Lens' DeleteAddressBookResponse Prelude.Int
deleteAddressBookResponse_httpStatus = Lens.lens (\DeleteAddressBookResponse' {httpStatus} -> httpStatus) (\s@DeleteAddressBookResponse' {} a -> s {httpStatus = a} :: DeleteAddressBookResponse)

instance Prelude.NFData DeleteAddressBookResponse where
  rnf DeleteAddressBookResponse' {..} =
    Prelude.rnf httpStatus
