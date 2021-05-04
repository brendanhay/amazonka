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
-- Module      : Network.AWS.AlexaBusiness.DeleteAddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an address book by the address book ARN.
module Network.AWS.AlexaBusiness.DeleteAddressBook
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAddressBook' smart constructor.
data DeleteAddressBook = DeleteAddressBook'
  { -- | The ARN of the address book to delete.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteAddressBook where
  type Rs DeleteAddressBook = DeleteAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAddressBookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAddressBook

instance Prelude.NFData DeleteAddressBook

instance Prelude.ToHeaders DeleteAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteAddressBook" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAddressBook where
  toJSON DeleteAddressBook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AddressBookArn" Prelude..= addressBookArn)
          ]
      )

instance Prelude.ToPath DeleteAddressBook where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAddressBookResponse' smart constructor.
data DeleteAddressBookResponse = DeleteAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteAddressBookResponse
