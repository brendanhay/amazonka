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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAddressBook' smart constructor.
data DeleteAddressBook = DeleteAddressBook'
  { -- | The ARN of the address book to delete.
    addressBookArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteAddressBook
newDeleteAddressBook pAddressBookArn_ =
  DeleteAddressBook'
    { addressBookArn =
        pAddressBookArn_
    }

-- | The ARN of the address book to delete.
deleteAddressBook_addressBookArn :: Lens.Lens' DeleteAddressBook Core.Text
deleteAddressBook_addressBookArn = Lens.lens (\DeleteAddressBook' {addressBookArn} -> addressBookArn) (\s@DeleteAddressBook' {} a -> s {addressBookArn = a} :: DeleteAddressBook)

instance Core.AWSRequest DeleteAddressBook where
  type
    AWSResponse DeleteAddressBook =
      DeleteAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAddressBookResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAddressBook

instance Core.NFData DeleteAddressBook

instance Core.ToHeaders DeleteAddressBook where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteAddressBook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAddressBook where
  toJSON DeleteAddressBook' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AddressBookArn" Core..= addressBookArn)
          ]
      )

instance Core.ToPath DeleteAddressBook where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAddressBook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAddressBookResponse' smart constructor.
data DeleteAddressBookResponse = DeleteAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteAddressBookResponse
newDeleteAddressBookResponse pHttpStatus_ =
  DeleteAddressBookResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAddressBookResponse_httpStatus :: Lens.Lens' DeleteAddressBookResponse Core.Int
deleteAddressBookResponse_httpStatus = Lens.lens (\DeleteAddressBookResponse' {httpStatus} -> httpStatus) (\s@DeleteAddressBookResponse' {} a -> s {httpStatus = a} :: DeleteAddressBookResponse)

instance Core.NFData DeleteAddressBookResponse
