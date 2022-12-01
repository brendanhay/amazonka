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
-- Module      : Amazonka.AlexaBusiness.GetAddressBook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets address the book details by the address book ARN.
module Amazonka.AlexaBusiness.GetAddressBook
  ( -- * Creating a Request
    GetAddressBook (..),
    newGetAddressBook,

    -- * Request Lenses
    getAddressBook_addressBookArn,

    -- * Destructuring the Response
    GetAddressBookResponse (..),
    newGetAddressBookResponse,

    -- * Response Lenses
    getAddressBookResponse_addressBook,
    getAddressBookResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAddressBook' smart constructor.
data GetAddressBook = GetAddressBook'
  { -- | The ARN of the address book for which to request details.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBookArn', 'getAddressBook_addressBookArn' - The ARN of the address book for which to request details.
newGetAddressBook ::
  -- | 'addressBookArn'
  Prelude.Text ->
  GetAddressBook
newGetAddressBook pAddressBookArn_ =
  GetAddressBook' {addressBookArn = pAddressBookArn_}

-- | The ARN of the address book for which to request details.
getAddressBook_addressBookArn :: Lens.Lens' GetAddressBook Prelude.Text
getAddressBook_addressBookArn = Lens.lens (\GetAddressBook' {addressBookArn} -> addressBookArn) (\s@GetAddressBook' {} a -> s {addressBookArn = a} :: GetAddressBook)

instance Core.AWSRequest GetAddressBook where
  type
    AWSResponse GetAddressBook =
      GetAddressBookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAddressBookResponse'
            Prelude.<$> (x Core..?> "AddressBook")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAddressBook where
  hashWithSalt _salt GetAddressBook' {..} =
    _salt `Prelude.hashWithSalt` addressBookArn

instance Prelude.NFData GetAddressBook where
  rnf GetAddressBook' {..} = Prelude.rnf addressBookArn

instance Core.ToHeaders GetAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetAddressBook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetAddressBook where
  toJSON GetAddressBook' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AddressBookArn" Core..= addressBookArn)
          ]
      )

instance Core.ToPath GetAddressBook where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAddressBookResponse' smart constructor.
data GetAddressBookResponse = GetAddressBookResponse'
  { -- | The details of the requested address book.
    addressBook :: Prelude.Maybe AddressBook,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAddressBookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBook', 'getAddressBookResponse_addressBook' - The details of the requested address book.
--
-- 'httpStatus', 'getAddressBookResponse_httpStatus' - The response's http status code.
newGetAddressBookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAddressBookResponse
newGetAddressBookResponse pHttpStatus_ =
  GetAddressBookResponse'
    { addressBook =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the requested address book.
getAddressBookResponse_addressBook :: Lens.Lens' GetAddressBookResponse (Prelude.Maybe AddressBook)
getAddressBookResponse_addressBook = Lens.lens (\GetAddressBookResponse' {addressBook} -> addressBook) (\s@GetAddressBookResponse' {} a -> s {addressBook = a} :: GetAddressBookResponse)

-- | The response's http status code.
getAddressBookResponse_httpStatus :: Lens.Lens' GetAddressBookResponse Prelude.Int
getAddressBookResponse_httpStatus = Lens.lens (\GetAddressBookResponse' {httpStatus} -> httpStatus) (\s@GetAddressBookResponse' {} a -> s {httpStatus = a} :: GetAddressBookResponse)

instance Prelude.NFData GetAddressBookResponse where
  rnf GetAddressBookResponse' {..} =
    Prelude.rnf addressBook
      `Prelude.seq` Prelude.rnf httpStatus
