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
-- Module      : Network.AWS.AlexaBusiness.GetAddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets address the book details by the address book ARN.
module Network.AWS.AlexaBusiness.GetAddressBook
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAddressBook' smart constructor.
data GetAddressBook = GetAddressBook'
  { -- | The ARN of the address book for which to request details.
    addressBookArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetAddressBook
newGetAddressBook pAddressBookArn_ =
  GetAddressBook' {addressBookArn = pAddressBookArn_}

-- | The ARN of the address book for which to request details.
getAddressBook_addressBookArn :: Lens.Lens' GetAddressBook Core.Text
getAddressBook_addressBookArn = Lens.lens (\GetAddressBook' {addressBookArn} -> addressBookArn) (\s@GetAddressBook' {} a -> s {addressBookArn = a} :: GetAddressBook)

instance Core.AWSRequest GetAddressBook where
  type
    AWSResponse GetAddressBook =
      GetAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAddressBookResponse'
            Core.<$> (x Core..?> "AddressBook")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAddressBook

instance Core.NFData GetAddressBook

instance Core.ToHeaders GetAddressBook where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetAddressBook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAddressBook where
  toJSON GetAddressBook' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AddressBookArn" Core..= addressBookArn)
          ]
      )

instance Core.ToPath GetAddressBook where
  toPath = Core.const "/"

instance Core.ToQuery GetAddressBook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAddressBookResponse' smart constructor.
data GetAddressBookResponse = GetAddressBookResponse'
  { -- | The details of the requested address book.
    addressBook :: Core.Maybe AddressBook,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetAddressBookResponse
newGetAddressBookResponse pHttpStatus_ =
  GetAddressBookResponse'
    { addressBook = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the requested address book.
getAddressBookResponse_addressBook :: Lens.Lens' GetAddressBookResponse (Core.Maybe AddressBook)
getAddressBookResponse_addressBook = Lens.lens (\GetAddressBookResponse' {addressBook} -> addressBook) (\s@GetAddressBookResponse' {} a -> s {addressBook = a} :: GetAddressBookResponse)

-- | The response's http status code.
getAddressBookResponse_httpStatus :: Lens.Lens' GetAddressBookResponse Core.Int
getAddressBookResponse_httpStatus = Lens.lens (\GetAddressBookResponse' {httpStatus} -> httpStatus) (\s@GetAddressBookResponse' {} a -> s {httpStatus = a} :: GetAddressBookResponse)

instance Core.NFData GetAddressBookResponse
