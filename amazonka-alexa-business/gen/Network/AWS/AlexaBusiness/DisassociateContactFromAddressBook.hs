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
-- Module      : Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a contact from a given address book.
module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
  ( -- * Creating a Request
    DisassociateContactFromAddressBook (..),
    newDisassociateContactFromAddressBook,

    -- * Request Lenses
    disassociateContactFromAddressBook_contactArn,
    disassociateContactFromAddressBook_addressBookArn,

    -- * Destructuring the Response
    DisassociateContactFromAddressBookResponse (..),
    newDisassociateContactFromAddressBookResponse,

    -- * Response Lenses
    disassociateContactFromAddressBookResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateContactFromAddressBook' smart constructor.
data DisassociateContactFromAddressBook = DisassociateContactFromAddressBook'
  { -- | The ARN of the contact to disassociate from an address book.
    contactArn :: Prelude.Text,
    -- | The ARN of the address from which to disassociate the contact.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateContactFromAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'disassociateContactFromAddressBook_contactArn' - The ARN of the contact to disassociate from an address book.
--
-- 'addressBookArn', 'disassociateContactFromAddressBook_addressBookArn' - The ARN of the address from which to disassociate the contact.
newDisassociateContactFromAddressBook ::
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'addressBookArn'
  Prelude.Text ->
  DisassociateContactFromAddressBook
newDisassociateContactFromAddressBook
  pContactArn_
  pAddressBookArn_ =
    DisassociateContactFromAddressBook'
      { contactArn =
          pContactArn_,
        addressBookArn = pAddressBookArn_
      }

-- | The ARN of the contact to disassociate from an address book.
disassociateContactFromAddressBook_contactArn :: Lens.Lens' DisassociateContactFromAddressBook Prelude.Text
disassociateContactFromAddressBook_contactArn = Lens.lens (\DisassociateContactFromAddressBook' {contactArn} -> contactArn) (\s@DisassociateContactFromAddressBook' {} a -> s {contactArn = a} :: DisassociateContactFromAddressBook)

-- | The ARN of the address from which to disassociate the contact.
disassociateContactFromAddressBook_addressBookArn :: Lens.Lens' DisassociateContactFromAddressBook Prelude.Text
disassociateContactFromAddressBook_addressBookArn = Lens.lens (\DisassociateContactFromAddressBook' {addressBookArn} -> addressBookArn) (\s@DisassociateContactFromAddressBook' {} a -> s {addressBookArn = a} :: DisassociateContactFromAddressBook)

instance
  Prelude.AWSRequest
    DisassociateContactFromAddressBook
  where
  type
    Rs DisassociateContactFromAddressBook =
      DisassociateContactFromAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateContactFromAddressBookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateContactFromAddressBook

instance
  Prelude.NFData
    DisassociateContactFromAddressBook

instance
  Prelude.ToHeaders
    DisassociateContactFromAddressBook
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DisassociateContactFromAddressBook" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisassociateContactFromAddressBook
  where
  toJSON DisassociateContactFromAddressBook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContactArn" Prelude..= contactArn),
            Prelude.Just
              ("AddressBookArn" Prelude..= addressBookArn)
          ]
      )

instance
  Prelude.ToPath
    DisassociateContactFromAddressBook
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateContactFromAddressBook
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateContactFromAddressBookResponse' smart constructor.
data DisassociateContactFromAddressBookResponse = DisassociateContactFromAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateContactFromAddressBookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateContactFromAddressBookResponse_httpStatus' - The response's http status code.
newDisassociateContactFromAddressBookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateContactFromAddressBookResponse
newDisassociateContactFromAddressBookResponse
  pHttpStatus_ =
    DisassociateContactFromAddressBookResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateContactFromAddressBookResponse_httpStatus :: Lens.Lens' DisassociateContactFromAddressBookResponse Prelude.Int
disassociateContactFromAddressBookResponse_httpStatus = Lens.lens (\DisassociateContactFromAddressBookResponse' {httpStatus} -> httpStatus) (\s@DisassociateContactFromAddressBookResponse' {} a -> s {httpStatus = a} :: DisassociateContactFromAddressBookResponse)

instance
  Prelude.NFData
    DisassociateContactFromAddressBookResponse
