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
-- Module      : Amazonka.AlexaBusiness.AssociateContactWithAddressBook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a contact with a given address book.
module Amazonka.AlexaBusiness.AssociateContactWithAddressBook
  ( -- * Creating a Request
    AssociateContactWithAddressBook (..),
    newAssociateContactWithAddressBook,

    -- * Request Lenses
    associateContactWithAddressBook_contactArn,
    associateContactWithAddressBook_addressBookArn,

    -- * Destructuring the Response
    AssociateContactWithAddressBookResponse (..),
    newAssociateContactWithAddressBookResponse,

    -- * Response Lenses
    associateContactWithAddressBookResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateContactWithAddressBook' smart constructor.
data AssociateContactWithAddressBook = AssociateContactWithAddressBook'
  { -- | The ARN of the contact to associate with an address book.
    contactArn :: Prelude.Text,
    -- | The ARN of the address book with which to associate the contact.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateContactWithAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'associateContactWithAddressBook_contactArn' - The ARN of the contact to associate with an address book.
--
-- 'addressBookArn', 'associateContactWithAddressBook_addressBookArn' - The ARN of the address book with which to associate the contact.
newAssociateContactWithAddressBook ::
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'addressBookArn'
  Prelude.Text ->
  AssociateContactWithAddressBook
newAssociateContactWithAddressBook
  pContactArn_
  pAddressBookArn_ =
    AssociateContactWithAddressBook'
      { contactArn =
          pContactArn_,
        addressBookArn = pAddressBookArn_
      }

-- | The ARN of the contact to associate with an address book.
associateContactWithAddressBook_contactArn :: Lens.Lens' AssociateContactWithAddressBook Prelude.Text
associateContactWithAddressBook_contactArn = Lens.lens (\AssociateContactWithAddressBook' {contactArn} -> contactArn) (\s@AssociateContactWithAddressBook' {} a -> s {contactArn = a} :: AssociateContactWithAddressBook)

-- | The ARN of the address book with which to associate the contact.
associateContactWithAddressBook_addressBookArn :: Lens.Lens' AssociateContactWithAddressBook Prelude.Text
associateContactWithAddressBook_addressBookArn = Lens.lens (\AssociateContactWithAddressBook' {addressBookArn} -> addressBookArn) (\s@AssociateContactWithAddressBook' {} a -> s {addressBookArn = a} :: AssociateContactWithAddressBook)

instance
  Core.AWSRequest
    AssociateContactWithAddressBook
  where
  type
    AWSResponse AssociateContactWithAddressBook =
      AssociateContactWithAddressBookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateContactWithAddressBookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateContactWithAddressBook
  where
  hashWithSalt
    _salt
    AssociateContactWithAddressBook' {..} =
      _salt
        `Prelude.hashWithSalt` contactArn
        `Prelude.hashWithSalt` addressBookArn

instance
  Prelude.NFData
    AssociateContactWithAddressBook
  where
  rnf AssociateContactWithAddressBook' {..} =
    Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf addressBookArn

instance
  Data.ToHeaders
    AssociateContactWithAddressBook
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.AssociateContactWithAddressBook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateContactWithAddressBook where
  toJSON AssociateContactWithAddressBook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContactArn" Data..= contactArn),
            Prelude.Just
              ("AddressBookArn" Data..= addressBookArn)
          ]
      )

instance Data.ToPath AssociateContactWithAddressBook where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateContactWithAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateContactWithAddressBookResponse' smart constructor.
data AssociateContactWithAddressBookResponse = AssociateContactWithAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateContactWithAddressBookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateContactWithAddressBookResponse_httpStatus' - The response's http status code.
newAssociateContactWithAddressBookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateContactWithAddressBookResponse
newAssociateContactWithAddressBookResponse
  pHttpStatus_ =
    AssociateContactWithAddressBookResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateContactWithAddressBookResponse_httpStatus :: Lens.Lens' AssociateContactWithAddressBookResponse Prelude.Int
associateContactWithAddressBookResponse_httpStatus = Lens.lens (\AssociateContactWithAddressBookResponse' {httpStatus} -> httpStatus) (\s@AssociateContactWithAddressBookResponse' {} a -> s {httpStatus = a} :: AssociateContactWithAddressBookResponse)

instance
  Prelude.NFData
    AssociateContactWithAddressBookResponse
  where
  rnf AssociateContactWithAddressBookResponse' {..} =
    Prelude.rnf httpStatus
