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
-- Module      : Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a contact with a given address book.
module Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateContactWithAddressBook' smart constructor.
data AssociateContactWithAddressBook = AssociateContactWithAddressBook'
  { -- | The ARN of the contact to associate with an address book.
    contactArn :: Core.Text,
    -- | The ARN of the address book with which to associate the contact.
    addressBookArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'addressBookArn'
  Core.Text ->
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
associateContactWithAddressBook_contactArn :: Lens.Lens' AssociateContactWithAddressBook Core.Text
associateContactWithAddressBook_contactArn = Lens.lens (\AssociateContactWithAddressBook' {contactArn} -> contactArn) (\s@AssociateContactWithAddressBook' {} a -> s {contactArn = a} :: AssociateContactWithAddressBook)

-- | The ARN of the address book with which to associate the contact.
associateContactWithAddressBook_addressBookArn :: Lens.Lens' AssociateContactWithAddressBook Core.Text
associateContactWithAddressBook_addressBookArn = Lens.lens (\AssociateContactWithAddressBook' {addressBookArn} -> addressBookArn) (\s@AssociateContactWithAddressBook' {} a -> s {addressBookArn = a} :: AssociateContactWithAddressBook)

instance
  Core.AWSRequest
    AssociateContactWithAddressBook
  where
  type
    AWSResponse AssociateContactWithAddressBook =
      AssociateContactWithAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateContactWithAddressBookResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociateContactWithAddressBook

instance Core.NFData AssociateContactWithAddressBook

instance
  Core.ToHeaders
    AssociateContactWithAddressBook
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.AssociateContactWithAddressBook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateContactWithAddressBook where
  toJSON AssociateContactWithAddressBook' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContactArn" Core..= contactArn),
            Core.Just ("AddressBookArn" Core..= addressBookArn)
          ]
      )

instance Core.ToPath AssociateContactWithAddressBook where
  toPath = Core.const "/"

instance Core.ToQuery AssociateContactWithAddressBook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateContactWithAddressBookResponse' smart constructor.
data AssociateContactWithAddressBookResponse = AssociateContactWithAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AssociateContactWithAddressBookResponse
newAssociateContactWithAddressBookResponse
  pHttpStatus_ =
    AssociateContactWithAddressBookResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateContactWithAddressBookResponse_httpStatus :: Lens.Lens' AssociateContactWithAddressBookResponse Core.Int
associateContactWithAddressBookResponse_httpStatus = Lens.lens (\AssociateContactWithAddressBookResponse' {httpStatus} -> httpStatus) (\s@AssociateContactWithAddressBookResponse' {} a -> s {httpStatus = a} :: AssociateContactWithAddressBookResponse)

instance
  Core.NFData
    AssociateContactWithAddressBookResponse
