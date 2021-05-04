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
-- Module      : Network.AWS.AlexaBusiness.CreateAddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address book with the specified details.
module Network.AWS.AlexaBusiness.CreateAddressBook
  ( -- * Creating a Request
    CreateAddressBook (..),
    newCreateAddressBook,

    -- * Request Lenses
    createAddressBook_tags,
    createAddressBook_description,
    createAddressBook_clientRequestToken,
    createAddressBook_name,

    -- * Destructuring the Response
    CreateAddressBookResponse (..),
    newCreateAddressBookResponse,

    -- * Response Lenses
    createAddressBookResponse_addressBookArn,
    createAddressBookResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAddressBook' smart constructor.
data CreateAddressBook = CreateAddressBook'
  { -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The description of the address book.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the address book.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAddressBook_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'description', 'createAddressBook_description' - The description of the address book.
--
-- 'clientRequestToken', 'createAddressBook_clientRequestToken' - A unique, user-specified identifier for the request that ensures
-- idempotency.
--
-- 'name', 'createAddressBook_name' - The name of the address book.
newCreateAddressBook ::
  -- | 'name'
  Prelude.Text ->
  CreateAddressBook
newCreateAddressBook pName_ =
  CreateAddressBook'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      name = pName_
    }

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createAddressBook_tags :: Lens.Lens' CreateAddressBook (Prelude.Maybe [Tag])
createAddressBook_tags = Lens.lens (\CreateAddressBook' {tags} -> tags) (\s@CreateAddressBook' {} a -> s {tags = a} :: CreateAddressBook) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the address book.
createAddressBook_description :: Lens.Lens' CreateAddressBook (Prelude.Maybe Prelude.Text)
createAddressBook_description = Lens.lens (\CreateAddressBook' {description} -> description) (\s@CreateAddressBook' {} a -> s {description = a} :: CreateAddressBook)

-- | A unique, user-specified identifier for the request that ensures
-- idempotency.
createAddressBook_clientRequestToken :: Lens.Lens' CreateAddressBook (Prelude.Maybe Prelude.Text)
createAddressBook_clientRequestToken = Lens.lens (\CreateAddressBook' {clientRequestToken} -> clientRequestToken) (\s@CreateAddressBook' {} a -> s {clientRequestToken = a} :: CreateAddressBook)

-- | The name of the address book.
createAddressBook_name :: Lens.Lens' CreateAddressBook Prelude.Text
createAddressBook_name = Lens.lens (\CreateAddressBook' {name} -> name) (\s@CreateAddressBook' {} a -> s {name = a} :: CreateAddressBook)

instance Prelude.AWSRequest CreateAddressBook where
  type Rs CreateAddressBook = CreateAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddressBookResponse'
            Prelude.<$> (x Prelude..?> "AddressBookArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAddressBook

instance Prelude.NFData CreateAddressBook

instance Prelude.ToHeaders CreateAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.CreateAddressBook" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAddressBook where
  toJSON CreateAddressBook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateAddressBook where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAddressBookResponse' smart constructor.
data CreateAddressBookResponse = CreateAddressBookResponse'
  { -- | The ARN of the newly created address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAddressBookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBookArn', 'createAddressBookResponse_addressBookArn' - The ARN of the newly created address book.
--
-- 'httpStatus', 'createAddressBookResponse_httpStatus' - The response's http status code.
newCreateAddressBookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAddressBookResponse
newCreateAddressBookResponse pHttpStatus_ =
  CreateAddressBookResponse'
    { addressBookArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created address book.
createAddressBookResponse_addressBookArn :: Lens.Lens' CreateAddressBookResponse (Prelude.Maybe Prelude.Text)
createAddressBookResponse_addressBookArn = Lens.lens (\CreateAddressBookResponse' {addressBookArn} -> addressBookArn) (\s@CreateAddressBookResponse' {} a -> s {addressBookArn = a} :: CreateAddressBookResponse)

-- | The response's http status code.
createAddressBookResponse_httpStatus :: Lens.Lens' CreateAddressBookResponse Prelude.Int
createAddressBookResponse_httpStatus = Lens.lens (\CreateAddressBookResponse' {httpStatus} -> httpStatus) (\s@CreateAddressBookResponse' {} a -> s {httpStatus = a} :: CreateAddressBookResponse)

instance Prelude.NFData CreateAddressBookResponse
