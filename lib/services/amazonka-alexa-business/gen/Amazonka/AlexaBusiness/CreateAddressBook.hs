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
-- Module      : Amazonka.AlexaBusiness.CreateAddressBook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address book with the specified details.
module Amazonka.AlexaBusiness.CreateAddressBook
  ( -- * Creating a Request
    CreateAddressBook (..),
    newCreateAddressBook,

    -- * Request Lenses
    createAddressBook_clientRequestToken,
    createAddressBook_description,
    createAddressBook_tags,
    createAddressBook_name,

    -- * Destructuring the Response
    CreateAddressBookResponse (..),
    newCreateAddressBookResponse,

    -- * Response Lenses
    createAddressBookResponse_addressBookArn,
    createAddressBookResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAddressBook' smart constructor.
data CreateAddressBook = CreateAddressBook'
  { -- | A unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the address book.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the address book.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createAddressBook_clientRequestToken' - A unique, user-specified identifier for the request that ensures
-- idempotency.
--
-- 'description', 'createAddressBook_description' - The description of the address book.
--
-- 'tags', 'createAddressBook_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'name', 'createAddressBook_name' - The name of the address book.
newCreateAddressBook ::
  -- | 'name'
  Prelude.Text ->
  CreateAddressBook
newCreateAddressBook pName_ =
  CreateAddressBook'
    { clientRequestToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A unique, user-specified identifier for the request that ensures
-- idempotency.
createAddressBook_clientRequestToken :: Lens.Lens' CreateAddressBook (Prelude.Maybe Prelude.Text)
createAddressBook_clientRequestToken = Lens.lens (\CreateAddressBook' {clientRequestToken} -> clientRequestToken) (\s@CreateAddressBook' {} a -> s {clientRequestToken = a} :: CreateAddressBook)

-- | The description of the address book.
createAddressBook_description :: Lens.Lens' CreateAddressBook (Prelude.Maybe Prelude.Text)
createAddressBook_description = Lens.lens (\CreateAddressBook' {description} -> description) (\s@CreateAddressBook' {} a -> s {description = a} :: CreateAddressBook)

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createAddressBook_tags :: Lens.Lens' CreateAddressBook (Prelude.Maybe [Tag])
createAddressBook_tags = Lens.lens (\CreateAddressBook' {tags} -> tags) (\s@CreateAddressBook' {} a -> s {tags = a} :: CreateAddressBook) Prelude.. Lens.mapping Lens.coerced

-- | The name of the address book.
createAddressBook_name :: Lens.Lens' CreateAddressBook Prelude.Text
createAddressBook_name = Lens.lens (\CreateAddressBook' {name} -> name) (\s@CreateAddressBook' {} a -> s {name = a} :: CreateAddressBook)

instance Core.AWSRequest CreateAddressBook where
  type
    AWSResponse CreateAddressBook =
      CreateAddressBookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddressBookResponse'
            Prelude.<$> (x Data..?> "AddressBookArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAddressBook where
  hashWithSalt _salt CreateAddressBook' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateAddressBook where
  rnf CreateAddressBook' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateAddressBook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAddressBook where
  toJSON CreateAddressBook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateAddressBook where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAddressBookResponse' smart constructor.
data CreateAddressBookResponse = CreateAddressBookResponse'
  { -- | The ARN of the newly created address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateAddressBookResponse where
  rnf CreateAddressBookResponse' {..} =
    Prelude.rnf addressBookArn
      `Prelude.seq` Prelude.rnf httpStatus
