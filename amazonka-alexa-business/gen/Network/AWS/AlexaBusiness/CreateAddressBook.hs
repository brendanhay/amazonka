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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAddressBook' smart constructor.
data CreateAddressBook = CreateAddressBook'
  { -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Core.Maybe [Tag],
    -- | The description of the address book.
    description :: Core.Maybe Core.Text,
    -- | A unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name of the address book.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  CreateAddressBook
newCreateAddressBook pName_ =
  CreateAddressBook'
    { tags = Core.Nothing,
      description = Core.Nothing,
      clientRequestToken = Core.Nothing,
      name = pName_
    }

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createAddressBook_tags :: Lens.Lens' CreateAddressBook (Core.Maybe [Tag])
createAddressBook_tags = Lens.lens (\CreateAddressBook' {tags} -> tags) (\s@CreateAddressBook' {} a -> s {tags = a} :: CreateAddressBook) Core.. Lens.mapping Lens._Coerce

-- | The description of the address book.
createAddressBook_description :: Lens.Lens' CreateAddressBook (Core.Maybe Core.Text)
createAddressBook_description = Lens.lens (\CreateAddressBook' {description} -> description) (\s@CreateAddressBook' {} a -> s {description = a} :: CreateAddressBook)

-- | A unique, user-specified identifier for the request that ensures
-- idempotency.
createAddressBook_clientRequestToken :: Lens.Lens' CreateAddressBook (Core.Maybe Core.Text)
createAddressBook_clientRequestToken = Lens.lens (\CreateAddressBook' {clientRequestToken} -> clientRequestToken) (\s@CreateAddressBook' {} a -> s {clientRequestToken = a} :: CreateAddressBook)

-- | The name of the address book.
createAddressBook_name :: Lens.Lens' CreateAddressBook Core.Text
createAddressBook_name = Lens.lens (\CreateAddressBook' {name} -> name) (\s@CreateAddressBook' {} a -> s {name = a} :: CreateAddressBook)

instance Core.AWSRequest CreateAddressBook where
  type
    AWSResponse CreateAddressBook =
      CreateAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddressBookResponse'
            Core.<$> (x Core..?> "AddressBookArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateAddressBook

instance Core.NFData CreateAddressBook

instance Core.ToHeaders CreateAddressBook where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateAddressBook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateAddressBook where
  toJSON CreateAddressBook' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateAddressBook where
  toPath = Core.const "/"

instance Core.ToQuery CreateAddressBook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAddressBookResponse' smart constructor.
data CreateAddressBookResponse = CreateAddressBookResponse'
  { -- | The ARN of the newly created address book.
    addressBookArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateAddressBookResponse
newCreateAddressBookResponse pHttpStatus_ =
  CreateAddressBookResponse'
    { addressBookArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created address book.
createAddressBookResponse_addressBookArn :: Lens.Lens' CreateAddressBookResponse (Core.Maybe Core.Text)
createAddressBookResponse_addressBookArn = Lens.lens (\CreateAddressBookResponse' {addressBookArn} -> addressBookArn) (\s@CreateAddressBookResponse' {} a -> s {addressBookArn = a} :: CreateAddressBookResponse)

-- | The response's http status code.
createAddressBookResponse_httpStatus :: Lens.Lens' CreateAddressBookResponse Core.Int
createAddressBookResponse_httpStatus = Lens.lens (\CreateAddressBookResponse' {httpStatus} -> httpStatus) (\s@CreateAddressBookResponse' {} a -> s {httpStatus = a} :: CreateAddressBookResponse)

instance Core.NFData CreateAddressBookResponse
