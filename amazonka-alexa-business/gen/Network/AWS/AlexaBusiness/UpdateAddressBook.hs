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
-- Module      : Network.AWS.AlexaBusiness.UpdateAddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates address book details by the address book ARN.
module Network.AWS.AlexaBusiness.UpdateAddressBook
  ( -- * Creating a Request
    UpdateAddressBook (..),
    newUpdateAddressBook,

    -- * Request Lenses
    updateAddressBook_name,
    updateAddressBook_description,
    updateAddressBook_addressBookArn,

    -- * Destructuring the Response
    UpdateAddressBookResponse (..),
    newUpdateAddressBookResponse,

    -- * Response Lenses
    updateAddressBookResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { -- | The updated name of the room.
    name :: Core.Maybe Core.Text,
    -- | The updated description of the room.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the room to update.
    addressBookArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateAddressBook_name' - The updated name of the room.
--
-- 'description', 'updateAddressBook_description' - The updated description of the room.
--
-- 'addressBookArn', 'updateAddressBook_addressBookArn' - The ARN of the room to update.
newUpdateAddressBook ::
  -- | 'addressBookArn'
  Core.Text ->
  UpdateAddressBook
newUpdateAddressBook pAddressBookArn_ =
  UpdateAddressBook'
    { name = Core.Nothing,
      description = Core.Nothing,
      addressBookArn = pAddressBookArn_
    }

-- | The updated name of the room.
updateAddressBook_name :: Lens.Lens' UpdateAddressBook (Core.Maybe Core.Text)
updateAddressBook_name = Lens.lens (\UpdateAddressBook' {name} -> name) (\s@UpdateAddressBook' {} a -> s {name = a} :: UpdateAddressBook)

-- | The updated description of the room.
updateAddressBook_description :: Lens.Lens' UpdateAddressBook (Core.Maybe Core.Text)
updateAddressBook_description = Lens.lens (\UpdateAddressBook' {description} -> description) (\s@UpdateAddressBook' {} a -> s {description = a} :: UpdateAddressBook)

-- | The ARN of the room to update.
updateAddressBook_addressBookArn :: Lens.Lens' UpdateAddressBook Core.Text
updateAddressBook_addressBookArn = Lens.lens (\UpdateAddressBook' {addressBookArn} -> addressBookArn) (\s@UpdateAddressBook' {} a -> s {addressBookArn = a} :: UpdateAddressBook)

instance Core.AWSRequest UpdateAddressBook where
  type
    AWSResponse UpdateAddressBook =
      UpdateAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAddressBookResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateAddressBook

instance Core.NFData UpdateAddressBook

instance Core.ToHeaders UpdateAddressBook where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateAddressBook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAddressBook where
  toJSON UpdateAddressBook' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("AddressBookArn" Core..= addressBookArn)
          ]
      )

instance Core.ToPath UpdateAddressBook where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAddressBook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAddressBookResponse' smart constructor.
data UpdateAddressBookResponse = UpdateAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAddressBookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAddressBookResponse_httpStatus' - The response's http status code.
newUpdateAddressBookResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateAddressBookResponse
newUpdateAddressBookResponse pHttpStatus_ =
  UpdateAddressBookResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateAddressBookResponse_httpStatus :: Lens.Lens' UpdateAddressBookResponse Core.Int
updateAddressBookResponse_httpStatus = Lens.lens (\UpdateAddressBookResponse' {httpStatus} -> httpStatus) (\s@UpdateAddressBookResponse' {} a -> s {httpStatus = a} :: UpdateAddressBookResponse)

instance Core.NFData UpdateAddressBookResponse
