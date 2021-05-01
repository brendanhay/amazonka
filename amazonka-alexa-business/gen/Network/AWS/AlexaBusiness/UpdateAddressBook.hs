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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { -- | The updated name of the room.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the room.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the room to update.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateAddressBook
newUpdateAddressBook pAddressBookArn_ =
  UpdateAddressBook'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      addressBookArn = pAddressBookArn_
    }

-- | The updated name of the room.
updateAddressBook_name :: Lens.Lens' UpdateAddressBook (Prelude.Maybe Prelude.Text)
updateAddressBook_name = Lens.lens (\UpdateAddressBook' {name} -> name) (\s@UpdateAddressBook' {} a -> s {name = a} :: UpdateAddressBook)

-- | The updated description of the room.
updateAddressBook_description :: Lens.Lens' UpdateAddressBook (Prelude.Maybe Prelude.Text)
updateAddressBook_description = Lens.lens (\UpdateAddressBook' {description} -> description) (\s@UpdateAddressBook' {} a -> s {description = a} :: UpdateAddressBook)

-- | The ARN of the room to update.
updateAddressBook_addressBookArn :: Lens.Lens' UpdateAddressBook Prelude.Text
updateAddressBook_addressBookArn = Lens.lens (\UpdateAddressBook' {addressBookArn} -> addressBookArn) (\s@UpdateAddressBook' {} a -> s {addressBookArn = a} :: UpdateAddressBook)

instance Prelude.AWSRequest UpdateAddressBook where
  type Rs UpdateAddressBook = UpdateAddressBookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAddressBookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAddressBook

instance Prelude.NFData UpdateAddressBook

instance Prelude.ToHeaders UpdateAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.UpdateAddressBook" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateAddressBook where
  toJSON UpdateAddressBook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just
              ("AddressBookArn" Prelude..= addressBookArn)
          ]
      )

instance Prelude.ToPath UpdateAddressBook where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAddressBookResponse' smart constructor.
data UpdateAddressBookResponse = UpdateAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateAddressBookResponse
newUpdateAddressBookResponse pHttpStatus_ =
  UpdateAddressBookResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateAddressBookResponse_httpStatus :: Lens.Lens' UpdateAddressBookResponse Prelude.Int
updateAddressBookResponse_httpStatus = Lens.lens (\UpdateAddressBookResponse' {httpStatus} -> httpStatus) (\s@UpdateAddressBookResponse' {} a -> s {httpStatus = a} :: UpdateAddressBookResponse)

instance Prelude.NFData UpdateAddressBookResponse
