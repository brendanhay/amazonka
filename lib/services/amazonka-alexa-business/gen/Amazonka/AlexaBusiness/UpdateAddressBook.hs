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
-- Module      : Amazonka.AlexaBusiness.UpdateAddressBook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates address book details by the address book ARN.
module Amazonka.AlexaBusiness.UpdateAddressBook
  ( -- * Creating a Request
    UpdateAddressBook (..),
    newUpdateAddressBook,

    -- * Request Lenses
    updateAddressBook_description,
    updateAddressBook_name,
    updateAddressBook_addressBookArn,

    -- * Destructuring the Response
    UpdateAddressBookResponse (..),
    newUpdateAddressBookResponse,

    -- * Response Lenses
    updateAddressBookResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { -- | The updated description of the room.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated name of the room.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the room to update.
    addressBookArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateAddressBook_description' - The updated description of the room.
--
-- 'name', 'updateAddressBook_name' - The updated name of the room.
--
-- 'addressBookArn', 'updateAddressBook_addressBookArn' - The ARN of the room to update.
newUpdateAddressBook ::
  -- | 'addressBookArn'
  Prelude.Text ->
  UpdateAddressBook
newUpdateAddressBook pAddressBookArn_ =
  UpdateAddressBook'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      addressBookArn = pAddressBookArn_
    }

-- | The updated description of the room.
updateAddressBook_description :: Lens.Lens' UpdateAddressBook (Prelude.Maybe Prelude.Text)
updateAddressBook_description = Lens.lens (\UpdateAddressBook' {description} -> description) (\s@UpdateAddressBook' {} a -> s {description = a} :: UpdateAddressBook)

-- | The updated name of the room.
updateAddressBook_name :: Lens.Lens' UpdateAddressBook (Prelude.Maybe Prelude.Text)
updateAddressBook_name = Lens.lens (\UpdateAddressBook' {name} -> name) (\s@UpdateAddressBook' {} a -> s {name = a} :: UpdateAddressBook)

-- | The ARN of the room to update.
updateAddressBook_addressBookArn :: Lens.Lens' UpdateAddressBook Prelude.Text
updateAddressBook_addressBookArn = Lens.lens (\UpdateAddressBook' {addressBookArn} -> addressBookArn) (\s@UpdateAddressBook' {} a -> s {addressBookArn = a} :: UpdateAddressBook)

instance Core.AWSRequest UpdateAddressBook where
  type
    AWSResponse UpdateAddressBook =
      UpdateAddressBookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAddressBookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAddressBook where
  hashWithSalt _salt UpdateAddressBook' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` addressBookArn

instance Prelude.NFData UpdateAddressBook where
  rnf UpdateAddressBook' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf addressBookArn

instance Data.ToHeaders UpdateAddressBook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.UpdateAddressBook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAddressBook where
  toJSON UpdateAddressBook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just
              ("AddressBookArn" Data..= addressBookArn)
          ]
      )

instance Data.ToPath UpdateAddressBook where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAddressBook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAddressBookResponse' smart constructor.
data UpdateAddressBookResponse = UpdateAddressBookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateAddressBookResponse where
  rnf UpdateAddressBookResponse' {..} =
    Prelude.rnf httpStatus
