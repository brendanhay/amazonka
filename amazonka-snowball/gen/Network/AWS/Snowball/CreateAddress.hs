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
-- Module      : Network.AWS.Snowball.CreateAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address for a Snow device to be shipped to. In most regions,
-- addresses are validated at the time of creation. The address you provide
-- must be located within the serviceable area of your region. If the
-- address is invalid or unsupported, then an exception is thrown.
module Network.AWS.Snowball.CreateAddress
  ( -- * Creating a Request
    CreateAddress (..),
    newCreateAddress,

    -- * Request Lenses
    createAddress_address,

    -- * Destructuring the Response
    CreateAddressResponse (..),
    newCreateAddressResponse,

    -- * Response Lenses
    createAddressResponse_addressId,
    createAddressResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newCreateAddress' smart constructor.
data CreateAddress = CreateAddress'
  { -- | The address that you want the Snow device shipped to.
    address :: Address
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'createAddress_address' - The address that you want the Snow device shipped to.
newCreateAddress ::
  -- | 'address'
  Address ->
  CreateAddress
newCreateAddress pAddress_ =
  CreateAddress' {address = pAddress_}

-- | The address that you want the Snow device shipped to.
createAddress_address :: Lens.Lens' CreateAddress Address
createAddress_address = Lens.lens (\CreateAddress' {address} -> address) (\s@CreateAddress' {} a -> s {address = a} :: CreateAddress)

instance Prelude.AWSRequest CreateAddress where
  type Rs CreateAddress = CreateAddressResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAddressResponse'
            Prelude.<$> (x Prelude..?> "AddressId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAddress

instance Prelude.NFData CreateAddress

instance Prelude.ToHeaders CreateAddress where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.CreateAddress" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAddress where
  toJSON CreateAddress' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Address" Prelude..= address)]
      )

instance Prelude.ToPath CreateAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAddress where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAddressResponse' smart constructor.
data CreateAddressResponse = CreateAddressResponse'
  { -- | The automatically generated ID for a specific address. You\'ll use this
    -- ID when you create a job to specify which address you want the Snow
    -- device for that job shipped to.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressId', 'createAddressResponse_addressId' - The automatically generated ID for a specific address. You\'ll use this
-- ID when you create a job to specify which address you want the Snow
-- device for that job shipped to.
--
-- 'httpStatus', 'createAddressResponse_httpStatus' - The response's http status code.
newCreateAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAddressResponse
newCreateAddressResponse pHttpStatus_ =
  CreateAddressResponse'
    { addressId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The automatically generated ID for a specific address. You\'ll use this
-- ID when you create a job to specify which address you want the Snow
-- device for that job shipped to.
createAddressResponse_addressId :: Lens.Lens' CreateAddressResponse (Prelude.Maybe Prelude.Text)
createAddressResponse_addressId = Lens.lens (\CreateAddressResponse' {addressId} -> addressId) (\s@CreateAddressResponse' {} a -> s {addressId = a} :: CreateAddressResponse)

-- | The response's http status code.
createAddressResponse_httpStatus :: Lens.Lens' CreateAddressResponse Prelude.Int
createAddressResponse_httpStatus = Lens.lens (\CreateAddressResponse' {httpStatus} -> httpStatus) (\s@CreateAddressResponse' {} a -> s {httpStatus = a} :: CreateAddressResponse)

instance Prelude.NFData CreateAddressResponse
