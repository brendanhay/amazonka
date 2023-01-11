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
-- Module      : Amazonka.Snowball.DescribeAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes an @AddressId@ and returns specific details about that address in
-- the form of an @Address@ object.
module Amazonka.Snowball.DescribeAddress
  ( -- * Creating a Request
    DescribeAddress (..),
    newDescribeAddress,

    -- * Request Lenses
    describeAddress_addressId,

    -- * Destructuring the Response
    DescribeAddressResponse (..),
    newDescribeAddressResponse,

    -- * Response Lenses
    describeAddressResponse_address,
    describeAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newDescribeAddress' smart constructor.
data DescribeAddress = DescribeAddress'
  { -- | The automatically generated ID for a specific address.
    addressId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressId', 'describeAddress_addressId' - The automatically generated ID for a specific address.
newDescribeAddress ::
  -- | 'addressId'
  Prelude.Text ->
  DescribeAddress
newDescribeAddress pAddressId_ =
  DescribeAddress' {addressId = pAddressId_}

-- | The automatically generated ID for a specific address.
describeAddress_addressId :: Lens.Lens' DescribeAddress Prelude.Text
describeAddress_addressId = Lens.lens (\DescribeAddress' {addressId} -> addressId) (\s@DescribeAddress' {} a -> s {addressId = a} :: DescribeAddress)

instance Core.AWSRequest DescribeAddress where
  type
    AWSResponse DescribeAddress =
      DescribeAddressResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressResponse'
            Prelude.<$> (x Data..?> "Address")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddress where
  hashWithSalt _salt DescribeAddress' {..} =
    _salt `Prelude.hashWithSalt` addressId

instance Prelude.NFData DescribeAddress where
  rnf DescribeAddress' {..} = Prelude.rnf addressId

instance Data.ToHeaders DescribeAddress where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.DescribeAddress" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAddress where
  toJSON DescribeAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AddressId" Data..= addressId)]
      )

instance Data.ToPath DescribeAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAddress where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAddressResponse' smart constructor.
data DescribeAddressResponse = DescribeAddressResponse'
  { -- | The address that you want the Snow device(s) associated with a specific
    -- job to be shipped to.
    address :: Prelude.Maybe Address,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'describeAddressResponse_address' - The address that you want the Snow device(s) associated with a specific
-- job to be shipped to.
--
-- 'httpStatus', 'describeAddressResponse_httpStatus' - The response's http status code.
newDescribeAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAddressResponse
newDescribeAddressResponse pHttpStatus_ =
  DescribeAddressResponse'
    { address = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The address that you want the Snow device(s) associated with a specific
-- job to be shipped to.
describeAddressResponse_address :: Lens.Lens' DescribeAddressResponse (Prelude.Maybe Address)
describeAddressResponse_address = Lens.lens (\DescribeAddressResponse' {address} -> address) (\s@DescribeAddressResponse' {} a -> s {address = a} :: DescribeAddressResponse)

-- | The response's http status code.
describeAddressResponse_httpStatus :: Lens.Lens' DescribeAddressResponse Prelude.Int
describeAddressResponse_httpStatus = Lens.lens (\DescribeAddressResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressResponse' {} a -> s {httpStatus = a} :: DescribeAddressResponse)

instance Prelude.NFData DescribeAddressResponse where
  rnf DescribeAddressResponse' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf httpStatus
