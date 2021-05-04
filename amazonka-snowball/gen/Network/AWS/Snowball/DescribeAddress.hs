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
-- Module      : Network.AWS.Snowball.DescribeAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes an @AddressId@ and returns specific details about that address in
-- the form of an @Address@ object.
module Network.AWS.Snowball.DescribeAddress
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeAddress' smart constructor.
data DescribeAddress = DescribeAddress'
  { -- | The automatically generated ID for a specific address.
    addressId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeAddress where
  type Rs DescribeAddress = DescribeAddressResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressResponse'
            Prelude.<$> (x Prelude..?> "Address")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAddress

instance Prelude.NFData DescribeAddress

instance Prelude.ToHeaders DescribeAddress where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.DescribeAddress" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeAddress where
  toJSON DescribeAddress' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("AddressId" Prelude..= addressId)]
      )

instance Prelude.ToPath DescribeAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAddress where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAddressResponse' smart constructor.
data DescribeAddressResponse = DescribeAddressResponse'
  { -- | The address that you want the Snow device(s) associated with a specific
    -- job to be shipped to.
    address :: Prelude.Maybe Address,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DescribeAddressResponse
