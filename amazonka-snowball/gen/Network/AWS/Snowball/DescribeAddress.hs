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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeAddress' smart constructor.
data DescribeAddress = DescribeAddress'
  { -- | The automatically generated ID for a specific address.
    addressId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeAddress
newDescribeAddress pAddressId_ =
  DescribeAddress' {addressId = pAddressId_}

-- | The automatically generated ID for a specific address.
describeAddress_addressId :: Lens.Lens' DescribeAddress Core.Text
describeAddress_addressId = Lens.lens (\DescribeAddress' {addressId} -> addressId) (\s@DescribeAddress' {} a -> s {addressId = a} :: DescribeAddress)

instance Core.AWSRequest DescribeAddress where
  type
    AWSResponse DescribeAddress =
      DescribeAddressResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressResponse'
            Core.<$> (x Core..?> "Address")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAddress

instance Core.NFData DescribeAddress

instance Core.ToHeaders DescribeAddress where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.DescribeAddress" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAddress where
  toJSON DescribeAddress' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AddressId" Core..= addressId)]
      )

instance Core.ToPath DescribeAddress where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAddress where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAddressResponse' smart constructor.
data DescribeAddressResponse = DescribeAddressResponse'
  { -- | The address that you want the Snow device(s) associated with a specific
    -- job to be shipped to.
    address :: Core.Maybe Address,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAddressResponse
newDescribeAddressResponse pHttpStatus_ =
  DescribeAddressResponse'
    { address = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The address that you want the Snow device(s) associated with a specific
-- job to be shipped to.
describeAddressResponse_address :: Lens.Lens' DescribeAddressResponse (Core.Maybe Address)
describeAddressResponse_address = Lens.lens (\DescribeAddressResponse' {address} -> address) (\s@DescribeAddressResponse' {} a -> s {address = a} :: DescribeAddressResponse)

-- | The response's http status code.
describeAddressResponse_httpStatus :: Lens.Lens' DescribeAddressResponse Core.Int
describeAddressResponse_httpStatus = Lens.lens (\DescribeAddressResponse' {httpStatus} -> httpStatus) (\s@DescribeAddressResponse' {} a -> s {httpStatus = a} :: DescribeAddressResponse)

instance Core.NFData DescribeAddressResponse
