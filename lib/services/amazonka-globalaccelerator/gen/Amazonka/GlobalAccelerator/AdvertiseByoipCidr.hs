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
-- Module      : Amazonka.GlobalAccelerator.AdvertiseByoipCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Advertises an IPv4 address range that is provisioned for use with your
-- Amazon Web Services resources through bring your own IP addresses
-- (BYOIP). It can take a few minutes before traffic to the specified
-- addresses starts routing to Amazon Web Services because of propagation
-- delays.
--
-- To stop advertising the BYOIP address range, use
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/WithdrawByoipCidr.html WithdrawByoipCidr>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
-- in the /Global Accelerator Developer Guide/.
module Amazonka.GlobalAccelerator.AdvertiseByoipCidr
  ( -- * Creating a Request
    AdvertiseByoipCidr (..),
    newAdvertiseByoipCidr,

    -- * Request Lenses
    advertiseByoipCidr_cidr,

    -- * Destructuring the Response
    AdvertiseByoipCidrResponse (..),
    newAdvertiseByoipCidrResponse,

    -- * Response Lenses
    advertiseByoipCidrResponse_byoipCidr,
    advertiseByoipCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAdvertiseByoipCidr' smart constructor.
data AdvertiseByoipCidr = AdvertiseByoipCidr'
  { -- | The address range, in CIDR notation. This must be the exact range that
    -- you provisioned. You can\'t advertise only a portion of the provisioned
    -- range.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvertiseByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'advertiseByoipCidr_cidr' - The address range, in CIDR notation. This must be the exact range that
-- you provisioned. You can\'t advertise only a portion of the provisioned
-- range.
newAdvertiseByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  AdvertiseByoipCidr
newAdvertiseByoipCidr pCidr_ =
  AdvertiseByoipCidr' {cidr = pCidr_}

-- | The address range, in CIDR notation. This must be the exact range that
-- you provisioned. You can\'t advertise only a portion of the provisioned
-- range.
advertiseByoipCidr_cidr :: Lens.Lens' AdvertiseByoipCidr Prelude.Text
advertiseByoipCidr_cidr = Lens.lens (\AdvertiseByoipCidr' {cidr} -> cidr) (\s@AdvertiseByoipCidr' {} a -> s {cidr = a} :: AdvertiseByoipCidr)

instance Core.AWSRequest AdvertiseByoipCidr where
  type
    AWSResponse AdvertiseByoipCidr =
      AdvertiseByoipCidrResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AdvertiseByoipCidrResponse'
            Prelude.<$> (x Data..?> "ByoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdvertiseByoipCidr where
  hashWithSalt _salt AdvertiseByoipCidr' {..} =
    _salt `Prelude.hashWithSalt` cidr

instance Prelude.NFData AdvertiseByoipCidr where
  rnf AdvertiseByoipCidr' {..} = Prelude.rnf cidr

instance Data.ToHeaders AdvertiseByoipCidr where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.AdvertiseByoipCidr" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdvertiseByoipCidr where
  toJSON AdvertiseByoipCidr' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Cidr" Data..= cidr)]
      )

instance Data.ToPath AdvertiseByoipCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery AdvertiseByoipCidr where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdvertiseByoipCidrResponse' smart constructor.
data AdvertiseByoipCidrResponse = AdvertiseByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvertiseByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'advertiseByoipCidrResponse_byoipCidr' - Information about the address range.
--
-- 'httpStatus', 'advertiseByoipCidrResponse_httpStatus' - The response's http status code.
newAdvertiseByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdvertiseByoipCidrResponse
newAdvertiseByoipCidrResponse pHttpStatus_ =
  AdvertiseByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address range.
advertiseByoipCidrResponse_byoipCidr :: Lens.Lens' AdvertiseByoipCidrResponse (Prelude.Maybe ByoipCidr)
advertiseByoipCidrResponse_byoipCidr = Lens.lens (\AdvertiseByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@AdvertiseByoipCidrResponse' {} a -> s {byoipCidr = a} :: AdvertiseByoipCidrResponse)

-- | The response's http status code.
advertiseByoipCidrResponse_httpStatus :: Lens.Lens' AdvertiseByoipCidrResponse Prelude.Int
advertiseByoipCidrResponse_httpStatus = Lens.lens (\AdvertiseByoipCidrResponse' {httpStatus} -> httpStatus) (\s@AdvertiseByoipCidrResponse' {} a -> s {httpStatus = a} :: AdvertiseByoipCidrResponse)

instance Prelude.NFData AdvertiseByoipCidrResponse where
  rnf AdvertiseByoipCidrResponse' {..} =
    Prelude.rnf byoipCidr `Prelude.seq`
      Prelude.rnf httpStatus
