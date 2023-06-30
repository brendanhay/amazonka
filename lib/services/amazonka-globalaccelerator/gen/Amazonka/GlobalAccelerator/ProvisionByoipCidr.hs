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
-- Module      : Amazonka.GlobalAccelerator.ProvisionByoipCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions an IP address range to use with your Amazon Web Services
-- resources through bring your own IP addresses (BYOIP) and creates a
-- corresponding address pool. After the address range is provisioned, it
-- is ready to be advertised using
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/AdvertiseByoipCidr.html AdvertiseByoipCidr>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
-- in the /Global Accelerator Developer Guide/.
module Amazonka.GlobalAccelerator.ProvisionByoipCidr
  ( -- * Creating a Request
    ProvisionByoipCidr (..),
    newProvisionByoipCidr,

    -- * Request Lenses
    provisionByoipCidr_cidr,
    provisionByoipCidr_cidrAuthorizationContext,

    -- * Destructuring the Response
    ProvisionByoipCidrResponse (..),
    newProvisionByoipCidrResponse,

    -- * Response Lenses
    provisionByoipCidrResponse_byoipCidr,
    provisionByoipCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newProvisionByoipCidr' smart constructor.
data ProvisionByoipCidr = ProvisionByoipCidr'
  { -- | The public IPv4 address range, in CIDR notation. The most specific IP
    -- prefix that you can specify is \/24. The address range cannot overlap
    -- with another address range that you\'ve brought to this or another
    -- Region.
    cidr :: Prelude.Text,
    -- | A signed document that proves that you are authorized to bring the
    -- specified IP address range to Amazon using BYOIP.
    cidrAuthorizationContext :: CidrAuthorizationContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'provisionByoipCidr_cidr' - The public IPv4 address range, in CIDR notation. The most specific IP
-- prefix that you can specify is \/24. The address range cannot overlap
-- with another address range that you\'ve brought to this or another
-- Region.
--
-- 'cidrAuthorizationContext', 'provisionByoipCidr_cidrAuthorizationContext' - A signed document that proves that you are authorized to bring the
-- specified IP address range to Amazon using BYOIP.
newProvisionByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  -- | 'cidrAuthorizationContext'
  CidrAuthorizationContext ->
  ProvisionByoipCidr
newProvisionByoipCidr
  pCidr_
  pCidrAuthorizationContext_ =
    ProvisionByoipCidr'
      { cidr = pCidr_,
        cidrAuthorizationContext =
          pCidrAuthorizationContext_
      }

-- | The public IPv4 address range, in CIDR notation. The most specific IP
-- prefix that you can specify is \/24. The address range cannot overlap
-- with another address range that you\'ve brought to this or another
-- Region.
provisionByoipCidr_cidr :: Lens.Lens' ProvisionByoipCidr Prelude.Text
provisionByoipCidr_cidr = Lens.lens (\ProvisionByoipCidr' {cidr} -> cidr) (\s@ProvisionByoipCidr' {} a -> s {cidr = a} :: ProvisionByoipCidr)

-- | A signed document that proves that you are authorized to bring the
-- specified IP address range to Amazon using BYOIP.
provisionByoipCidr_cidrAuthorizationContext :: Lens.Lens' ProvisionByoipCidr CidrAuthorizationContext
provisionByoipCidr_cidrAuthorizationContext = Lens.lens (\ProvisionByoipCidr' {cidrAuthorizationContext} -> cidrAuthorizationContext) (\s@ProvisionByoipCidr' {} a -> s {cidrAuthorizationContext = a} :: ProvisionByoipCidr)

instance Core.AWSRequest ProvisionByoipCidr where
  type
    AWSResponse ProvisionByoipCidr =
      ProvisionByoipCidrResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvisionByoipCidrResponse'
            Prelude.<$> (x Data..?> "ByoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ProvisionByoipCidr where
  hashWithSalt _salt ProvisionByoipCidr' {..} =
    _salt
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` cidrAuthorizationContext

instance Prelude.NFData ProvisionByoipCidr where
  rnf ProvisionByoipCidr' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf cidrAuthorizationContext

instance Data.ToHeaders ProvisionByoipCidr where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.ProvisionByoipCidr" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ProvisionByoipCidr where
  toJSON ProvisionByoipCidr' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Cidr" Data..= cidr),
            Prelude.Just
              ( "CidrAuthorizationContext"
                  Data..= cidrAuthorizationContext
              )
          ]
      )

instance Data.ToPath ProvisionByoipCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery ProvisionByoipCidr where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newProvisionByoipCidrResponse' smart constructor.
data ProvisionByoipCidrResponse = ProvisionByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'provisionByoipCidrResponse_byoipCidr' - Information about the address range.
--
-- 'httpStatus', 'provisionByoipCidrResponse_httpStatus' - The response's http status code.
newProvisionByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ProvisionByoipCidrResponse
newProvisionByoipCidrResponse pHttpStatus_ =
  ProvisionByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address range.
provisionByoipCidrResponse_byoipCidr :: Lens.Lens' ProvisionByoipCidrResponse (Prelude.Maybe ByoipCidr)
provisionByoipCidrResponse_byoipCidr = Lens.lens (\ProvisionByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@ProvisionByoipCidrResponse' {} a -> s {byoipCidr = a} :: ProvisionByoipCidrResponse)

-- | The response's http status code.
provisionByoipCidrResponse_httpStatus :: Lens.Lens' ProvisionByoipCidrResponse Prelude.Int
provisionByoipCidrResponse_httpStatus = Lens.lens (\ProvisionByoipCidrResponse' {httpStatus} -> httpStatus) (\s@ProvisionByoipCidrResponse' {} a -> s {httpStatus = a} :: ProvisionByoipCidrResponse)

instance Prelude.NFData ProvisionByoipCidrResponse where
  rnf ProvisionByoipCidrResponse' {..} =
    Prelude.rnf byoipCidr
      `Prelude.seq` Prelude.rnf httpStatus
