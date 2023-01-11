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
-- Module      : Amazonka.EC2.WithdrawByoipCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops advertising an address range that is provisioned as an address
-- pool.
--
-- You can perform this operation at most once every 10 seconds, even if
-- you specify different address ranges each time.
--
-- It can take a few minutes before traffic to the specified addresses
-- stops routing to Amazon Web Services because of BGP propagation delays.
module Amazonka.EC2.WithdrawByoipCidr
  ( -- * Creating a Request
    WithdrawByoipCidr (..),
    newWithdrawByoipCidr,

    -- * Request Lenses
    withdrawByoipCidr_dryRun,
    withdrawByoipCidr_cidr,

    -- * Destructuring the Response
    WithdrawByoipCidrResponse (..),
    newWithdrawByoipCidrResponse,

    -- * Response Lenses
    withdrawByoipCidrResponse_byoipCidr,
    withdrawByoipCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newWithdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The address range, in CIDR notation.
    cidr :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WithdrawByoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'withdrawByoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'withdrawByoipCidr_cidr' - The address range, in CIDR notation.
newWithdrawByoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  WithdrawByoipCidr
newWithdrawByoipCidr pCidr_ =
  WithdrawByoipCidr'
    { dryRun = Prelude.Nothing,
      cidr = pCidr_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
withdrawByoipCidr_dryRun :: Lens.Lens' WithdrawByoipCidr (Prelude.Maybe Prelude.Bool)
withdrawByoipCidr_dryRun = Lens.lens (\WithdrawByoipCidr' {dryRun} -> dryRun) (\s@WithdrawByoipCidr' {} a -> s {dryRun = a} :: WithdrawByoipCidr)

-- | The address range, in CIDR notation.
withdrawByoipCidr_cidr :: Lens.Lens' WithdrawByoipCidr Prelude.Text
withdrawByoipCidr_cidr = Lens.lens (\WithdrawByoipCidr' {cidr} -> cidr) (\s@WithdrawByoipCidr' {} a -> s {cidr = a} :: WithdrawByoipCidr)

instance Core.AWSRequest WithdrawByoipCidr where
  type
    AWSResponse WithdrawByoipCidr =
      WithdrawByoipCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          WithdrawByoipCidrResponse'
            Prelude.<$> (x Data..@? "byoipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable WithdrawByoipCidr where
  hashWithSalt _salt WithdrawByoipCidr' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidr

instance Prelude.NFData WithdrawByoipCidr where
  rnf WithdrawByoipCidr' {..} =
    Prelude.rnf dryRun `Prelude.seq` Prelude.rnf cidr

instance Data.ToHeaders WithdrawByoipCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath WithdrawByoipCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery WithdrawByoipCidr where
  toQuery WithdrawByoipCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("WithdrawByoipCidr" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Cidr" Data.=: cidr
      ]

-- | /See:/ 'newWithdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { -- | Information about the address pool.
    byoipCidr :: Prelude.Maybe ByoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WithdrawByoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byoipCidr', 'withdrawByoipCidrResponse_byoipCidr' - Information about the address pool.
--
-- 'httpStatus', 'withdrawByoipCidrResponse_httpStatus' - The response's http status code.
newWithdrawByoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  WithdrawByoipCidrResponse
newWithdrawByoipCidrResponse pHttpStatus_ =
  WithdrawByoipCidrResponse'
    { byoipCidr =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address pool.
withdrawByoipCidrResponse_byoipCidr :: Lens.Lens' WithdrawByoipCidrResponse (Prelude.Maybe ByoipCidr)
withdrawByoipCidrResponse_byoipCidr = Lens.lens (\WithdrawByoipCidrResponse' {byoipCidr} -> byoipCidr) (\s@WithdrawByoipCidrResponse' {} a -> s {byoipCidr = a} :: WithdrawByoipCidrResponse)

-- | The response's http status code.
withdrawByoipCidrResponse_httpStatus :: Lens.Lens' WithdrawByoipCidrResponse Prelude.Int
withdrawByoipCidrResponse_httpStatus = Lens.lens (\WithdrawByoipCidrResponse' {httpStatus} -> httpStatus) (\s@WithdrawByoipCidrResponse' {} a -> s {httpStatus = a} :: WithdrawByoipCidrResponse)

instance Prelude.NFData WithdrawByoipCidrResponse where
  rnf WithdrawByoipCidrResponse' {..} =
    Prelude.rnf byoipCidr
      `Prelude.seq` Prelude.rnf httpStatus
