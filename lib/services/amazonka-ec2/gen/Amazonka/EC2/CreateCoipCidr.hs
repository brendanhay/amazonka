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
-- Module      : Amazonka.EC2.CreateCoipCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a range of customer-owned IP addresses.
module Amazonka.EC2.CreateCoipCidr
  ( -- * Creating a Request
    CreateCoipCidr (..),
    newCreateCoipCidr,

    -- * Request Lenses
    createCoipCidr_dryRun,
    createCoipCidr_cidr,
    createCoipCidr_coipPoolId,

    -- * Destructuring the Response
    CreateCoipCidrResponse (..),
    newCreateCoipCidrResponse,

    -- * Response Lenses
    createCoipCidrResponse_coipCidr,
    createCoipCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCoipCidr' smart constructor.
data CreateCoipCidr = CreateCoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A customer-owned IP address range to create.
    cidr :: Prelude.Text,
    -- | The ID of the address pool.
    coipPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createCoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'createCoipCidr_cidr' - A customer-owned IP address range to create.
--
-- 'coipPoolId', 'createCoipCidr_coipPoolId' - The ID of the address pool.
newCreateCoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  -- | 'coipPoolId'
  Prelude.Text ->
  CreateCoipCidr
newCreateCoipCidr pCidr_ pCoipPoolId_ =
  CreateCoipCidr'
    { dryRun = Prelude.Nothing,
      cidr = pCidr_,
      coipPoolId = pCoipPoolId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createCoipCidr_dryRun :: Lens.Lens' CreateCoipCidr (Prelude.Maybe Prelude.Bool)
createCoipCidr_dryRun = Lens.lens (\CreateCoipCidr' {dryRun} -> dryRun) (\s@CreateCoipCidr' {} a -> s {dryRun = a} :: CreateCoipCidr)

-- | A customer-owned IP address range to create.
createCoipCidr_cidr :: Lens.Lens' CreateCoipCidr Prelude.Text
createCoipCidr_cidr = Lens.lens (\CreateCoipCidr' {cidr} -> cidr) (\s@CreateCoipCidr' {} a -> s {cidr = a} :: CreateCoipCidr)

-- | The ID of the address pool.
createCoipCidr_coipPoolId :: Lens.Lens' CreateCoipCidr Prelude.Text
createCoipCidr_coipPoolId = Lens.lens (\CreateCoipCidr' {coipPoolId} -> coipPoolId) (\s@CreateCoipCidr' {} a -> s {coipPoolId = a} :: CreateCoipCidr)

instance Core.AWSRequest CreateCoipCidr where
  type
    AWSResponse CreateCoipCidr =
      CreateCoipCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCoipCidrResponse'
            Prelude.<$> (x Data..@? "coipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCoipCidr where
  hashWithSalt _salt CreateCoipCidr' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` coipPoolId

instance Prelude.NFData CreateCoipCidr where
  rnf CreateCoipCidr' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf cidr `Prelude.seq`
        Prelude.rnf coipPoolId

instance Data.ToHeaders CreateCoipCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCoipCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCoipCidr where
  toQuery CreateCoipCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateCoipCidr" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Cidr" Data.=: cidr,
        "CoipPoolId" Data.=: coipPoolId
      ]

-- | /See:/ 'newCreateCoipCidrResponse' smart constructor.
data CreateCoipCidrResponse = CreateCoipCidrResponse'
  { -- | Information about a range of customer-owned IP addresses.
    coipCidr :: Prelude.Maybe CoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coipCidr', 'createCoipCidrResponse_coipCidr' - Information about a range of customer-owned IP addresses.
--
-- 'httpStatus', 'createCoipCidrResponse_httpStatus' - The response's http status code.
newCreateCoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCoipCidrResponse
newCreateCoipCidrResponse pHttpStatus_ =
  CreateCoipCidrResponse'
    { coipCidr = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a range of customer-owned IP addresses.
createCoipCidrResponse_coipCidr :: Lens.Lens' CreateCoipCidrResponse (Prelude.Maybe CoipCidr)
createCoipCidrResponse_coipCidr = Lens.lens (\CreateCoipCidrResponse' {coipCidr} -> coipCidr) (\s@CreateCoipCidrResponse' {} a -> s {coipCidr = a} :: CreateCoipCidrResponse)

-- | The response's http status code.
createCoipCidrResponse_httpStatus :: Lens.Lens' CreateCoipCidrResponse Prelude.Int
createCoipCidrResponse_httpStatus = Lens.lens (\CreateCoipCidrResponse' {httpStatus} -> httpStatus) (\s@CreateCoipCidrResponse' {} a -> s {httpStatus = a} :: CreateCoipCidrResponse)

instance Prelude.NFData CreateCoipCidrResponse where
  rnf CreateCoipCidrResponse' {..} =
    Prelude.rnf coipCidr `Prelude.seq`
      Prelude.rnf httpStatus
