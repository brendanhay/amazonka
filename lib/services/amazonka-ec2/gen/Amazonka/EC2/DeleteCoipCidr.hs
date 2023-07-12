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
-- Module      : Amazonka.EC2.DeleteCoipCidr
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a range of customer-owned IP addresses.
module Amazonka.EC2.DeleteCoipCidr
  ( -- * Creating a Request
    DeleteCoipCidr (..),
    newDeleteCoipCidr,

    -- * Request Lenses
    deleteCoipCidr_dryRun,
    deleteCoipCidr_cidr,
    deleteCoipCidr_coipPoolId,

    -- * Destructuring the Response
    DeleteCoipCidrResponse (..),
    newDeleteCoipCidrResponse,

    -- * Response Lenses
    deleteCoipCidrResponse_coipCidr,
    deleteCoipCidrResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCoipCidr' smart constructor.
data DeleteCoipCidr = DeleteCoipCidr'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A customer-owned IP address range that you want to delete.
    cidr :: Prelude.Text,
    -- | The ID of the customer-owned address pool.
    coipPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoipCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteCoipCidr_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'cidr', 'deleteCoipCidr_cidr' - A customer-owned IP address range that you want to delete.
--
-- 'coipPoolId', 'deleteCoipCidr_coipPoolId' - The ID of the customer-owned address pool.
newDeleteCoipCidr ::
  -- | 'cidr'
  Prelude.Text ->
  -- | 'coipPoolId'
  Prelude.Text ->
  DeleteCoipCidr
newDeleteCoipCidr pCidr_ pCoipPoolId_ =
  DeleteCoipCidr'
    { dryRun = Prelude.Nothing,
      cidr = pCidr_,
      coipPoolId = pCoipPoolId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteCoipCidr_dryRun :: Lens.Lens' DeleteCoipCidr (Prelude.Maybe Prelude.Bool)
deleteCoipCidr_dryRun = Lens.lens (\DeleteCoipCidr' {dryRun} -> dryRun) (\s@DeleteCoipCidr' {} a -> s {dryRun = a} :: DeleteCoipCidr)

-- | A customer-owned IP address range that you want to delete.
deleteCoipCidr_cidr :: Lens.Lens' DeleteCoipCidr Prelude.Text
deleteCoipCidr_cidr = Lens.lens (\DeleteCoipCidr' {cidr} -> cidr) (\s@DeleteCoipCidr' {} a -> s {cidr = a} :: DeleteCoipCidr)

-- | The ID of the customer-owned address pool.
deleteCoipCidr_coipPoolId :: Lens.Lens' DeleteCoipCidr Prelude.Text
deleteCoipCidr_coipPoolId = Lens.lens (\DeleteCoipCidr' {coipPoolId} -> coipPoolId) (\s@DeleteCoipCidr' {} a -> s {coipPoolId = a} :: DeleteCoipCidr)

instance Core.AWSRequest DeleteCoipCidr where
  type
    AWSResponse DeleteCoipCidr =
      DeleteCoipCidrResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteCoipCidrResponse'
            Prelude.<$> (x Data..@? "coipCidr")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCoipCidr where
  hashWithSalt _salt DeleteCoipCidr' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` coipPoolId

instance Prelude.NFData DeleteCoipCidr where
  rnf DeleteCoipCidr' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf coipPoolId

instance Data.ToHeaders DeleteCoipCidr where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCoipCidr where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCoipCidr where
  toQuery DeleteCoipCidr' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCoipCidr" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Cidr" Data.=: cidr,
        "CoipPoolId" Data.=: coipPoolId
      ]

-- | /See:/ 'newDeleteCoipCidrResponse' smart constructor.
data DeleteCoipCidrResponse = DeleteCoipCidrResponse'
  { -- | Information about a range of customer-owned IP addresses.
    coipCidr :: Prelude.Maybe CoipCidr,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoipCidrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coipCidr', 'deleteCoipCidrResponse_coipCidr' - Information about a range of customer-owned IP addresses.
--
-- 'httpStatus', 'deleteCoipCidrResponse_httpStatus' - The response's http status code.
newDeleteCoipCidrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCoipCidrResponse
newDeleteCoipCidrResponse pHttpStatus_ =
  DeleteCoipCidrResponse'
    { coipCidr = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a range of customer-owned IP addresses.
deleteCoipCidrResponse_coipCidr :: Lens.Lens' DeleteCoipCidrResponse (Prelude.Maybe CoipCidr)
deleteCoipCidrResponse_coipCidr = Lens.lens (\DeleteCoipCidrResponse' {coipCidr} -> coipCidr) (\s@DeleteCoipCidrResponse' {} a -> s {coipCidr = a} :: DeleteCoipCidrResponse)

-- | The response's http status code.
deleteCoipCidrResponse_httpStatus :: Lens.Lens' DeleteCoipCidrResponse Prelude.Int
deleteCoipCidrResponse_httpStatus = Lens.lens (\DeleteCoipCidrResponse' {httpStatus} -> httpStatus) (\s@DeleteCoipCidrResponse' {} a -> s {httpStatus = a} :: DeleteCoipCidrResponse)

instance Prelude.NFData DeleteCoipCidrResponse where
  rnf DeleteCoipCidrResponse' {..} =
    Prelude.rnf coipCidr
      `Prelude.seq` Prelude.rnf httpStatus
