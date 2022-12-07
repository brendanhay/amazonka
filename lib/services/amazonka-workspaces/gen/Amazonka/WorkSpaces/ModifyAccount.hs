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
-- Module      : Amazonka.WorkSpaces.ModifyAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of Bring Your Own License (BYOL) for the
-- specified account.
module Amazonka.WorkSpaces.ModifyAccount
  ( -- * Creating a Request
    ModifyAccount (..),
    newModifyAccount,

    -- * Request Lenses
    modifyAccount_dedicatedTenancyManagementCidrRange,
    modifyAccount_dedicatedTenancySupport,

    -- * Destructuring the Response
    ModifyAccountResponse (..),
    newModifyAccountResponse,

    -- * Response Lenses
    modifyAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newModifyAccount' smart constructor.
data ModifyAccount = ModifyAccount'
  { -- | The IP address range, specified as an IPv4 CIDR block, for the
    -- management network interface. Specify an IP address range that is
    -- compatible with your network and in CIDR notation (that is, specify the
    -- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
    -- example, 203.0.113.25\/16). It must also be specified as available by
    -- the @ListAvailableManagementCidrRanges@ operation.
    dedicatedTenancyManagementCidrRange :: Prelude.Maybe Prelude.Text,
    -- | The status of BYOL.
    dedicatedTenancySupport :: Prelude.Maybe DedicatedTenancySupportEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedTenancyManagementCidrRange', 'modifyAccount_dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
-- example, 203.0.113.25\/16). It must also be specified as available by
-- the @ListAvailableManagementCidrRanges@ operation.
--
-- 'dedicatedTenancySupport', 'modifyAccount_dedicatedTenancySupport' - The status of BYOL.
newModifyAccount ::
  ModifyAccount
newModifyAccount =
  ModifyAccount'
    { dedicatedTenancyManagementCidrRange =
        Prelude.Nothing,
      dedicatedTenancySupport = Prelude.Nothing
    }

-- | The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
-- example, 203.0.113.25\/16). It must also be specified as available by
-- the @ListAvailableManagementCidrRanges@ operation.
modifyAccount_dedicatedTenancyManagementCidrRange :: Lens.Lens' ModifyAccount (Prelude.Maybe Prelude.Text)
modifyAccount_dedicatedTenancyManagementCidrRange = Lens.lens (\ModifyAccount' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@ModifyAccount' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: ModifyAccount)

-- | The status of BYOL.
modifyAccount_dedicatedTenancySupport :: Lens.Lens' ModifyAccount (Prelude.Maybe DedicatedTenancySupportEnum)
modifyAccount_dedicatedTenancySupport = Lens.lens (\ModifyAccount' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@ModifyAccount' {} a -> s {dedicatedTenancySupport = a} :: ModifyAccount)

instance Core.AWSRequest ModifyAccount where
  type
    AWSResponse ModifyAccount =
      ModifyAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAccount where
  hashWithSalt _salt ModifyAccount' {..} =
    _salt
      `Prelude.hashWithSalt` dedicatedTenancyManagementCidrRange
      `Prelude.hashWithSalt` dedicatedTenancySupport

instance Prelude.NFData ModifyAccount where
  rnf ModifyAccount' {..} =
    Prelude.rnf dedicatedTenancyManagementCidrRange
      `Prelude.seq` Prelude.rnf dedicatedTenancySupport

instance Data.ToHeaders ModifyAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.ModifyAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyAccount where
  toJSON ModifyAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DedicatedTenancyManagementCidrRange" Data..=)
              Prelude.<$> dedicatedTenancyManagementCidrRange,
            ("DedicatedTenancySupport" Data..=)
              Prelude.<$> dedicatedTenancySupport
          ]
      )

instance Data.ToPath ModifyAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyAccountResponse' smart constructor.
data ModifyAccountResponse = ModifyAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyAccountResponse_httpStatus' - The response's http status code.
newModifyAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyAccountResponse
newModifyAccountResponse pHttpStatus_ =
  ModifyAccountResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
modifyAccountResponse_httpStatus :: Lens.Lens' ModifyAccountResponse Prelude.Int
modifyAccountResponse_httpStatus = Lens.lens (\ModifyAccountResponse' {httpStatus} -> httpStatus) (\s@ModifyAccountResponse' {} a -> s {httpStatus = a} :: ModifyAccountResponse)

instance Prelude.NFData ModifyAccountResponse where
  rnf ModifyAccountResponse' {..} =
    Prelude.rnf httpStatus
