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
-- Module      : Network.AWS.WorkSpaces.ModifyAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of Bring Your Own License (BYOL) for the
-- specified account.
module Network.AWS.WorkSpaces.ModifyAccount
  ( -- * Creating a Request
    ModifyAccount (..),
    newModifyAccount,

    -- * Request Lenses
    modifyAccount_dedicatedTenancySupport,
    modifyAccount_dedicatedTenancyManagementCidrRange,

    -- * Destructuring the Response
    ModifyAccountResponse (..),
    newModifyAccountResponse,

    -- * Response Lenses
    modifyAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyAccount' smart constructor.
data ModifyAccount = ModifyAccount'
  { -- | The status of BYOL.
    dedicatedTenancySupport :: Prelude.Maybe DedicatedTenancySupportEnum,
    -- | The IP address range, specified as an IPv4 CIDR block, for the
    -- management network interface. Specify an IP address range that is
    -- compatible with your network and in CIDR notation (that is, specify the
    -- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
    -- example, 203.0.113.25\/16). It must also be specified as available by
    -- the @ListAvailableManagementCidrRanges@ operation.
    dedicatedTenancyManagementCidrRange :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedTenancySupport', 'modifyAccount_dedicatedTenancySupport' - The status of BYOL.
--
-- 'dedicatedTenancyManagementCidrRange', 'modifyAccount_dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
-- example, 203.0.113.25\/16). It must also be specified as available by
-- the @ListAvailableManagementCidrRanges@ operation.
newModifyAccount ::
  ModifyAccount
newModifyAccount =
  ModifyAccount'
    { dedicatedTenancySupport =
        Prelude.Nothing,
      dedicatedTenancyManagementCidrRange =
        Prelude.Nothing
    }

-- | The status of BYOL.
modifyAccount_dedicatedTenancySupport :: Lens.Lens' ModifyAccount (Prelude.Maybe DedicatedTenancySupportEnum)
modifyAccount_dedicatedTenancySupport = Lens.lens (\ModifyAccount' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@ModifyAccount' {} a -> s {dedicatedTenancySupport = a} :: ModifyAccount)

-- | The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
-- example, 203.0.113.25\/16). It must also be specified as available by
-- the @ListAvailableManagementCidrRanges@ operation.
modifyAccount_dedicatedTenancyManagementCidrRange :: Lens.Lens' ModifyAccount (Prelude.Maybe Prelude.Text)
modifyAccount_dedicatedTenancyManagementCidrRange = Lens.lens (\ModifyAccount' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@ModifyAccount' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: ModifyAccount)

instance Prelude.AWSRequest ModifyAccount where
  type Rs ModifyAccount = ModifyAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAccount

instance Prelude.NFData ModifyAccount

instance Prelude.ToHeaders ModifyAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.ModifyAccount" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyAccount where
  toJSON ModifyAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DedicatedTenancySupport" Prelude..=)
              Prelude.<$> dedicatedTenancySupport,
            ("DedicatedTenancyManagementCidrRange" Prelude..=)
              Prelude.<$> dedicatedTenancyManagementCidrRange
          ]
      )

instance Prelude.ToPath ModifyAccount where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyAccountResponse' smart constructor.
data ModifyAccountResponse = ModifyAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ModifyAccountResponse
