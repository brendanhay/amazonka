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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyAccount' smart constructor.
data ModifyAccount = ModifyAccount'
  { -- | The status of BYOL.
    dedicatedTenancySupport :: Core.Maybe DedicatedTenancySupportEnum,
    -- | The IP address range, specified as an IPv4 CIDR block, for the
    -- management network interface. Specify an IP address range that is
    -- compatible with your network and in CIDR notation (that is, specify the
    -- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
    -- example, 203.0.113.25\/16). It must also be specified as available by
    -- the @ListAvailableManagementCidrRanges@ operation.
    dedicatedTenancyManagementCidrRange :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      dedicatedTenancyManagementCidrRange = Core.Nothing
    }

-- | The status of BYOL.
modifyAccount_dedicatedTenancySupport :: Lens.Lens' ModifyAccount (Core.Maybe DedicatedTenancySupportEnum)
modifyAccount_dedicatedTenancySupport = Lens.lens (\ModifyAccount' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@ModifyAccount' {} a -> s {dedicatedTenancySupport = a} :: ModifyAccount)

-- | The IP address range, specified as an IPv4 CIDR block, for the
-- management network interface. Specify an IP address range that is
-- compatible with your network and in CIDR notation (that is, specify the
-- range as an IPv4 CIDR block). The CIDR block size must be \/16 (for
-- example, 203.0.113.25\/16). It must also be specified as available by
-- the @ListAvailableManagementCidrRanges@ operation.
modifyAccount_dedicatedTenancyManagementCidrRange :: Lens.Lens' ModifyAccount (Core.Maybe Core.Text)
modifyAccount_dedicatedTenancyManagementCidrRange = Lens.lens (\ModifyAccount' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@ModifyAccount' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: ModifyAccount)

instance Core.AWSRequest ModifyAccount where
  type
    AWSResponse ModifyAccount =
      ModifyAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyAccount

instance Core.NFData ModifyAccount

instance Core.ToHeaders ModifyAccount where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ModifyAccount" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyAccount where
  toJSON ModifyAccount' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DedicatedTenancySupport" Core..=)
              Core.<$> dedicatedTenancySupport,
            ("DedicatedTenancyManagementCidrRange" Core..=)
              Core.<$> dedicatedTenancyManagementCidrRange
          ]
      )

instance Core.ToPath ModifyAccount where
  toPath = Core.const "/"

instance Core.ToQuery ModifyAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyAccountResponse' smart constructor.
data ModifyAccountResponse = ModifyAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyAccountResponse
newModifyAccountResponse pHttpStatus_ =
  ModifyAccountResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
modifyAccountResponse_httpStatus :: Lens.Lens' ModifyAccountResponse Core.Int
modifyAccountResponse_httpStatus = Lens.lens (\ModifyAccountResponse' {httpStatus} -> httpStatus) (\s@ModifyAccountResponse' {} a -> s {httpStatus = a} :: ModifyAccountResponse)

instance Core.NFData ModifyAccountResponse
