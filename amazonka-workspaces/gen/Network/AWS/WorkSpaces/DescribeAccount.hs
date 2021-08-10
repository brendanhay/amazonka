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
-- Module      : Network.AWS.WorkSpaces.DescribeAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the configuration of Bring Your Own
-- License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.DescribeAccount
  ( -- * Creating a Request
    DescribeAccount (..),
    newDescribeAccount,

    -- * Destructuring the Response
    DescribeAccountResponse (..),
    newDescribeAccountResponse,

    -- * Response Lenses
    describeAccountResponse_dedicatedTenancySupport,
    describeAccountResponse_dedicatedTenancyManagementCidrRange,
    describeAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeAccount' smart constructor.
data DescribeAccount = DescribeAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccount ::
  DescribeAccount
newDescribeAccount = DescribeAccount'

instance Core.AWSRequest DescribeAccount where
  type
    AWSResponse DescribeAccount =
      DescribeAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Prelude.<$> (x Core..?> "DedicatedTenancySupport")
            Prelude.<*> (x Core..?> "DedicatedTenancyManagementCidrRange")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccount

instance Prelude.NFData DescribeAccount

instance Core.ToHeaders DescribeAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAccount where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeAccount where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { -- | The status of BYOL (whether BYOL is enabled or disabled).
    dedicatedTenancySupport :: Prelude.Maybe DedicatedTenancySupportResultEnum,
    -- | The IP address range, specified as an IPv4 CIDR block, used for the
    -- management network interface.
    --
    -- The management network interface is connected to a secure Amazon
    -- WorkSpaces management network. It is used for interactive streaming of
    -- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
    -- WorkSpaces to manage the WorkSpace.
    dedicatedTenancyManagementCidrRange :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedTenancySupport', 'describeAccountResponse_dedicatedTenancySupport' - The status of BYOL (whether BYOL is enabled or disabled).
--
-- 'dedicatedTenancyManagementCidrRange', 'describeAccountResponse_dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, used for the
-- management network interface.
--
-- The management network interface is connected to a secure Amazon
-- WorkSpaces management network. It is used for interactive streaming of
-- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
-- WorkSpaces to manage the WorkSpace.
--
-- 'httpStatus', 'describeAccountResponse_httpStatus' - The response's http status code.
newDescribeAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountResponse
newDescribeAccountResponse pHttpStatus_ =
  DescribeAccountResponse'
    { dedicatedTenancySupport =
        Prelude.Nothing,
      dedicatedTenancyManagementCidrRange =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of BYOL (whether BYOL is enabled or disabled).
describeAccountResponse_dedicatedTenancySupport :: Lens.Lens' DescribeAccountResponse (Prelude.Maybe DedicatedTenancySupportResultEnum)
describeAccountResponse_dedicatedTenancySupport = Lens.lens (\DescribeAccountResponse' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@DescribeAccountResponse' {} a -> s {dedicatedTenancySupport = a} :: DescribeAccountResponse)

-- | The IP address range, specified as an IPv4 CIDR block, used for the
-- management network interface.
--
-- The management network interface is connected to a secure Amazon
-- WorkSpaces management network. It is used for interactive streaming of
-- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
-- WorkSpaces to manage the WorkSpace.
describeAccountResponse_dedicatedTenancyManagementCidrRange :: Lens.Lens' DescribeAccountResponse (Prelude.Maybe Prelude.Text)
describeAccountResponse_dedicatedTenancyManagementCidrRange = Lens.lens (\DescribeAccountResponse' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@DescribeAccountResponse' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: DescribeAccountResponse)

-- | The response's http status code.
describeAccountResponse_httpStatus :: Lens.Lens' DescribeAccountResponse Prelude.Int
describeAccountResponse_httpStatus = Lens.lens (\DescribeAccountResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountResponse' {} a -> s {httpStatus = a} :: DescribeAccountResponse)

instance Prelude.NFData DescribeAccountResponse
