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
-- Module      : Amazonka.WorkSpaces.DescribeAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the configuration of Bring Your Own
-- License (BYOL) for the specified account.
module Amazonka.WorkSpaces.DescribeAccount
  ( -- * Creating a Request
    DescribeAccount (..),
    newDescribeAccount,

    -- * Destructuring the Response
    DescribeAccountResponse (..),
    newDescribeAccountResponse,

    -- * Response Lenses
    describeAccountResponse_dedicatedTenancyManagementCidrRange,
    describeAccountResponse_dedicatedTenancySupport,
    describeAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Prelude.<$> (x Data..?> "DedicatedTenancyManagementCidrRange")
            Prelude.<*> (x Data..?> "DedicatedTenancySupport")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeAccount where
  rnf _ = ()

instance Data.ToHeaders DescribeAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccount where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { -- | The IP address range, specified as an IPv4 CIDR block, used for the
    -- management network interface.
    --
    -- The management network interface is connected to a secure Amazon
    -- WorkSpaces management network. It is used for interactive streaming of
    -- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
    -- WorkSpaces to manage the WorkSpace.
    dedicatedTenancyManagementCidrRange :: Prelude.Maybe Prelude.Text,
    -- | The status of BYOL (whether BYOL is enabled or disabled).
    dedicatedTenancySupport :: Prelude.Maybe DedicatedTenancySupportResultEnum,
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
-- 'dedicatedTenancyManagementCidrRange', 'describeAccountResponse_dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, used for the
-- management network interface.
--
-- The management network interface is connected to a secure Amazon
-- WorkSpaces management network. It is used for interactive streaming of
-- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
-- WorkSpaces to manage the WorkSpace.
--
-- 'dedicatedTenancySupport', 'describeAccountResponse_dedicatedTenancySupport' - The status of BYOL (whether BYOL is enabled or disabled).
--
-- 'httpStatus', 'describeAccountResponse_httpStatus' - The response's http status code.
newDescribeAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountResponse
newDescribeAccountResponse pHttpStatus_ =
  DescribeAccountResponse'
    { dedicatedTenancyManagementCidrRange =
        Prelude.Nothing,
      dedicatedTenancySupport = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IP address range, specified as an IPv4 CIDR block, used for the
-- management network interface.
--
-- The management network interface is connected to a secure Amazon
-- WorkSpaces management network. It is used for interactive streaming of
-- the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon
-- WorkSpaces to manage the WorkSpace.
describeAccountResponse_dedicatedTenancyManagementCidrRange :: Lens.Lens' DescribeAccountResponse (Prelude.Maybe Prelude.Text)
describeAccountResponse_dedicatedTenancyManagementCidrRange = Lens.lens (\DescribeAccountResponse' {dedicatedTenancyManagementCidrRange} -> dedicatedTenancyManagementCidrRange) (\s@DescribeAccountResponse' {} a -> s {dedicatedTenancyManagementCidrRange = a} :: DescribeAccountResponse)

-- | The status of BYOL (whether BYOL is enabled or disabled).
describeAccountResponse_dedicatedTenancySupport :: Lens.Lens' DescribeAccountResponse (Prelude.Maybe DedicatedTenancySupportResultEnum)
describeAccountResponse_dedicatedTenancySupport = Lens.lens (\DescribeAccountResponse' {dedicatedTenancySupport} -> dedicatedTenancySupport) (\s@DescribeAccountResponse' {} a -> s {dedicatedTenancySupport = a} :: DescribeAccountResponse)

-- | The response's http status code.
describeAccountResponse_httpStatus :: Lens.Lens' DescribeAccountResponse Prelude.Int
describeAccountResponse_httpStatus = Lens.lens (\DescribeAccountResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountResponse' {} a -> s {httpStatus = a} :: DescribeAccountResponse)

instance Prelude.NFData DescribeAccountResponse where
  rnf DescribeAccountResponse' {..} =
    Prelude.rnf dedicatedTenancyManagementCidrRange
      `Prelude.seq` Prelude.rnf dedicatedTenancySupport
      `Prelude.seq` Prelude.rnf httpStatus
