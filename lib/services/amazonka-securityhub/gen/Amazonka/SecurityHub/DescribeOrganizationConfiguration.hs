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
-- Module      : Amazonka.SecurityHub.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Organizations configuration for Security
-- Hub. Can only be called from a Security Hub administrator account.
module Amazonka.SecurityHub.DescribeOrganizationConfiguration
  ( -- * Creating a Request
    DescribeOrganizationConfiguration (..),
    newDescribeOrganizationConfiguration,

    -- * Destructuring the Response
    DescribeOrganizationConfigurationResponse (..),
    newDescribeOrganizationConfigurationResponse,

    -- * Response Lenses
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_autoEnableStandards,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,
    describeOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDescribeOrganizationConfiguration' smart constructor.
data DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeOrganizationConfiguration ::
  DescribeOrganizationConfiguration
newDescribeOrganizationConfiguration =
  DescribeOrganizationConfiguration'

instance
  Core.AWSRequest
    DescribeOrganizationConfiguration
  where
  type
    AWSResponse DescribeOrganizationConfiguration =
      DescribeOrganizationConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigurationResponse'
            Prelude.<$> (x Data..?> "AutoEnable")
              Prelude.<*> (x Data..?> "AutoEnableStandards")
              Prelude.<*> (x Data..?> "MemberAccountLimitReached")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DescribeOrganizationConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DescribeOrganizationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DescribeOrganizationConfiguration
  where
  toPath = Prelude.const "/organization/configuration"

instance
  Data.ToQuery
    DescribeOrganizationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { -- | Whether to automatically enable Security Hub for new accounts in the
    -- organization.
    --
    -- If set to @true@, then Security Hub is enabled for new accounts. If set
    -- to false, then new accounts are not added automatically.
    autoEnable :: Prelude.Maybe Prelude.Bool,
    -- | Whether to automatically enable Security Hub
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-enable-disable.html default standards>
    -- for new member accounts in the organization.
    --
    -- The default value of this parameter is equal to @DEFAULT@.
    --
    -- If equal to @DEFAULT@, then Security Hub default standards are
    -- automatically enabled for new member accounts. If equal to @NONE@, then
    -- default standards are not automatically enabled for new member accounts.
    autoEnableStandards :: Prelude.Maybe AutoEnableStandards,
    -- | Whether the maximum number of allowed member accounts are already
    -- associated with the Security Hub administrator account.
    memberAccountLimitReached :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'describeOrganizationConfigurationResponse_autoEnable' - Whether to automatically enable Security Hub for new accounts in the
-- organization.
--
-- If set to @true@, then Security Hub is enabled for new accounts. If set
-- to false, then new accounts are not added automatically.
--
-- 'autoEnableStandards', 'describeOrganizationConfigurationResponse_autoEnableStandards' - Whether to automatically enable Security Hub
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-enable-disable.html default standards>
-- for new member accounts in the organization.
--
-- The default value of this parameter is equal to @DEFAULT@.
--
-- If equal to @DEFAULT@, then Security Hub default standards are
-- automatically enabled for new member accounts. If equal to @NONE@, then
-- default standards are not automatically enabled for new member accounts.
--
-- 'memberAccountLimitReached', 'describeOrganizationConfigurationResponse_memberAccountLimitReached' - Whether the maximum number of allowed member accounts are already
-- associated with the Security Hub administrator account.
--
-- 'httpStatus', 'describeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConfigurationResponse
newDescribeOrganizationConfigurationResponse
  pHttpStatus_ =
    DescribeOrganizationConfigurationResponse'
      { autoEnable =
          Prelude.Nothing,
        autoEnableStandards =
          Prelude.Nothing,
        memberAccountLimitReached =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Whether to automatically enable Security Hub for new accounts in the
-- organization.
--
-- If set to @true@, then Security Hub is enabled for new accounts. If set
-- to false, then new accounts are not added automatically.
describeOrganizationConfigurationResponse_autoEnable :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe Prelude.Bool)
describeOrganizationConfigurationResponse_autoEnable = Lens.lens (\DescribeOrganizationConfigurationResponse' {autoEnable} -> autoEnable) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {autoEnable = a} :: DescribeOrganizationConfigurationResponse)

-- | Whether to automatically enable Security Hub
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-standards-enable-disable.html default standards>
-- for new member accounts in the organization.
--
-- The default value of this parameter is equal to @DEFAULT@.
--
-- If equal to @DEFAULT@, then Security Hub default standards are
-- automatically enabled for new member accounts. If equal to @NONE@, then
-- default standards are not automatically enabled for new member accounts.
describeOrganizationConfigurationResponse_autoEnableStandards :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe AutoEnableStandards)
describeOrganizationConfigurationResponse_autoEnableStandards = Lens.lens (\DescribeOrganizationConfigurationResponse' {autoEnableStandards} -> autoEnableStandards) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {autoEnableStandards = a} :: DescribeOrganizationConfigurationResponse)

-- | Whether the maximum number of allowed member accounts are already
-- associated with the Security Hub administrator account.
describeOrganizationConfigurationResponse_memberAccountLimitReached :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe Prelude.Bool)
describeOrganizationConfigurationResponse_memberAccountLimitReached = Lens.lens (\DescribeOrganizationConfigurationResponse' {memberAccountLimitReached} -> memberAccountLimitReached) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {memberAccountLimitReached = a} :: DescribeOrganizationConfigurationResponse)

-- | The response's http status code.
describeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Int
describeOrganizationConfigurationResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigurationResponse
  where
  rnf DescribeOrganizationConfigurationResponse' {..} =
    Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf autoEnableStandards
      `Prelude.seq` Prelude.rnf memberAccountLimitReached
      `Prelude.seq` Prelude.rnf httpStatus
