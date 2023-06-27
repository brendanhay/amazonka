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
-- Module      : Amazonka.SecurityHub.DescribeHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the Hub resource in your account, including the
-- @HubArn@ and the time when you enabled Security Hub.
module Amazonka.SecurityHub.DescribeHub
  ( -- * Creating a Request
    DescribeHub (..),
    newDescribeHub,

    -- * Request Lenses
    describeHub_hubArn,

    -- * Destructuring the Response
    DescribeHubResponse (..),
    newDescribeHubResponse,

    -- * Response Lenses
    describeHubResponse_autoEnableControls,
    describeHubResponse_controlFindingGenerator,
    describeHubResponse_hubArn,
    describeHubResponse_subscribedAt,
    describeHubResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDescribeHub' smart constructor.
data DescribeHub = DescribeHub'
  { -- | The ARN of the Hub resource to retrieve.
    hubArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHub' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubArn', 'describeHub_hubArn' - The ARN of the Hub resource to retrieve.
newDescribeHub ::
  DescribeHub
newDescribeHub =
  DescribeHub' {hubArn = Prelude.Nothing}

-- | The ARN of the Hub resource to retrieve.
describeHub_hubArn :: Lens.Lens' DescribeHub (Prelude.Maybe Prelude.Text)
describeHub_hubArn = Lens.lens (\DescribeHub' {hubArn} -> hubArn) (\s@DescribeHub' {} a -> s {hubArn = a} :: DescribeHub)

instance Core.AWSRequest DescribeHub where
  type AWSResponse DescribeHub = DescribeHubResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHubResponse'
            Prelude.<$> (x Data..?> "AutoEnableControls")
            Prelude.<*> (x Data..?> "ControlFindingGenerator")
            Prelude.<*> (x Data..?> "HubArn")
            Prelude.<*> (x Data..?> "SubscribedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHub where
  hashWithSalt _salt DescribeHub' {..} =
    _salt `Prelude.hashWithSalt` hubArn

instance Prelude.NFData DescribeHub where
  rnf DescribeHub' {..} = Prelude.rnf hubArn

instance Data.ToHeaders DescribeHub where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeHub where
  toPath = Prelude.const "/accounts"

instance Data.ToQuery DescribeHub where
  toQuery DescribeHub' {..} =
    Prelude.mconcat ["HubArn" Data.=: hubArn]

-- | /See:/ 'newDescribeHubResponse' smart constructor.
data DescribeHubResponse = DescribeHubResponse'
  { -- | Whether to automatically enable new controls when they are added to
    -- standards that are enabled.
    --
    -- If set to @true@, then new controls for enabled standards are enabled
    -- automatically. If set to @false@, then new controls are not enabled.
    autoEnableControls :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the calling account has consolidated control findings
    -- turned on. If the value for this field is set to @SECURITY_CONTROL@,
    -- Security Hub generates a single finding for a control check even when
    -- the check applies to multiple enabled standards.
    --
    -- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
    -- generates separate findings for a control check when the check applies
    -- to multiple enabled standards.
    --
    -- The value for this field in a member account matches the value in the
    -- administrator account. For accounts that aren\'t part of an
    -- organization, the default value of this field is @SECURITY_CONTROL@ if
    -- you enabled Security Hub on or after February 23, 2023.
    controlFindingGenerator :: Prelude.Maybe ControlFindingGenerator,
    -- | The ARN of the Hub resource that was retrieved.
    hubArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when Security Hub was enabled in the account.
    subscribedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHubResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableControls', 'describeHubResponse_autoEnableControls' - Whether to automatically enable new controls when they are added to
-- standards that are enabled.
--
-- If set to @true@, then new controls for enabled standards are enabled
-- automatically. If set to @false@, then new controls are not enabled.
--
-- 'controlFindingGenerator', 'describeHubResponse_controlFindingGenerator' - Specifies whether the calling account has consolidated control findings
-- turned on. If the value for this field is set to @SECURITY_CONTROL@,
-- Security Hub generates a single finding for a control check even when
-- the check applies to multiple enabled standards.
--
-- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
-- generates separate findings for a control check when the check applies
-- to multiple enabled standards.
--
-- The value for this field in a member account matches the value in the
-- administrator account. For accounts that aren\'t part of an
-- organization, the default value of this field is @SECURITY_CONTROL@ if
-- you enabled Security Hub on or after February 23, 2023.
--
-- 'hubArn', 'describeHubResponse_hubArn' - The ARN of the Hub resource that was retrieved.
--
-- 'subscribedAt', 'describeHubResponse_subscribedAt' - The date and time when Security Hub was enabled in the account.
--
-- 'httpStatus', 'describeHubResponse_httpStatus' - The response's http status code.
newDescribeHubResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHubResponse
newDescribeHubResponse pHttpStatus_ =
  DescribeHubResponse'
    { autoEnableControls =
        Prelude.Nothing,
      controlFindingGenerator = Prelude.Nothing,
      hubArn = Prelude.Nothing,
      subscribedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Whether to automatically enable new controls when they are added to
-- standards that are enabled.
--
-- If set to @true@, then new controls for enabled standards are enabled
-- automatically. If set to @false@, then new controls are not enabled.
describeHubResponse_autoEnableControls :: Lens.Lens' DescribeHubResponse (Prelude.Maybe Prelude.Bool)
describeHubResponse_autoEnableControls = Lens.lens (\DescribeHubResponse' {autoEnableControls} -> autoEnableControls) (\s@DescribeHubResponse' {} a -> s {autoEnableControls = a} :: DescribeHubResponse)

-- | Specifies whether the calling account has consolidated control findings
-- turned on. If the value for this field is set to @SECURITY_CONTROL@,
-- Security Hub generates a single finding for a control check even when
-- the check applies to multiple enabled standards.
--
-- If the value for this field is set to @STANDARD_CONTROL@, Security Hub
-- generates separate findings for a control check when the check applies
-- to multiple enabled standards.
--
-- The value for this field in a member account matches the value in the
-- administrator account. For accounts that aren\'t part of an
-- organization, the default value of this field is @SECURITY_CONTROL@ if
-- you enabled Security Hub on or after February 23, 2023.
describeHubResponse_controlFindingGenerator :: Lens.Lens' DescribeHubResponse (Prelude.Maybe ControlFindingGenerator)
describeHubResponse_controlFindingGenerator = Lens.lens (\DescribeHubResponse' {controlFindingGenerator} -> controlFindingGenerator) (\s@DescribeHubResponse' {} a -> s {controlFindingGenerator = a} :: DescribeHubResponse)

-- | The ARN of the Hub resource that was retrieved.
describeHubResponse_hubArn :: Lens.Lens' DescribeHubResponse (Prelude.Maybe Prelude.Text)
describeHubResponse_hubArn = Lens.lens (\DescribeHubResponse' {hubArn} -> hubArn) (\s@DescribeHubResponse' {} a -> s {hubArn = a} :: DescribeHubResponse)

-- | The date and time when Security Hub was enabled in the account.
describeHubResponse_subscribedAt :: Lens.Lens' DescribeHubResponse (Prelude.Maybe Prelude.Text)
describeHubResponse_subscribedAt = Lens.lens (\DescribeHubResponse' {subscribedAt} -> subscribedAt) (\s@DescribeHubResponse' {} a -> s {subscribedAt = a} :: DescribeHubResponse)

-- | The response's http status code.
describeHubResponse_httpStatus :: Lens.Lens' DescribeHubResponse Prelude.Int
describeHubResponse_httpStatus = Lens.lens (\DescribeHubResponse' {httpStatus} -> httpStatus) (\s@DescribeHubResponse' {} a -> s {httpStatus = a} :: DescribeHubResponse)

instance Prelude.NFData DescribeHubResponse where
  rnf DescribeHubResponse' {..} =
    Prelude.rnf autoEnableControls
      `Prelude.seq` Prelude.rnf controlFindingGenerator
      `Prelude.seq` Prelude.rnf hubArn
      `Prelude.seq` Prelude.rnf subscribedAt
      `Prelude.seq` Prelude.rnf httpStatus
