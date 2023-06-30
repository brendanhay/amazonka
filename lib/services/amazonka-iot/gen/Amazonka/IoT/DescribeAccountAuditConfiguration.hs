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
-- Module      : Amazonka.IoT.DescribeAccountAuditConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the Device Defender audit settings for this
-- account. Settings include how audit notifications are sent and which
-- audit checks are enabled or disabled.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeAccountAuditConfiguration>
-- action.
module Amazonka.IoT.DescribeAccountAuditConfiguration
  ( -- * Creating a Request
    DescribeAccountAuditConfiguration (..),
    newDescribeAccountAuditConfiguration,

    -- * Destructuring the Response
    DescribeAccountAuditConfigurationResponse (..),
    newDescribeAccountAuditConfigurationResponse,

    -- * Response Lenses
    describeAccountAuditConfigurationResponse_auditCheckConfigurations,
    describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations,
    describeAccountAuditConfigurationResponse_roleArn,
    describeAccountAuditConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountAuditConfiguration' smart constructor.
data DescribeAccountAuditConfiguration = DescribeAccountAuditConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAuditConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountAuditConfiguration ::
  DescribeAccountAuditConfiguration
newDescribeAccountAuditConfiguration =
  DescribeAccountAuditConfiguration'

instance
  Core.AWSRequest
    DescribeAccountAuditConfiguration
  where
  type
    AWSResponse DescribeAccountAuditConfiguration =
      DescribeAccountAuditConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountAuditConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "auditCheckConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "auditNotificationTargetConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAccountAuditConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DescribeAccountAuditConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DescribeAccountAuditConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeAccountAuditConfiguration
  where
  toPath = Prelude.const "/audit/configuration"

instance
  Data.ToQuery
    DescribeAccountAuditConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountAuditConfigurationResponse' smart constructor.
data DescribeAccountAuditConfigurationResponse = DescribeAccountAuditConfigurationResponse'
  { -- | Which audit checks are enabled and disabled for this account.
    auditCheckConfigurations :: Prelude.Maybe (Prelude.HashMap Prelude.Text AuditCheckConfiguration),
    -- | Information about the targets to which audit notifications are sent for
    -- this account.
    auditNotificationTargetConfigurations :: Prelude.Maybe (Prelude.HashMap AuditNotificationType AuditNotificationTarget),
    -- | The ARN of the role that grants permission to IoT to access information
    -- about your devices, policies, certificates, and other items as required
    -- when performing an audit.
    --
    -- On the first call to @UpdateAccountAuditConfiguration@, this parameter
    -- is required.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAuditConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditCheckConfigurations', 'describeAccountAuditConfigurationResponse_auditCheckConfigurations' - Which audit checks are enabled and disabled for this account.
--
-- 'auditNotificationTargetConfigurations', 'describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations' - Information about the targets to which audit notifications are sent for
-- this account.
--
-- 'roleArn', 'describeAccountAuditConfigurationResponse_roleArn' - The ARN of the role that grants permission to IoT to access information
-- about your devices, policies, certificates, and other items as required
-- when performing an audit.
--
-- On the first call to @UpdateAccountAuditConfiguration@, this parameter
-- is required.
--
-- 'httpStatus', 'describeAccountAuditConfigurationResponse_httpStatus' - The response's http status code.
newDescribeAccountAuditConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAuditConfigurationResponse
newDescribeAccountAuditConfigurationResponse
  pHttpStatus_ =
    DescribeAccountAuditConfigurationResponse'
      { auditCheckConfigurations =
          Prelude.Nothing,
        auditNotificationTargetConfigurations =
          Prelude.Nothing,
        roleArn = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Which audit checks are enabled and disabled for this account.
describeAccountAuditConfigurationResponse_auditCheckConfigurations :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AuditCheckConfiguration))
describeAccountAuditConfigurationResponse_auditCheckConfigurations = Lens.lens (\DescribeAccountAuditConfigurationResponse' {auditCheckConfigurations} -> auditCheckConfigurations) (\s@DescribeAccountAuditConfigurationResponse' {} a -> s {auditCheckConfigurations = a} :: DescribeAccountAuditConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the targets to which audit notifications are sent for
-- this account.
describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Prelude.Maybe (Prelude.HashMap AuditNotificationType AuditNotificationTarget))
describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations = Lens.lens (\DescribeAccountAuditConfigurationResponse' {auditNotificationTargetConfigurations} -> auditNotificationTargetConfigurations) (\s@DescribeAccountAuditConfigurationResponse' {} a -> s {auditNotificationTargetConfigurations = a} :: DescribeAccountAuditConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the role that grants permission to IoT to access information
-- about your devices, policies, certificates, and other items as required
-- when performing an audit.
--
-- On the first call to @UpdateAccountAuditConfiguration@, this parameter
-- is required.
describeAccountAuditConfigurationResponse_roleArn :: Lens.Lens' DescribeAccountAuditConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAccountAuditConfigurationResponse_roleArn = Lens.lens (\DescribeAccountAuditConfigurationResponse' {roleArn} -> roleArn) (\s@DescribeAccountAuditConfigurationResponse' {} a -> s {roleArn = a} :: DescribeAccountAuditConfigurationResponse)

-- | The response's http status code.
describeAccountAuditConfigurationResponse_httpStatus :: Lens.Lens' DescribeAccountAuditConfigurationResponse Prelude.Int
describeAccountAuditConfigurationResponse_httpStatus = Lens.lens (\DescribeAccountAuditConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAuditConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeAccountAuditConfigurationResponse)

instance
  Prelude.NFData
    DescribeAccountAuditConfigurationResponse
  where
  rnf DescribeAccountAuditConfigurationResponse' {..} =
    Prelude.rnf auditCheckConfigurations
      `Prelude.seq` Prelude.rnf auditNotificationTargetConfigurations
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
