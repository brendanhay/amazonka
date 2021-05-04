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
-- Module      : Network.AWS.IoT.UpdateAccountAuditConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures or reconfigures the Device Defender audit settings for this
-- account. Settings include how audit notifications are sent and which
-- audit checks are enabled or disabled.
module Network.AWS.IoT.UpdateAccountAuditConfiguration
  ( -- * Creating a Request
    UpdateAccountAuditConfiguration (..),
    newUpdateAccountAuditConfiguration,

    -- * Request Lenses
    updateAccountAuditConfiguration_roleArn,
    updateAccountAuditConfiguration_auditCheckConfigurations,
    updateAccountAuditConfiguration_auditNotificationTargetConfigurations,

    -- * Destructuring the Response
    UpdateAccountAuditConfigurationResponse (..),
    newUpdateAccountAuditConfigurationResponse,

    -- * Response Lenses
    updateAccountAuditConfigurationResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAccountAuditConfiguration' smart constructor.
data UpdateAccountAuditConfiguration = UpdateAccountAuditConfiguration'
  { -- | The Amazon Resource Name (ARN) of the role that grants permission to AWS
    -- IoT to access information about your devices, policies, certificates,
    -- and other items as required when performing an audit.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies which audit checks are enabled and disabled for this account.
    -- Use @DescribeAccountAuditConfiguration@ to see the list of all checks,
    -- including those that are currently enabled.
    --
    -- Some data collection might start immediately when certain checks are
    -- enabled. When a check is disabled, any data collected so far in relation
    -- to the check is deleted.
    --
    -- You cannot disable a check if it\'s used by any scheduled audit. You
    -- must first delete the check from the scheduled audit or delete the
    -- scheduled audit itself.
    --
    -- On the first call to @UpdateAccountAuditConfiguration@, this parameter
    -- is required and must specify at least one enabled check.
    auditCheckConfigurations :: Prelude.Maybe (Prelude.HashMap Prelude.Text AuditCheckConfiguration),
    -- | Information about the targets to which audit notifications are sent.
    auditNotificationTargetConfigurations :: Prelude.Maybe (Prelude.HashMap AuditNotificationType AuditNotificationTarget)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountAuditConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateAccountAuditConfiguration_roleArn' - The Amazon Resource Name (ARN) of the role that grants permission to AWS
-- IoT to access information about your devices, policies, certificates,
-- and other items as required when performing an audit.
--
-- 'auditCheckConfigurations', 'updateAccountAuditConfiguration_auditCheckConfigurations' - Specifies which audit checks are enabled and disabled for this account.
-- Use @DescribeAccountAuditConfiguration@ to see the list of all checks,
-- including those that are currently enabled.
--
-- Some data collection might start immediately when certain checks are
-- enabled. When a check is disabled, any data collected so far in relation
-- to the check is deleted.
--
-- You cannot disable a check if it\'s used by any scheduled audit. You
-- must first delete the check from the scheduled audit or delete the
-- scheduled audit itself.
--
-- On the first call to @UpdateAccountAuditConfiguration@, this parameter
-- is required and must specify at least one enabled check.
--
-- 'auditNotificationTargetConfigurations', 'updateAccountAuditConfiguration_auditNotificationTargetConfigurations' - Information about the targets to which audit notifications are sent.
newUpdateAccountAuditConfiguration ::
  UpdateAccountAuditConfiguration
newUpdateAccountAuditConfiguration =
  UpdateAccountAuditConfiguration'
    { roleArn =
        Prelude.Nothing,
      auditCheckConfigurations = Prelude.Nothing,
      auditNotificationTargetConfigurations =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the role that grants permission to AWS
-- IoT to access information about your devices, policies, certificates,
-- and other items as required when performing an audit.
updateAccountAuditConfiguration_roleArn :: Lens.Lens' UpdateAccountAuditConfiguration (Prelude.Maybe Prelude.Text)
updateAccountAuditConfiguration_roleArn = Lens.lens (\UpdateAccountAuditConfiguration' {roleArn} -> roleArn) (\s@UpdateAccountAuditConfiguration' {} a -> s {roleArn = a} :: UpdateAccountAuditConfiguration)

-- | Specifies which audit checks are enabled and disabled for this account.
-- Use @DescribeAccountAuditConfiguration@ to see the list of all checks,
-- including those that are currently enabled.
--
-- Some data collection might start immediately when certain checks are
-- enabled. When a check is disabled, any data collected so far in relation
-- to the check is deleted.
--
-- You cannot disable a check if it\'s used by any scheduled audit. You
-- must first delete the check from the scheduled audit or delete the
-- scheduled audit itself.
--
-- On the first call to @UpdateAccountAuditConfiguration@, this parameter
-- is required and must specify at least one enabled check.
updateAccountAuditConfiguration_auditCheckConfigurations :: Lens.Lens' UpdateAccountAuditConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text AuditCheckConfiguration))
updateAccountAuditConfiguration_auditCheckConfigurations = Lens.lens (\UpdateAccountAuditConfiguration' {auditCheckConfigurations} -> auditCheckConfigurations) (\s@UpdateAccountAuditConfiguration' {} a -> s {auditCheckConfigurations = a} :: UpdateAccountAuditConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the targets to which audit notifications are sent.
updateAccountAuditConfiguration_auditNotificationTargetConfigurations :: Lens.Lens' UpdateAccountAuditConfiguration (Prelude.Maybe (Prelude.HashMap AuditNotificationType AuditNotificationTarget))
updateAccountAuditConfiguration_auditNotificationTargetConfigurations = Lens.lens (\UpdateAccountAuditConfiguration' {auditNotificationTargetConfigurations} -> auditNotificationTargetConfigurations) (\s@UpdateAccountAuditConfiguration' {} a -> s {auditNotificationTargetConfigurations = a} :: UpdateAccountAuditConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.AWSRequest
    UpdateAccountAuditConfiguration
  where
  type
    Rs UpdateAccountAuditConfiguration =
      UpdateAccountAuditConfigurationResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAccountAuditConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAccountAuditConfiguration

instance
  Prelude.NFData
    UpdateAccountAuditConfiguration

instance
  Prelude.ToHeaders
    UpdateAccountAuditConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToJSON
    UpdateAccountAuditConfiguration
  where
  toJSON UpdateAccountAuditConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("auditCheckConfigurations" Prelude..=)
              Prelude.<$> auditCheckConfigurations,
            ("auditNotificationTargetConfigurations" Prelude..=)
              Prelude.<$> auditNotificationTargetConfigurations
          ]
      )

instance
  Prelude.ToPath
    UpdateAccountAuditConfiguration
  where
  toPath = Prelude.const "/audit/configuration"

instance
  Prelude.ToQuery
    UpdateAccountAuditConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccountAuditConfigurationResponse' smart constructor.
data UpdateAccountAuditConfigurationResponse = UpdateAccountAuditConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountAuditConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAccountAuditConfigurationResponse_httpStatus' - The response's http status code.
newUpdateAccountAuditConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAccountAuditConfigurationResponse
newUpdateAccountAuditConfigurationResponse
  pHttpStatus_ =
    UpdateAccountAuditConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateAccountAuditConfigurationResponse_httpStatus :: Lens.Lens' UpdateAccountAuditConfigurationResponse Prelude.Int
updateAccountAuditConfigurationResponse_httpStatus = Lens.lens (\UpdateAccountAuditConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateAccountAuditConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateAccountAuditConfigurationResponse)

instance
  Prelude.NFData
    UpdateAccountAuditConfigurationResponse
