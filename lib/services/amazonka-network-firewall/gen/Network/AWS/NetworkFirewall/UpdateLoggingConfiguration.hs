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
-- Module      : Network.AWS.NetworkFirewall.UpdateLoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging configuration for the specified firewall.
--
-- To change the logging configuration, retrieve the LoggingConfiguration
-- by calling DescribeLoggingConfiguration, then change it and provide the
-- modified object to this update call. You must change the logging
-- configuration one LogDestinationConfig at a time inside the retrieved
-- LoggingConfiguration object.
--
-- You can perform only one of the following actions in any call to
-- @UpdateLoggingConfiguration@:
--
-- -   Create a new log destination object by adding a single
--     @LogDestinationConfig@ array element to @LogDestinationConfigs@.
--
-- -   Delete a log destination object by removing a single
--     @LogDestinationConfig@ array element from @LogDestinationConfigs@.
--
-- -   Change the @LogDestination@ setting in a single
--     @LogDestinationConfig@ array element.
--
-- You can\'t change the @LogDestinationType@ or @LogType@ in a
-- @LogDestinationConfig@. To change these settings, delete the existing
-- @LogDestinationConfig@ object and create a new one, using two separate
-- calls to this update operation.
module Network.AWS.NetworkFirewall.UpdateLoggingConfiguration
  ( -- * Creating a Request
    UpdateLoggingConfiguration (..),
    newUpdateLoggingConfiguration,

    -- * Request Lenses
    updateLoggingConfiguration_firewallArn,
    updateLoggingConfiguration_loggingConfiguration,
    updateLoggingConfiguration_firewallName,

    -- * Destructuring the Response
    UpdateLoggingConfigurationResponse (..),
    newUpdateLoggingConfigurationResponse,

    -- * Response Lenses
    updateLoggingConfigurationResponse_firewallArn,
    updateLoggingConfigurationResponse_loggingConfiguration,
    updateLoggingConfigurationResponse_firewallName,
    updateLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkFirewall.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLoggingConfiguration' smart constructor.
data UpdateLoggingConfiguration = UpdateLoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | Defines how Network Firewall performs logging for a firewall. If you
    -- omit this setting, Network Firewall disables logging for the firewall.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    firewallName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'updateLoggingConfiguration_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'loggingConfiguration', 'updateLoggingConfiguration_loggingConfiguration' - Defines how Network Firewall performs logging for a firewall. If you
-- omit this setting, Network Firewall disables logging for the firewall.
--
-- 'firewallName', 'updateLoggingConfiguration_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
newUpdateLoggingConfiguration ::
  UpdateLoggingConfiguration
newUpdateLoggingConfiguration =
  UpdateLoggingConfiguration'
    { firewallArn =
        Prelude.Nothing,
      loggingConfiguration = Prelude.Nothing,
      firewallName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall.
--
-- You must specify the ARN or the name, and you can specify both.
updateLoggingConfiguration_firewallArn :: Lens.Lens' UpdateLoggingConfiguration (Prelude.Maybe Prelude.Text)
updateLoggingConfiguration_firewallArn = Lens.lens (\UpdateLoggingConfiguration' {firewallArn} -> firewallArn) (\s@UpdateLoggingConfiguration' {} a -> s {firewallArn = a} :: UpdateLoggingConfiguration)

-- | Defines how Network Firewall performs logging for a firewall. If you
-- omit this setting, Network Firewall disables logging for the firewall.
updateLoggingConfiguration_loggingConfiguration :: Lens.Lens' UpdateLoggingConfiguration (Prelude.Maybe LoggingConfiguration)
updateLoggingConfiguration_loggingConfiguration = Lens.lens (\UpdateLoggingConfiguration' {loggingConfiguration} -> loggingConfiguration) (\s@UpdateLoggingConfiguration' {} a -> s {loggingConfiguration = a} :: UpdateLoggingConfiguration)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
updateLoggingConfiguration_firewallName :: Lens.Lens' UpdateLoggingConfiguration (Prelude.Maybe Prelude.Text)
updateLoggingConfiguration_firewallName = Lens.lens (\UpdateLoggingConfiguration' {firewallName} -> firewallName) (\s@UpdateLoggingConfiguration' {} a -> s {firewallName = a} :: UpdateLoggingConfiguration)

instance Core.AWSRequest UpdateLoggingConfiguration where
  type
    AWSResponse UpdateLoggingConfiguration =
      UpdateLoggingConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLoggingConfigurationResponse'
            Prelude.<$> (x Core..?> "FirewallArn")
            Prelude.<*> (x Core..?> "LoggingConfiguration")
            Prelude.<*> (x Core..?> "FirewallName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLoggingConfiguration

instance Prelude.NFData UpdateLoggingConfiguration

instance Core.ToHeaders UpdateLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.UpdateLoggingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLoggingConfiguration where
  toJSON UpdateLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FirewallArn" Core..=) Prelude.<$> firewallArn,
            ("LoggingConfiguration" Core..=)
              Prelude.<$> loggingConfiguration,
            ("FirewallName" Core..=) Prelude.<$> firewallName
          ]
      )

instance Core.ToPath UpdateLoggingConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLoggingConfigurationResponse' smart constructor.
data UpdateLoggingConfigurationResponse = UpdateLoggingConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'updateLoggingConfigurationResponse_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'loggingConfiguration', 'updateLoggingConfigurationResponse_loggingConfiguration' - Undocumented member.
--
-- 'firewallName', 'updateLoggingConfigurationResponse_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'httpStatus', 'updateLoggingConfigurationResponse_httpStatus' - The response's http status code.
newUpdateLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLoggingConfigurationResponse
newUpdateLoggingConfigurationResponse pHttpStatus_ =
  UpdateLoggingConfigurationResponse'
    { firewallArn =
        Prelude.Nothing,
      loggingConfiguration = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the firewall.
updateLoggingConfigurationResponse_firewallArn :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
updateLoggingConfigurationResponse_firewallArn = Lens.lens (\UpdateLoggingConfigurationResponse' {firewallArn} -> firewallArn) (\s@UpdateLoggingConfigurationResponse' {} a -> s {firewallArn = a} :: UpdateLoggingConfigurationResponse)

-- | Undocumented member.
updateLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe LoggingConfiguration)
updateLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\UpdateLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@UpdateLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: UpdateLoggingConfigurationResponse)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
updateLoggingConfigurationResponse_firewallName :: Lens.Lens' UpdateLoggingConfigurationResponse (Prelude.Maybe Prelude.Text)
updateLoggingConfigurationResponse_firewallName = Lens.lens (\UpdateLoggingConfigurationResponse' {firewallName} -> firewallName) (\s@UpdateLoggingConfigurationResponse' {} a -> s {firewallName = a} :: UpdateLoggingConfigurationResponse)

-- | The response's http status code.
updateLoggingConfigurationResponse_httpStatus :: Lens.Lens' UpdateLoggingConfigurationResponse Prelude.Int
updateLoggingConfigurationResponse_httpStatus = Lens.lens (\UpdateLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateLoggingConfigurationResponse)

instance
  Prelude.NFData
    UpdateLoggingConfigurationResponse
