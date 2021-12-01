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
-- Module      : Amazonka.MQ.UpdateBroker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a pending configuration change to a broker.
module Amazonka.MQ.UpdateBroker
  ( -- * Creating a Request
    UpdateBroker (..),
    newUpdateBroker,

    -- * Request Lenses
    updateBroker_engineVersion,
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_securityGroups,
    updateBroker_authenticationStrategy,
    updateBroker_ldapServerMetadata,
    updateBroker_maintenanceWindowStartTime,
    updateBroker_logs,
    updateBroker_configuration,
    updateBroker_hostInstanceType,
    updateBroker_brokerId,

    -- * Destructuring the Response
    UpdateBrokerResponse (..),
    newUpdateBrokerResponse,

    -- * Response Lenses
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_maintenanceWindowStartTime,
    updateBrokerResponse_logs,
    updateBrokerResponse_configuration,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates the broker using the specified properties.
--
-- /See:/ 'newUpdateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { -- | The broker engine version. For a list of supported engine versions, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Enables automatic upgrades to new minor versions for brokers, as new
    -- versions are released and supported by Amazon MQ. Automatic upgrades
    -- occur during the scheduled maintenance window of the broker or after a
    -- manual broker reboot.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Optional. The authentication strategy used to secure the broker. The
    -- default is SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | Optional. The metadata of the LDAP server used to authenticate and
    -- authorize connections to the broker. Does not apply to RabbitMQ brokers.
    ldapServerMetadata :: Prelude.Maybe LdapServerMetadataInput,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Prelude.Maybe WeeklyStartTime,
    -- | Enables Amazon CloudWatch logging for brokers.
    logs :: Prelude.Maybe Logs,
    -- | A list of information about the configuration.
    configuration :: Prelude.Maybe ConfigurationId,
    -- | The broker\'s host instance type to upgrade to. For a list of supported
    -- instance types, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'updateBroker_engineVersion' - The broker engine version. For a list of supported engine versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'autoMinorVersionUpgrade', 'updateBroker_autoMinorVersionUpgrade' - Enables automatic upgrades to new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
--
-- 'securityGroups', 'updateBroker_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'authenticationStrategy', 'updateBroker_authenticationStrategy' - Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
--
-- 'ldapServerMetadata', 'updateBroker_ldapServerMetadata' - Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
--
-- 'maintenanceWindowStartTime', 'updateBroker_maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- 'logs', 'updateBroker_logs' - Enables Amazon CloudWatch logging for brokers.
--
-- 'configuration', 'updateBroker_configuration' - A list of information about the configuration.
--
-- 'hostInstanceType', 'updateBroker_hostInstanceType' - The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
--
-- 'brokerId', 'updateBroker_brokerId' - The unique ID that Amazon MQ generates for the broker.
newUpdateBroker ::
  -- | 'brokerId'
  Prelude.Text ->
  UpdateBroker
newUpdateBroker pBrokerId_ =
  UpdateBroker'
    { engineVersion = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      logs = Prelude.Nothing,
      configuration = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      brokerId = pBrokerId_
    }

-- | The broker engine version. For a list of supported engine versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
updateBroker_engineVersion :: Lens.Lens' UpdateBroker (Prelude.Maybe Prelude.Text)
updateBroker_engineVersion = Lens.lens (\UpdateBroker' {engineVersion} -> engineVersion) (\s@UpdateBroker' {} a -> s {engineVersion = a} :: UpdateBroker)

-- | Enables automatic upgrades to new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
updateBroker_autoMinorVersionUpgrade :: Lens.Lens' UpdateBroker (Prelude.Maybe Prelude.Bool)
updateBroker_autoMinorVersionUpgrade = Lens.lens (\UpdateBroker' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@UpdateBroker' {} a -> s {autoMinorVersionUpgrade = a} :: UpdateBroker)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
updateBroker_securityGroups :: Lens.Lens' UpdateBroker (Prelude.Maybe [Prelude.Text])
updateBroker_securityGroups = Lens.lens (\UpdateBroker' {securityGroups} -> securityGroups) (\s@UpdateBroker' {} a -> s {securityGroups = a} :: UpdateBroker) Prelude.. Lens.mapping Lens.coerced

-- | Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
updateBroker_authenticationStrategy :: Lens.Lens' UpdateBroker (Prelude.Maybe AuthenticationStrategy)
updateBroker_authenticationStrategy = Lens.lens (\UpdateBroker' {authenticationStrategy} -> authenticationStrategy) (\s@UpdateBroker' {} a -> s {authenticationStrategy = a} :: UpdateBroker)

-- | Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
updateBroker_ldapServerMetadata :: Lens.Lens' UpdateBroker (Prelude.Maybe LdapServerMetadataInput)
updateBroker_ldapServerMetadata = Lens.lens (\UpdateBroker' {ldapServerMetadata} -> ldapServerMetadata) (\s@UpdateBroker' {} a -> s {ldapServerMetadata = a} :: UpdateBroker)

-- | The parameters that determine the WeeklyStartTime.
updateBroker_maintenanceWindowStartTime :: Lens.Lens' UpdateBroker (Prelude.Maybe WeeklyStartTime)
updateBroker_maintenanceWindowStartTime = Lens.lens (\UpdateBroker' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@UpdateBroker' {} a -> s {maintenanceWindowStartTime = a} :: UpdateBroker)

-- | Enables Amazon CloudWatch logging for brokers.
updateBroker_logs :: Lens.Lens' UpdateBroker (Prelude.Maybe Logs)
updateBroker_logs = Lens.lens (\UpdateBroker' {logs} -> logs) (\s@UpdateBroker' {} a -> s {logs = a} :: UpdateBroker)

-- | A list of information about the configuration.
updateBroker_configuration :: Lens.Lens' UpdateBroker (Prelude.Maybe ConfigurationId)
updateBroker_configuration = Lens.lens (\UpdateBroker' {configuration} -> configuration) (\s@UpdateBroker' {} a -> s {configuration = a} :: UpdateBroker)

-- | The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
updateBroker_hostInstanceType :: Lens.Lens' UpdateBroker (Prelude.Maybe Prelude.Text)
updateBroker_hostInstanceType = Lens.lens (\UpdateBroker' {hostInstanceType} -> hostInstanceType) (\s@UpdateBroker' {} a -> s {hostInstanceType = a} :: UpdateBroker)

-- | The unique ID that Amazon MQ generates for the broker.
updateBroker_brokerId :: Lens.Lens' UpdateBroker Prelude.Text
updateBroker_brokerId = Lens.lens (\UpdateBroker' {brokerId} -> brokerId) (\s@UpdateBroker' {} a -> s {brokerId = a} :: UpdateBroker)

instance Core.AWSRequest UpdateBroker where
  type AWSResponse UpdateBroker = UpdateBrokerResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBrokerResponse'
            Prelude.<$> (x Core..?> "engineVersion")
            Prelude.<*> (x Core..?> "autoMinorVersionUpgrade")
            Prelude.<*> (x Core..?> "securityGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "authenticationStrategy")
            Prelude.<*> (x Core..?> "ldapServerMetadata")
            Prelude.<*> (x Core..?> "maintenanceWindowStartTime")
            Prelude.<*> (x Core..?> "logs")
            Prelude.<*> (x Core..?> "configuration")
            Prelude.<*> (x Core..?> "brokerId")
            Prelude.<*> (x Core..?> "hostInstanceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBroker where
  hashWithSalt salt' UpdateBroker' {..} =
    salt' `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` hostInstanceType
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` logs
      `Prelude.hashWithSalt` maintenanceWindowStartTime
      `Prelude.hashWithSalt` ldapServerMetadata
      `Prelude.hashWithSalt` authenticationStrategy
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData UpdateBroker where
  rnf UpdateBroker' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf maintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf ldapServerMetadata
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade

instance Core.ToHeaders UpdateBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBroker where
  toJSON UpdateBroker' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("engineVersion" Core..=) Prelude.<$> engineVersion,
            ("autoMinorVersionUpgrade" Core..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("securityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("authenticationStrategy" Core..=)
              Prelude.<$> authenticationStrategy,
            ("ldapServerMetadata" Core..=)
              Prelude.<$> ldapServerMetadata,
            ("maintenanceWindowStartTime" Core..=)
              Prelude.<$> maintenanceWindowStartTime,
            ("logs" Core..=) Prelude.<$> logs,
            ("configuration" Core..=) Prelude.<$> configuration,
            ("hostInstanceType" Core..=)
              Prelude.<$> hostInstanceType
          ]
      )

instance Core.ToPath UpdateBroker where
  toPath UpdateBroker' {..} =
    Prelude.mconcat
      ["/v1/brokers/", Core.toBS brokerId]

instance Core.ToQuery UpdateBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { -- | The broker engine version to upgrade to. For a list of supported engine
    -- versions, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The new boolean value that specifies whether broker engines
    -- automatically upgrade to new minor versions as new versions are released
    -- and supported by Amazon MQ.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Optional. The authentication strategy used to secure the broker. The
    -- default is SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | Optional. The metadata of the LDAP server used to authenticate and
    -- authorize connections to the broker. Does not apply to RabbitMQ brokers.
    ldapServerMetadata :: Prelude.Maybe LdapServerMetadataOutput,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Prelude.Maybe WeeklyStartTime,
    -- | The list of information about logs to be enabled for the specified
    -- broker.
    logs :: Prelude.Maybe Logs,
    -- | The ID of the updated configuration.
    configuration :: Prelude.Maybe ConfigurationId,
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s host instance type to upgrade to. For a list of supported
    -- instance types, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'updateBrokerResponse_engineVersion' - The broker engine version to upgrade to. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'autoMinorVersionUpgrade', 'updateBrokerResponse_autoMinorVersionUpgrade' - The new boolean value that specifies whether broker engines
-- automatically upgrade to new minor versions as new versions are released
-- and supported by Amazon MQ.
--
-- 'securityGroups', 'updateBrokerResponse_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'authenticationStrategy', 'updateBrokerResponse_authenticationStrategy' - Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
--
-- 'ldapServerMetadata', 'updateBrokerResponse_ldapServerMetadata' - Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
--
-- 'maintenanceWindowStartTime', 'updateBrokerResponse_maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- 'logs', 'updateBrokerResponse_logs' - The list of information about logs to be enabled for the specified
-- broker.
--
-- 'configuration', 'updateBrokerResponse_configuration' - The ID of the updated configuration.
--
-- 'brokerId', 'updateBrokerResponse_brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- 'hostInstanceType', 'updateBrokerResponse_hostInstanceType' - The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
--
-- 'httpStatus', 'updateBrokerResponse_httpStatus' - The response's http status code.
newUpdateBrokerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBrokerResponse
newUpdateBrokerResponse pHttpStatus_ =
  UpdateBrokerResponse'
    { engineVersion =
        Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      logs = Prelude.Nothing,
      configuration = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The broker engine version to upgrade to. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
updateBrokerResponse_engineVersion :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Text)
updateBrokerResponse_engineVersion = Lens.lens (\UpdateBrokerResponse' {engineVersion} -> engineVersion) (\s@UpdateBrokerResponse' {} a -> s {engineVersion = a} :: UpdateBrokerResponse)

-- | The new boolean value that specifies whether broker engines
-- automatically upgrade to new minor versions as new versions are released
-- and supported by Amazon MQ.
updateBrokerResponse_autoMinorVersionUpgrade :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Bool)
updateBrokerResponse_autoMinorVersionUpgrade = Lens.lens (\UpdateBrokerResponse' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@UpdateBrokerResponse' {} a -> s {autoMinorVersionUpgrade = a} :: UpdateBrokerResponse)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
updateBrokerResponse_securityGroups :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe [Prelude.Text])
updateBrokerResponse_securityGroups = Lens.lens (\UpdateBrokerResponse' {securityGroups} -> securityGroups) (\s@UpdateBrokerResponse' {} a -> s {securityGroups = a} :: UpdateBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
updateBrokerResponse_authenticationStrategy :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe AuthenticationStrategy)
updateBrokerResponse_authenticationStrategy = Lens.lens (\UpdateBrokerResponse' {authenticationStrategy} -> authenticationStrategy) (\s@UpdateBrokerResponse' {} a -> s {authenticationStrategy = a} :: UpdateBrokerResponse)

-- | Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
updateBrokerResponse_ldapServerMetadata :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe LdapServerMetadataOutput)
updateBrokerResponse_ldapServerMetadata = Lens.lens (\UpdateBrokerResponse' {ldapServerMetadata} -> ldapServerMetadata) (\s@UpdateBrokerResponse' {} a -> s {ldapServerMetadata = a} :: UpdateBrokerResponse)

-- | The parameters that determine the WeeklyStartTime.
updateBrokerResponse_maintenanceWindowStartTime :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe WeeklyStartTime)
updateBrokerResponse_maintenanceWindowStartTime = Lens.lens (\UpdateBrokerResponse' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@UpdateBrokerResponse' {} a -> s {maintenanceWindowStartTime = a} :: UpdateBrokerResponse)

-- | The list of information about logs to be enabled for the specified
-- broker.
updateBrokerResponse_logs :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Logs)
updateBrokerResponse_logs = Lens.lens (\UpdateBrokerResponse' {logs} -> logs) (\s@UpdateBrokerResponse' {} a -> s {logs = a} :: UpdateBrokerResponse)

-- | The ID of the updated configuration.
updateBrokerResponse_configuration :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe ConfigurationId)
updateBrokerResponse_configuration = Lens.lens (\UpdateBrokerResponse' {configuration} -> configuration) (\s@UpdateBrokerResponse' {} a -> s {configuration = a} :: UpdateBrokerResponse)

-- | Required. The unique ID that Amazon MQ generates for the broker.
updateBrokerResponse_brokerId :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Text)
updateBrokerResponse_brokerId = Lens.lens (\UpdateBrokerResponse' {brokerId} -> brokerId) (\s@UpdateBrokerResponse' {} a -> s {brokerId = a} :: UpdateBrokerResponse)

-- | The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
updateBrokerResponse_hostInstanceType :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Text)
updateBrokerResponse_hostInstanceType = Lens.lens (\UpdateBrokerResponse' {hostInstanceType} -> hostInstanceType) (\s@UpdateBrokerResponse' {} a -> s {hostInstanceType = a} :: UpdateBrokerResponse)

-- | The response's http status code.
updateBrokerResponse_httpStatus :: Lens.Lens' UpdateBrokerResponse Prelude.Int
updateBrokerResponse_httpStatus = Lens.lens (\UpdateBrokerResponse' {httpStatus} -> httpStatus) (\s@UpdateBrokerResponse' {} a -> s {httpStatus = a} :: UpdateBrokerResponse)

instance Prelude.NFData UpdateBrokerResponse where
  rnf UpdateBrokerResponse' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf maintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf ldapServerMetadata
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
