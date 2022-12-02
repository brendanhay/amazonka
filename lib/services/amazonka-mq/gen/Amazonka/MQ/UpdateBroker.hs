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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_ldapServerMetadata,
    updateBroker_configuration,
    updateBroker_logs,
    updateBroker_securityGroups,
    updateBroker_hostInstanceType,
    updateBroker_authenticationStrategy,
    updateBroker_maintenanceWindowStartTime,
    updateBroker_engineVersion,
    updateBroker_brokerId,

    -- * Destructuring the Response
    UpdateBrokerResponse (..),
    newUpdateBrokerResponse,

    -- * Response Lenses
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_configuration,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_logs,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_maintenanceWindowStartTime,
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates the broker using the specified properties.
--
-- /See:/ 'newUpdateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { -- | Enables automatic upgrades to new minor versions for brokers, as new
    -- versions are released and supported by Amazon MQ. Automatic upgrades
    -- occur during the scheduled maintenance window of the broker or after a
    -- manual broker reboot.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Optional. The metadata of the LDAP server used to authenticate and
    -- authorize connections to the broker. Does not apply to RabbitMQ brokers.
    ldapServerMetadata :: Prelude.Maybe LdapServerMetadataInput,
    -- | A list of information about the configuration.
    configuration :: Prelude.Maybe ConfigurationId,
    -- | Enables Amazon CloudWatch logging for brokers.
    logs :: Prelude.Maybe Logs,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The broker\'s host instance type to upgrade to. For a list of supported
    -- instance types, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Optional. The authentication strategy used to secure the broker. The
    -- default is SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Prelude.Maybe WeeklyStartTime,
    -- | The broker engine version. For a list of supported engine versions, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Maybe Prelude.Text,
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
-- 'autoMinorVersionUpgrade', 'updateBroker_autoMinorVersionUpgrade' - Enables automatic upgrades to new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
--
-- 'ldapServerMetadata', 'updateBroker_ldapServerMetadata' - Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
--
-- 'configuration', 'updateBroker_configuration' - A list of information about the configuration.
--
-- 'logs', 'updateBroker_logs' - Enables Amazon CloudWatch logging for brokers.
--
-- 'securityGroups', 'updateBroker_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'hostInstanceType', 'updateBroker_hostInstanceType' - The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
--
-- 'authenticationStrategy', 'updateBroker_authenticationStrategy' - Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
--
-- 'maintenanceWindowStartTime', 'updateBroker_maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- 'engineVersion', 'updateBroker_engineVersion' - The broker engine version. For a list of supported engine versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'brokerId', 'updateBroker_brokerId' - The unique ID that Amazon MQ generates for the broker.
newUpdateBroker ::
  -- | 'brokerId'
  Prelude.Text ->
  UpdateBroker
newUpdateBroker pBrokerId_ =
  UpdateBroker'
    { autoMinorVersionUpgrade =
        Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      configuration = Prelude.Nothing,
      logs = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      brokerId = pBrokerId_
    }

-- | Enables automatic upgrades to new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
updateBroker_autoMinorVersionUpgrade :: Lens.Lens' UpdateBroker (Prelude.Maybe Prelude.Bool)
updateBroker_autoMinorVersionUpgrade = Lens.lens (\UpdateBroker' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@UpdateBroker' {} a -> s {autoMinorVersionUpgrade = a} :: UpdateBroker)

-- | Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
updateBroker_ldapServerMetadata :: Lens.Lens' UpdateBroker (Prelude.Maybe LdapServerMetadataInput)
updateBroker_ldapServerMetadata = Lens.lens (\UpdateBroker' {ldapServerMetadata} -> ldapServerMetadata) (\s@UpdateBroker' {} a -> s {ldapServerMetadata = a} :: UpdateBroker)

-- | A list of information about the configuration.
updateBroker_configuration :: Lens.Lens' UpdateBroker (Prelude.Maybe ConfigurationId)
updateBroker_configuration = Lens.lens (\UpdateBroker' {configuration} -> configuration) (\s@UpdateBroker' {} a -> s {configuration = a} :: UpdateBroker)

-- | Enables Amazon CloudWatch logging for brokers.
updateBroker_logs :: Lens.Lens' UpdateBroker (Prelude.Maybe Logs)
updateBroker_logs = Lens.lens (\UpdateBroker' {logs} -> logs) (\s@UpdateBroker' {} a -> s {logs = a} :: UpdateBroker)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
updateBroker_securityGroups :: Lens.Lens' UpdateBroker (Prelude.Maybe [Prelude.Text])
updateBroker_securityGroups = Lens.lens (\UpdateBroker' {securityGroups} -> securityGroups) (\s@UpdateBroker' {} a -> s {securityGroups = a} :: UpdateBroker) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
updateBroker_hostInstanceType :: Lens.Lens' UpdateBroker (Prelude.Maybe Prelude.Text)
updateBroker_hostInstanceType = Lens.lens (\UpdateBroker' {hostInstanceType} -> hostInstanceType) (\s@UpdateBroker' {} a -> s {hostInstanceType = a} :: UpdateBroker)

-- | Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
updateBroker_authenticationStrategy :: Lens.Lens' UpdateBroker (Prelude.Maybe AuthenticationStrategy)
updateBroker_authenticationStrategy = Lens.lens (\UpdateBroker' {authenticationStrategy} -> authenticationStrategy) (\s@UpdateBroker' {} a -> s {authenticationStrategy = a} :: UpdateBroker)

-- | The parameters that determine the WeeklyStartTime.
updateBroker_maintenanceWindowStartTime :: Lens.Lens' UpdateBroker (Prelude.Maybe WeeklyStartTime)
updateBroker_maintenanceWindowStartTime = Lens.lens (\UpdateBroker' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@UpdateBroker' {} a -> s {maintenanceWindowStartTime = a} :: UpdateBroker)

-- | The broker engine version. For a list of supported engine versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
updateBroker_engineVersion :: Lens.Lens' UpdateBroker (Prelude.Maybe Prelude.Text)
updateBroker_engineVersion = Lens.lens (\UpdateBroker' {engineVersion} -> engineVersion) (\s@UpdateBroker' {} a -> s {engineVersion = a} :: UpdateBroker)

-- | The unique ID that Amazon MQ generates for the broker.
updateBroker_brokerId :: Lens.Lens' UpdateBroker Prelude.Text
updateBroker_brokerId = Lens.lens (\UpdateBroker' {brokerId} -> brokerId) (\s@UpdateBroker' {} a -> s {brokerId = a} :: UpdateBroker)

instance Core.AWSRequest UpdateBroker where
  type AWSResponse UpdateBroker = UpdateBrokerResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBrokerResponse'
            Prelude.<$> (x Data..?> "autoMinorVersionUpgrade")
            Prelude.<*> (x Data..?> "ldapServerMetadata")
            Prelude.<*> (x Data..?> "configuration")
            Prelude.<*> (x Data..?> "brokerId")
            Prelude.<*> (x Data..?> "logs")
            Prelude.<*> (x Data..?> "securityGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "hostInstanceType")
            Prelude.<*> (x Data..?> "authenticationStrategy")
            Prelude.<*> (x Data..?> "maintenanceWindowStartTime")
            Prelude.<*> (x Data..?> "engineVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBroker where
  hashWithSalt _salt UpdateBroker' {..} =
    _salt
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` ldapServerMetadata
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` logs
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` hostInstanceType
      `Prelude.hashWithSalt` authenticationStrategy
      `Prelude.hashWithSalt` maintenanceWindowStartTime
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` brokerId

instance Prelude.NFData UpdateBroker where
  rnf UpdateBroker' {..} =
    Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf ldapServerMetadata
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf maintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf brokerId

instance Data.ToHeaders UpdateBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBroker where
  toJSON UpdateBroker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoMinorVersionUpgrade" Data..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("ldapServerMetadata" Data..=)
              Prelude.<$> ldapServerMetadata,
            ("configuration" Data..=) Prelude.<$> configuration,
            ("logs" Data..=) Prelude.<$> logs,
            ("securityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("hostInstanceType" Data..=)
              Prelude.<$> hostInstanceType,
            ("authenticationStrategy" Data..=)
              Prelude.<$> authenticationStrategy,
            ("maintenanceWindowStartTime" Data..=)
              Prelude.<$> maintenanceWindowStartTime,
            ("engineVersion" Data..=) Prelude.<$> engineVersion
          ]
      )

instance Data.ToPath UpdateBroker where
  toPath UpdateBroker' {..} =
    Prelude.mconcat
      ["/v1/brokers/", Data.toBS brokerId]

instance Data.ToQuery UpdateBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { -- | The new boolean value that specifies whether broker engines
    -- automatically upgrade to new minor versions as new versions are released
    -- and supported by Amazon MQ.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Optional. The metadata of the LDAP server used to authenticate and
    -- authorize connections to the broker. Does not apply to RabbitMQ brokers.
    ldapServerMetadata :: Prelude.Maybe LdapServerMetadataOutput,
    -- | The ID of the updated configuration.
    configuration :: Prelude.Maybe ConfigurationId,
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The list of information about logs to be enabled for the specified
    -- broker.
    logs :: Prelude.Maybe Logs,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The broker\'s host instance type to upgrade to. For a list of supported
    -- instance types, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Optional. The authentication strategy used to secure the broker. The
    -- default is SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Prelude.Maybe WeeklyStartTime,
    -- | The broker engine version to upgrade to. For a list of supported engine
    -- versions, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Maybe Prelude.Text,
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
-- 'autoMinorVersionUpgrade', 'updateBrokerResponse_autoMinorVersionUpgrade' - The new boolean value that specifies whether broker engines
-- automatically upgrade to new minor versions as new versions are released
-- and supported by Amazon MQ.
--
-- 'ldapServerMetadata', 'updateBrokerResponse_ldapServerMetadata' - Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
--
-- 'configuration', 'updateBrokerResponse_configuration' - The ID of the updated configuration.
--
-- 'brokerId', 'updateBrokerResponse_brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- 'logs', 'updateBrokerResponse_logs' - The list of information about logs to be enabled for the specified
-- broker.
--
-- 'securityGroups', 'updateBrokerResponse_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'hostInstanceType', 'updateBrokerResponse_hostInstanceType' - The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
--
-- 'authenticationStrategy', 'updateBrokerResponse_authenticationStrategy' - Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
--
-- 'maintenanceWindowStartTime', 'updateBrokerResponse_maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- 'engineVersion', 'updateBrokerResponse_engineVersion' - The broker engine version to upgrade to. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'httpStatus', 'updateBrokerResponse_httpStatus' - The response's http status code.
newUpdateBrokerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBrokerResponse
newUpdateBrokerResponse pHttpStatus_ =
  UpdateBrokerResponse'
    { autoMinorVersionUpgrade =
        Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      configuration = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      logs = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new boolean value that specifies whether broker engines
-- automatically upgrade to new minor versions as new versions are released
-- and supported by Amazon MQ.
updateBrokerResponse_autoMinorVersionUpgrade :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Bool)
updateBrokerResponse_autoMinorVersionUpgrade = Lens.lens (\UpdateBrokerResponse' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@UpdateBrokerResponse' {} a -> s {autoMinorVersionUpgrade = a} :: UpdateBrokerResponse)

-- | Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker. Does not apply to RabbitMQ brokers.
updateBrokerResponse_ldapServerMetadata :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe LdapServerMetadataOutput)
updateBrokerResponse_ldapServerMetadata = Lens.lens (\UpdateBrokerResponse' {ldapServerMetadata} -> ldapServerMetadata) (\s@UpdateBrokerResponse' {} a -> s {ldapServerMetadata = a} :: UpdateBrokerResponse)

-- | The ID of the updated configuration.
updateBrokerResponse_configuration :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe ConfigurationId)
updateBrokerResponse_configuration = Lens.lens (\UpdateBrokerResponse' {configuration} -> configuration) (\s@UpdateBrokerResponse' {} a -> s {configuration = a} :: UpdateBrokerResponse)

-- | Required. The unique ID that Amazon MQ generates for the broker.
updateBrokerResponse_brokerId :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Text)
updateBrokerResponse_brokerId = Lens.lens (\UpdateBrokerResponse' {brokerId} -> brokerId) (\s@UpdateBrokerResponse' {} a -> s {brokerId = a} :: UpdateBrokerResponse)

-- | The list of information about logs to be enabled for the specified
-- broker.
updateBrokerResponse_logs :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Logs)
updateBrokerResponse_logs = Lens.lens (\UpdateBrokerResponse' {logs} -> logs) (\s@UpdateBrokerResponse' {} a -> s {logs = a} :: UpdateBrokerResponse)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
updateBrokerResponse_securityGroups :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe [Prelude.Text])
updateBrokerResponse_securityGroups = Lens.lens (\UpdateBrokerResponse' {securityGroups} -> securityGroups) (\s@UpdateBrokerResponse' {} a -> s {securityGroups = a} :: UpdateBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
updateBrokerResponse_hostInstanceType :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Text)
updateBrokerResponse_hostInstanceType = Lens.lens (\UpdateBrokerResponse' {hostInstanceType} -> hostInstanceType) (\s@UpdateBrokerResponse' {} a -> s {hostInstanceType = a} :: UpdateBrokerResponse)

-- | Optional. The authentication strategy used to secure the broker. The
-- default is SIMPLE.
updateBrokerResponse_authenticationStrategy :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe AuthenticationStrategy)
updateBrokerResponse_authenticationStrategy = Lens.lens (\UpdateBrokerResponse' {authenticationStrategy} -> authenticationStrategy) (\s@UpdateBrokerResponse' {} a -> s {authenticationStrategy = a} :: UpdateBrokerResponse)

-- | The parameters that determine the WeeklyStartTime.
updateBrokerResponse_maintenanceWindowStartTime :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe WeeklyStartTime)
updateBrokerResponse_maintenanceWindowStartTime = Lens.lens (\UpdateBrokerResponse' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@UpdateBrokerResponse' {} a -> s {maintenanceWindowStartTime = a} :: UpdateBrokerResponse)

-- | The broker engine version to upgrade to. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
updateBrokerResponse_engineVersion :: Lens.Lens' UpdateBrokerResponse (Prelude.Maybe Prelude.Text)
updateBrokerResponse_engineVersion = Lens.lens (\UpdateBrokerResponse' {engineVersion} -> engineVersion) (\s@UpdateBrokerResponse' {} a -> s {engineVersion = a} :: UpdateBrokerResponse)

-- | The response's http status code.
updateBrokerResponse_httpStatus :: Lens.Lens' UpdateBrokerResponse Prelude.Int
updateBrokerResponse_httpStatus = Lens.lens (\UpdateBrokerResponse' {httpStatus} -> httpStatus) (\s@UpdateBrokerResponse' {} a -> s {httpStatus = a} :: UpdateBrokerResponse)

instance Prelude.NFData UpdateBrokerResponse where
  rnf UpdateBrokerResponse' {..} =
    Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf ldapServerMetadata
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf maintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf httpStatus
