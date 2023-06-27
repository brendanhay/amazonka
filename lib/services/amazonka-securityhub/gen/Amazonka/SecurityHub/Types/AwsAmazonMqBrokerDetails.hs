{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerEncryptionOptionsDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLdapServerMetadataDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerUsersDetails

-- | Provides details about an Amazon MQ message broker. A message broker
-- allows software applications and components to communicate using various
-- programming languages, operating systems, and formal messaging
-- protocols.
--
-- /See:/ 'newAwsAmazonMqBrokerDetails' smart constructor.
data AwsAmazonMqBrokerDetails = AwsAmazonMqBrokerDetails'
  { -- | The authentication strategy used to secure the broker. The default is
    -- @SIMPLE@.
    authenticationStrategy :: Prelude.Maybe Prelude.Text,
    -- | Whether automatically upgrade new minor versions for brokers, as new
    -- versions are released and supported by Amazon MQ. Automatic upgrades
    -- occur during the scheduled maintenance window of the broker or after a
    -- manual broker reboot.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the broker.
    brokerArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s name.
    brokerName :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s deployment mode.
    deploymentMode :: Prelude.Maybe Prelude.Text,
    -- | Encryption options for the broker. Doesn’t apply to RabbitMQ brokers.
    encryptionOptions :: Prelude.Maybe AwsAmazonMqBrokerEncryptionOptionsDetails,
    -- | The type of broker engine.
    engineType :: Prelude.Maybe Prelude.Text,
    -- | The version of the broker engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s instance type.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the Lightweight Directory Access Protocol (LDAP) server
    -- used to authenticate and authorize connections to the broker. This is an
    -- optional failover server.
    ldapServerMetadata :: Prelude.Maybe AwsAmazonMqBrokerLdapServerMetadataDetails,
    -- | Turns on Amazon CloudWatch logging for brokers.
    logs :: Prelude.Maybe AwsAmazonMqBrokerLogsDetails,
    -- | The scheduled time period (UTC) during which Amazon MQ begins to apply
    -- pending updates or patches to the broker.
    maintenanceWindowStartTime :: Prelude.Maybe AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails,
    -- | Permits connections from applications outside of the VPC that hosts the
    -- broker\'s subnets.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The list of rules (one minimum, 125 maximum) that authorize connections
    -- to brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The broker\'s storage type.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of groups that define which subnets and IP ranges the broker
    -- can use from different Availability Zones.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The list of all broker usernames for the specified broker. Doesn\'t
    -- apply to RabbitMQ brokers.
    users :: Prelude.Maybe [AwsAmazonMqBrokerUsersDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationStrategy', 'awsAmazonMqBrokerDetails_authenticationStrategy' - The authentication strategy used to secure the broker. The default is
-- @SIMPLE@.
--
-- 'autoMinorVersionUpgrade', 'awsAmazonMqBrokerDetails_autoMinorVersionUpgrade' - Whether automatically upgrade new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
--
-- 'brokerArn', 'awsAmazonMqBrokerDetails_brokerArn' - The Amazon Resource Name (ARN) of the broker.
--
-- 'brokerId', 'awsAmazonMqBrokerDetails_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'brokerName', 'awsAmazonMqBrokerDetails_brokerName' - The broker\'s name.
--
-- 'deploymentMode', 'awsAmazonMqBrokerDetails_deploymentMode' - The broker\'s deployment mode.
--
-- 'encryptionOptions', 'awsAmazonMqBrokerDetails_encryptionOptions' - Encryption options for the broker. Doesn’t apply to RabbitMQ brokers.
--
-- 'engineType', 'awsAmazonMqBrokerDetails_engineType' - The type of broker engine.
--
-- 'engineVersion', 'awsAmazonMqBrokerDetails_engineVersion' - The version of the broker engine.
--
-- 'hostInstanceType', 'awsAmazonMqBrokerDetails_hostInstanceType' - The broker\'s instance type.
--
-- 'ldapServerMetadata', 'awsAmazonMqBrokerDetails_ldapServerMetadata' - The metadata of the Lightweight Directory Access Protocol (LDAP) server
-- used to authenticate and authorize connections to the broker. This is an
-- optional failover server.
--
-- 'logs', 'awsAmazonMqBrokerDetails_logs' - Turns on Amazon CloudWatch logging for brokers.
--
-- 'maintenanceWindowStartTime', 'awsAmazonMqBrokerDetails_maintenanceWindowStartTime' - The scheduled time period (UTC) during which Amazon MQ begins to apply
-- pending updates or patches to the broker.
--
-- 'publiclyAccessible', 'awsAmazonMqBrokerDetails_publiclyAccessible' - Permits connections from applications outside of the VPC that hosts the
-- broker\'s subnets.
--
-- 'securityGroups', 'awsAmazonMqBrokerDetails_securityGroups' - The list of rules (one minimum, 125 maximum) that authorize connections
-- to brokers.
--
-- 'storageType', 'awsAmazonMqBrokerDetails_storageType' - The broker\'s storage type.
--
-- 'subnetIds', 'awsAmazonMqBrokerDetails_subnetIds' - The list of groups that define which subnets and IP ranges the broker
-- can use from different Availability Zones.
--
-- 'users', 'awsAmazonMqBrokerDetails_users' - The list of all broker usernames for the specified broker. Doesn\'t
-- apply to RabbitMQ brokers.
newAwsAmazonMqBrokerDetails ::
  AwsAmazonMqBrokerDetails
newAwsAmazonMqBrokerDetails =
  AwsAmazonMqBrokerDetails'
    { authenticationStrategy =
        Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      brokerArn = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      brokerName = Prelude.Nothing,
      deploymentMode = Prelude.Nothing,
      encryptionOptions = Prelude.Nothing,
      engineType = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      logs = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      storageType = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      users = Prelude.Nothing
    }

-- | The authentication strategy used to secure the broker. The default is
-- @SIMPLE@.
awsAmazonMqBrokerDetails_authenticationStrategy :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_authenticationStrategy = Lens.lens (\AwsAmazonMqBrokerDetails' {authenticationStrategy} -> authenticationStrategy) (\s@AwsAmazonMqBrokerDetails' {} a -> s {authenticationStrategy = a} :: AwsAmazonMqBrokerDetails)

-- | Whether automatically upgrade new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
awsAmazonMqBrokerDetails_autoMinorVersionUpgrade :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerDetails_autoMinorVersionUpgrade = Lens.lens (\AwsAmazonMqBrokerDetails' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@AwsAmazonMqBrokerDetails' {} a -> s {autoMinorVersionUpgrade = a} :: AwsAmazonMqBrokerDetails)

-- | The Amazon Resource Name (ARN) of the broker.
awsAmazonMqBrokerDetails_brokerArn :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_brokerArn = Lens.lens (\AwsAmazonMqBrokerDetails' {brokerArn} -> brokerArn) (\s@AwsAmazonMqBrokerDetails' {} a -> s {brokerArn = a} :: AwsAmazonMqBrokerDetails)

-- | The unique ID that Amazon MQ generates for the broker.
awsAmazonMqBrokerDetails_brokerId :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_brokerId = Lens.lens (\AwsAmazonMqBrokerDetails' {brokerId} -> brokerId) (\s@AwsAmazonMqBrokerDetails' {} a -> s {brokerId = a} :: AwsAmazonMqBrokerDetails)

-- | The broker\'s name.
awsAmazonMqBrokerDetails_brokerName :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_brokerName = Lens.lens (\AwsAmazonMqBrokerDetails' {brokerName} -> brokerName) (\s@AwsAmazonMqBrokerDetails' {} a -> s {brokerName = a} :: AwsAmazonMqBrokerDetails)

-- | The broker\'s deployment mode.
awsAmazonMqBrokerDetails_deploymentMode :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_deploymentMode = Lens.lens (\AwsAmazonMqBrokerDetails' {deploymentMode} -> deploymentMode) (\s@AwsAmazonMqBrokerDetails' {} a -> s {deploymentMode = a} :: AwsAmazonMqBrokerDetails)

-- | Encryption options for the broker. Doesn’t apply to RabbitMQ brokers.
awsAmazonMqBrokerDetails_encryptionOptions :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe AwsAmazonMqBrokerEncryptionOptionsDetails)
awsAmazonMqBrokerDetails_encryptionOptions = Lens.lens (\AwsAmazonMqBrokerDetails' {encryptionOptions} -> encryptionOptions) (\s@AwsAmazonMqBrokerDetails' {} a -> s {encryptionOptions = a} :: AwsAmazonMqBrokerDetails)

-- | The type of broker engine.
awsAmazonMqBrokerDetails_engineType :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_engineType = Lens.lens (\AwsAmazonMqBrokerDetails' {engineType} -> engineType) (\s@AwsAmazonMqBrokerDetails' {} a -> s {engineType = a} :: AwsAmazonMqBrokerDetails)

-- | The version of the broker engine.
awsAmazonMqBrokerDetails_engineVersion :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_engineVersion = Lens.lens (\AwsAmazonMqBrokerDetails' {engineVersion} -> engineVersion) (\s@AwsAmazonMqBrokerDetails' {} a -> s {engineVersion = a} :: AwsAmazonMqBrokerDetails)

-- | The broker\'s instance type.
awsAmazonMqBrokerDetails_hostInstanceType :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_hostInstanceType = Lens.lens (\AwsAmazonMqBrokerDetails' {hostInstanceType} -> hostInstanceType) (\s@AwsAmazonMqBrokerDetails' {} a -> s {hostInstanceType = a} :: AwsAmazonMqBrokerDetails)

-- | The metadata of the Lightweight Directory Access Protocol (LDAP) server
-- used to authenticate and authorize connections to the broker. This is an
-- optional failover server.
awsAmazonMqBrokerDetails_ldapServerMetadata :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe AwsAmazonMqBrokerLdapServerMetadataDetails)
awsAmazonMqBrokerDetails_ldapServerMetadata = Lens.lens (\AwsAmazonMqBrokerDetails' {ldapServerMetadata} -> ldapServerMetadata) (\s@AwsAmazonMqBrokerDetails' {} a -> s {ldapServerMetadata = a} :: AwsAmazonMqBrokerDetails)

-- | Turns on Amazon CloudWatch logging for brokers.
awsAmazonMqBrokerDetails_logs :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe AwsAmazonMqBrokerLogsDetails)
awsAmazonMqBrokerDetails_logs = Lens.lens (\AwsAmazonMqBrokerDetails' {logs} -> logs) (\s@AwsAmazonMqBrokerDetails' {} a -> s {logs = a} :: AwsAmazonMqBrokerDetails)

-- | The scheduled time period (UTC) during which Amazon MQ begins to apply
-- pending updates or patches to the broker.
awsAmazonMqBrokerDetails_maintenanceWindowStartTime :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails)
awsAmazonMqBrokerDetails_maintenanceWindowStartTime = Lens.lens (\AwsAmazonMqBrokerDetails' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@AwsAmazonMqBrokerDetails' {} a -> s {maintenanceWindowStartTime = a} :: AwsAmazonMqBrokerDetails)

-- | Permits connections from applications outside of the VPC that hosts the
-- broker\'s subnets.
awsAmazonMqBrokerDetails_publiclyAccessible :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerDetails_publiclyAccessible = Lens.lens (\AwsAmazonMqBrokerDetails' {publiclyAccessible} -> publiclyAccessible) (\s@AwsAmazonMqBrokerDetails' {} a -> s {publiclyAccessible = a} :: AwsAmazonMqBrokerDetails)

-- | The list of rules (one minimum, 125 maximum) that authorize connections
-- to brokers.
awsAmazonMqBrokerDetails_securityGroups :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe [Prelude.Text])
awsAmazonMqBrokerDetails_securityGroups = Lens.lens (\AwsAmazonMqBrokerDetails' {securityGroups} -> securityGroups) (\s@AwsAmazonMqBrokerDetails' {} a -> s {securityGroups = a} :: AwsAmazonMqBrokerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s storage type.
awsAmazonMqBrokerDetails_storageType :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerDetails_storageType = Lens.lens (\AwsAmazonMqBrokerDetails' {storageType} -> storageType) (\s@AwsAmazonMqBrokerDetails' {} a -> s {storageType = a} :: AwsAmazonMqBrokerDetails)

-- | The list of groups that define which subnets and IP ranges the broker
-- can use from different Availability Zones.
awsAmazonMqBrokerDetails_subnetIds :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe [Prelude.Text])
awsAmazonMqBrokerDetails_subnetIds = Lens.lens (\AwsAmazonMqBrokerDetails' {subnetIds} -> subnetIds) (\s@AwsAmazonMqBrokerDetails' {} a -> s {subnetIds = a} :: AwsAmazonMqBrokerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The list of all broker usernames for the specified broker. Doesn\'t
-- apply to RabbitMQ brokers.
awsAmazonMqBrokerDetails_users :: Lens.Lens' AwsAmazonMqBrokerDetails (Prelude.Maybe [AwsAmazonMqBrokerUsersDetails])
awsAmazonMqBrokerDetails_users = Lens.lens (\AwsAmazonMqBrokerDetails' {users} -> users) (\s@AwsAmazonMqBrokerDetails' {} a -> s {users = a} :: AwsAmazonMqBrokerDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsAmazonMqBrokerDetails where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerDetails"
      ( \x ->
          AwsAmazonMqBrokerDetails'
            Prelude.<$> (x Data..:? "AuthenticationStrategy")
            Prelude.<*> (x Data..:? "AutoMinorVersionUpgrade")
            Prelude.<*> (x Data..:? "BrokerArn")
            Prelude.<*> (x Data..:? "BrokerId")
            Prelude.<*> (x Data..:? "BrokerName")
            Prelude.<*> (x Data..:? "DeploymentMode")
            Prelude.<*> (x Data..:? "EncryptionOptions")
            Prelude.<*> (x Data..:? "EngineType")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "HostInstanceType")
            Prelude.<*> (x Data..:? "LdapServerMetadata")
            Prelude.<*> (x Data..:? "Logs")
            Prelude.<*> (x Data..:? "MaintenanceWindowStartTime")
            Prelude.<*> (x Data..:? "PubliclyAccessible")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StorageType")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Users" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsAmazonMqBrokerDetails where
  hashWithSalt _salt AwsAmazonMqBrokerDetails' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationStrategy
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` brokerArn
      `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` brokerName
      `Prelude.hashWithSalt` deploymentMode
      `Prelude.hashWithSalt` encryptionOptions
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` hostInstanceType
      `Prelude.hashWithSalt` ldapServerMetadata
      `Prelude.hashWithSalt` logs
      `Prelude.hashWithSalt` maintenanceWindowStartTime
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` users

instance Prelude.NFData AwsAmazonMqBrokerDetails where
  rnf AwsAmazonMqBrokerDetails' {..} =
    Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf brokerArn
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf brokerName
      `Prelude.seq` Prelude.rnf deploymentMode
      `Prelude.seq` Prelude.rnf encryptionOptions
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf ldapServerMetadata
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf maintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf users

instance Data.ToJSON AwsAmazonMqBrokerDetails where
  toJSON AwsAmazonMqBrokerDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationStrategy" Data..=)
              Prelude.<$> authenticationStrategy,
            ("AutoMinorVersionUpgrade" Data..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("BrokerArn" Data..=) Prelude.<$> brokerArn,
            ("BrokerId" Data..=) Prelude.<$> brokerId,
            ("BrokerName" Data..=) Prelude.<$> brokerName,
            ("DeploymentMode" Data..=)
              Prelude.<$> deploymentMode,
            ("EncryptionOptions" Data..=)
              Prelude.<$> encryptionOptions,
            ("EngineType" Data..=) Prelude.<$> engineType,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("HostInstanceType" Data..=)
              Prelude.<$> hostInstanceType,
            ("LdapServerMetadata" Data..=)
              Prelude.<$> ldapServerMetadata,
            ("Logs" Data..=) Prelude.<$> logs,
            ("MaintenanceWindowStartTime" Data..=)
              Prelude.<$> maintenanceWindowStartTime,
            ("PubliclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("StorageType" Data..=) Prelude.<$> storageType,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds,
            ("Users" Data..=) Prelude.<$> users
          ]
      )
