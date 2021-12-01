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
-- Module      : Amazonka.MQ.DescribeBroker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified broker.
module Amazonka.MQ.DescribeBroker
  ( -- * Creating a Request
    DescribeBroker (..),
    newDescribeBroker,

    -- * Request Lenses
    describeBroker_brokerId,

    -- * Destructuring the Response
    DescribeBrokerResponse (..),
    newDescribeBrokerResponse,

    -- * Response Lenses
    describeBrokerResponse_brokerName,
    describeBrokerResponse_engineVersion,
    describeBrokerResponse_pendingAuthenticationStrategy,
    describeBrokerResponse_brokerState,
    describeBrokerResponse_publiclyAccessible,
    describeBrokerResponse_autoMinorVersionUpgrade,
    describeBrokerResponse_securityGroups,
    describeBrokerResponse_users,
    describeBrokerResponse_pendingSecurityGroups,
    describeBrokerResponse_subnetIds,
    describeBrokerResponse_created,
    describeBrokerResponse_configurations,
    describeBrokerResponse_authenticationStrategy,
    describeBrokerResponse_pendingHostInstanceType,
    describeBrokerResponse_ldapServerMetadata,
    describeBrokerResponse_maintenanceWindowStartTime,
    describeBrokerResponse_logs,
    describeBrokerResponse_encryptionOptions,
    describeBrokerResponse_deploymentMode,
    describeBrokerResponse_pendingEngineVersion,
    describeBrokerResponse_brokerId,
    describeBrokerResponse_pendingLdapServerMetadata,
    describeBrokerResponse_engineType,
    describeBrokerResponse_brokerArn,
    describeBrokerResponse_tags,
    describeBrokerResponse_brokerInstances,
    describeBrokerResponse_hostInstanceType,
    describeBrokerResponse_storageType,
    describeBrokerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBroker' smart constructor.
data DescribeBroker = DescribeBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'describeBroker_brokerId' - The unique ID that Amazon MQ generates for the broker.
newDescribeBroker ::
  -- | 'brokerId'
  Prelude.Text ->
  DescribeBroker
newDescribeBroker pBrokerId_ =
  DescribeBroker' {brokerId = pBrokerId_}

-- | The unique ID that Amazon MQ generates for the broker.
describeBroker_brokerId :: Lens.Lens' DescribeBroker Prelude.Text
describeBroker_brokerId = Lens.lens (\DescribeBroker' {brokerId} -> brokerId) (\s@DescribeBroker' {} a -> s {brokerId = a} :: DescribeBroker)

instance Core.AWSRequest DescribeBroker where
  type
    AWSResponse DescribeBroker =
      DescribeBrokerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBrokerResponse'
            Prelude.<$> (x Core..?> "brokerName")
            Prelude.<*> (x Core..?> "engineVersion")
            Prelude.<*> (x Core..?> "pendingAuthenticationStrategy")
            Prelude.<*> (x Core..?> "brokerState")
            Prelude.<*> (x Core..?> "publiclyAccessible")
            Prelude.<*> (x Core..?> "autoMinorVersionUpgrade")
            Prelude.<*> (x Core..?> "securityGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "users" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "pendingSecurityGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "subnetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "created")
            Prelude.<*> (x Core..?> "configurations")
            Prelude.<*> (x Core..?> "authenticationStrategy")
            Prelude.<*> (x Core..?> "pendingHostInstanceType")
            Prelude.<*> (x Core..?> "ldapServerMetadata")
            Prelude.<*> (x Core..?> "maintenanceWindowStartTime")
            Prelude.<*> (x Core..?> "logs")
            Prelude.<*> (x Core..?> "encryptionOptions")
            Prelude.<*> (x Core..?> "deploymentMode")
            Prelude.<*> (x Core..?> "pendingEngineVersion")
            Prelude.<*> (x Core..?> "brokerId")
            Prelude.<*> (x Core..?> "pendingLdapServerMetadata")
            Prelude.<*> (x Core..?> "engineType")
            Prelude.<*> (x Core..?> "brokerArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "brokerInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "hostInstanceType")
            Prelude.<*> (x Core..?> "storageType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBroker where
  hashWithSalt salt' DescribeBroker' {..} =
    salt' `Prelude.hashWithSalt` brokerId

instance Prelude.NFData DescribeBroker where
  rnf DescribeBroker' {..} = Prelude.rnf brokerId

instance Core.ToHeaders DescribeBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeBroker where
  toPath DescribeBroker' {..} =
    Prelude.mconcat
      ["/v1/brokers/", Core.toBS brokerId]

instance Core.ToQuery DescribeBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBrokerResponse' smart constructor.
data DescribeBrokerResponse = DescribeBrokerResponse'
  { -- | The broker\'s name. This value must be unique in your AWS account, 1-50
    -- characters long, must contain only letters, numbers, dashes, and
    -- underscores, and must not contain white spaces, brackets, wildcard
    -- characters, or special characters.
    brokerName :: Prelude.Maybe Prelude.Text,
    -- | The broker engine\'s version. For a list of supported engine versions,
    -- see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The authentication strategy that will be applied when the broker is
    -- rebooted. The default is SIMPLE.
    pendingAuthenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | The broker\'s status.
    brokerState :: Prelude.Maybe BrokerState,
    -- | Enables connections from applications outside of the VPC that hosts the
    -- broker\'s subnets.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Enables automatic upgrades to new minor versions for brokers, as new
    -- versions are released and supported by Amazon MQ. Automatic upgrades
    -- occur during the scheduled maintenance window of the broker or after a
    -- manual broker reboot.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The list of rules (1 minimum, 125 maximum) that authorize connections to
    -- brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The list of all broker usernames for the specified broker.
    users :: Prelude.Maybe [UserSummary],
    -- | The list of pending security groups to authorize connections to brokers.
    pendingSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The list of groups that define which subnets and IP ranges the broker
    -- can use from different Availability Zones.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The time when the broker was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The list of all revisions for the specified configuration.
    configurations :: Prelude.Maybe Configurations,
    -- | The authentication strategy used to secure the broker. The default is
    -- SIMPLE.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | The broker\'s host instance type to upgrade to. For a list of supported
    -- instance types, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
    pendingHostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the LDAP server used to authenticate and authorize
    -- connections to the broker.
    ldapServerMetadata :: Prelude.Maybe LdapServerMetadataOutput,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Prelude.Maybe WeeklyStartTime,
    -- | The list of information about logs currently enabled and pending to be
    -- deployed for the specified broker.
    logs :: Prelude.Maybe LogsSummary,
    -- | Encryption options for the broker. Does not apply to RabbitMQ brokers.
    encryptionOptions :: Prelude.Maybe EncryptionOptions,
    -- | The broker\'s deployment mode.
    deploymentMode :: Prelude.Maybe DeploymentMode,
    -- | The broker engine version to upgrade to. For a list of supported engine
    -- versions, see
    -- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
    pendingEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the LDAP server that will be used to authenticate and
    -- authorize connections to the broker after it is rebooted.
    pendingLdapServerMetadata :: Prelude.Maybe LdapServerMetadataOutput,
    -- | The type of broker engine. Currently, Amazon MQ supports ACTIVEMQ and
    -- RABBITMQ.
    engineType :: Prelude.Maybe EngineType,
    -- | The broker\'s Amazon Resource Name (ARN).
    brokerArn :: Prelude.Maybe Prelude.Text,
    -- | The list of all tags associated with this broker.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of information about allocated brokers.
    brokerInstances :: Prelude.Maybe [BrokerInstance],
    -- | The broker\'s instance type.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s storage type.
    storageType :: Prelude.Maybe BrokerStorageType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerName', 'describeBrokerResponse_brokerName' - The broker\'s name. This value must be unique in your AWS account, 1-50
-- characters long, must contain only letters, numbers, dashes, and
-- underscores, and must not contain white spaces, brackets, wildcard
-- characters, or special characters.
--
-- 'engineVersion', 'describeBrokerResponse_engineVersion' - The broker engine\'s version. For a list of supported engine versions,
-- see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'pendingAuthenticationStrategy', 'describeBrokerResponse_pendingAuthenticationStrategy' - The authentication strategy that will be applied when the broker is
-- rebooted. The default is SIMPLE.
--
-- 'brokerState', 'describeBrokerResponse_brokerState' - The broker\'s status.
--
-- 'publiclyAccessible', 'describeBrokerResponse_publiclyAccessible' - Enables connections from applications outside of the VPC that hosts the
-- broker\'s subnets.
--
-- 'autoMinorVersionUpgrade', 'describeBrokerResponse_autoMinorVersionUpgrade' - Enables automatic upgrades to new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
--
-- 'securityGroups', 'describeBrokerResponse_securityGroups' - The list of rules (1 minimum, 125 maximum) that authorize connections to
-- brokers.
--
-- 'users', 'describeBrokerResponse_users' - The list of all broker usernames for the specified broker.
--
-- 'pendingSecurityGroups', 'describeBrokerResponse_pendingSecurityGroups' - The list of pending security groups to authorize connections to brokers.
--
-- 'subnetIds', 'describeBrokerResponse_subnetIds' - The list of groups that define which subnets and IP ranges the broker
-- can use from different Availability Zones.
--
-- 'created', 'describeBrokerResponse_created' - The time when the broker was created.
--
-- 'configurations', 'describeBrokerResponse_configurations' - The list of all revisions for the specified configuration.
--
-- 'authenticationStrategy', 'describeBrokerResponse_authenticationStrategy' - The authentication strategy used to secure the broker. The default is
-- SIMPLE.
--
-- 'pendingHostInstanceType', 'describeBrokerResponse_pendingHostInstanceType' - The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
--
-- 'ldapServerMetadata', 'describeBrokerResponse_ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- 'maintenanceWindowStartTime', 'describeBrokerResponse_maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- 'logs', 'describeBrokerResponse_logs' - The list of information about logs currently enabled and pending to be
-- deployed for the specified broker.
--
-- 'encryptionOptions', 'describeBrokerResponse_encryptionOptions' - Encryption options for the broker. Does not apply to RabbitMQ brokers.
--
-- 'deploymentMode', 'describeBrokerResponse_deploymentMode' - The broker\'s deployment mode.
--
-- 'pendingEngineVersion', 'describeBrokerResponse_pendingEngineVersion' - The broker engine version to upgrade to. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
--
-- 'brokerId', 'describeBrokerResponse_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'pendingLdapServerMetadata', 'describeBrokerResponse_pendingLdapServerMetadata' - The metadata of the LDAP server that will be used to authenticate and
-- authorize connections to the broker after it is rebooted.
--
-- 'engineType', 'describeBrokerResponse_engineType' - The type of broker engine. Currently, Amazon MQ supports ACTIVEMQ and
-- RABBITMQ.
--
-- 'brokerArn', 'describeBrokerResponse_brokerArn' - The broker\'s Amazon Resource Name (ARN).
--
-- 'tags', 'describeBrokerResponse_tags' - The list of all tags associated with this broker.
--
-- 'brokerInstances', 'describeBrokerResponse_brokerInstances' - A list of information about allocated brokers.
--
-- 'hostInstanceType', 'describeBrokerResponse_hostInstanceType' - The broker\'s instance type.
--
-- 'storageType', 'describeBrokerResponse_storageType' - The broker\'s storage type.
--
-- 'httpStatus', 'describeBrokerResponse_httpStatus' - The response's http status code.
newDescribeBrokerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBrokerResponse
newDescribeBrokerResponse pHttpStatus_ =
  DescribeBrokerResponse'
    { brokerName =
        Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      pendingAuthenticationStrategy = Prelude.Nothing,
      brokerState = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      users = Prelude.Nothing,
      pendingSecurityGroups = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      created = Prelude.Nothing,
      configurations = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      pendingHostInstanceType = Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      logs = Prelude.Nothing,
      encryptionOptions = Prelude.Nothing,
      deploymentMode = Prelude.Nothing,
      pendingEngineVersion = Prelude.Nothing,
      brokerId = Prelude.Nothing,
      pendingLdapServerMetadata = Prelude.Nothing,
      engineType = Prelude.Nothing,
      brokerArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      brokerInstances = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      storageType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The broker\'s name. This value must be unique in your AWS account, 1-50
-- characters long, must contain only letters, numbers, dashes, and
-- underscores, and must not contain white spaces, brackets, wildcard
-- characters, or special characters.
describeBrokerResponse_brokerName :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_brokerName = Lens.lens (\DescribeBrokerResponse' {brokerName} -> brokerName) (\s@DescribeBrokerResponse' {} a -> s {brokerName = a} :: DescribeBrokerResponse)

-- | The broker engine\'s version. For a list of supported engine versions,
-- see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
describeBrokerResponse_engineVersion :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_engineVersion = Lens.lens (\DescribeBrokerResponse' {engineVersion} -> engineVersion) (\s@DescribeBrokerResponse' {} a -> s {engineVersion = a} :: DescribeBrokerResponse)

-- | The authentication strategy that will be applied when the broker is
-- rebooted. The default is SIMPLE.
describeBrokerResponse_pendingAuthenticationStrategy :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe AuthenticationStrategy)
describeBrokerResponse_pendingAuthenticationStrategy = Lens.lens (\DescribeBrokerResponse' {pendingAuthenticationStrategy} -> pendingAuthenticationStrategy) (\s@DescribeBrokerResponse' {} a -> s {pendingAuthenticationStrategy = a} :: DescribeBrokerResponse)

-- | The broker\'s status.
describeBrokerResponse_brokerState :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe BrokerState)
describeBrokerResponse_brokerState = Lens.lens (\DescribeBrokerResponse' {brokerState} -> brokerState) (\s@DescribeBrokerResponse' {} a -> s {brokerState = a} :: DescribeBrokerResponse)

-- | Enables connections from applications outside of the VPC that hosts the
-- broker\'s subnets.
describeBrokerResponse_publiclyAccessible :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Bool)
describeBrokerResponse_publiclyAccessible = Lens.lens (\DescribeBrokerResponse' {publiclyAccessible} -> publiclyAccessible) (\s@DescribeBrokerResponse' {} a -> s {publiclyAccessible = a} :: DescribeBrokerResponse)

-- | Enables automatic upgrades to new minor versions for brokers, as new
-- versions are released and supported by Amazon MQ. Automatic upgrades
-- occur during the scheduled maintenance window of the broker or after a
-- manual broker reboot.
describeBrokerResponse_autoMinorVersionUpgrade :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Bool)
describeBrokerResponse_autoMinorVersionUpgrade = Lens.lens (\DescribeBrokerResponse' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DescribeBrokerResponse' {} a -> s {autoMinorVersionUpgrade = a} :: DescribeBrokerResponse)

-- | The list of rules (1 minimum, 125 maximum) that authorize connections to
-- brokers.
describeBrokerResponse_securityGroups :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe [Prelude.Text])
describeBrokerResponse_securityGroups = Lens.lens (\DescribeBrokerResponse' {securityGroups} -> securityGroups) (\s@DescribeBrokerResponse' {} a -> s {securityGroups = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of all broker usernames for the specified broker.
describeBrokerResponse_users :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe [UserSummary])
describeBrokerResponse_users = Lens.lens (\DescribeBrokerResponse' {users} -> users) (\s@DescribeBrokerResponse' {} a -> s {users = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of pending security groups to authorize connections to brokers.
describeBrokerResponse_pendingSecurityGroups :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe [Prelude.Text])
describeBrokerResponse_pendingSecurityGroups = Lens.lens (\DescribeBrokerResponse' {pendingSecurityGroups} -> pendingSecurityGroups) (\s@DescribeBrokerResponse' {} a -> s {pendingSecurityGroups = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of groups that define which subnets and IP ranges the broker
-- can use from different Availability Zones.
describeBrokerResponse_subnetIds :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe [Prelude.Text])
describeBrokerResponse_subnetIds = Lens.lens (\DescribeBrokerResponse' {subnetIds} -> subnetIds) (\s@DescribeBrokerResponse' {} a -> s {subnetIds = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time when the broker was created.
describeBrokerResponse_created :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.UTCTime)
describeBrokerResponse_created = Lens.lens (\DescribeBrokerResponse' {created} -> created) (\s@DescribeBrokerResponse' {} a -> s {created = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Core._Time

-- | The list of all revisions for the specified configuration.
describeBrokerResponse_configurations :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Configurations)
describeBrokerResponse_configurations = Lens.lens (\DescribeBrokerResponse' {configurations} -> configurations) (\s@DescribeBrokerResponse' {} a -> s {configurations = a} :: DescribeBrokerResponse)

-- | The authentication strategy used to secure the broker. The default is
-- SIMPLE.
describeBrokerResponse_authenticationStrategy :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe AuthenticationStrategy)
describeBrokerResponse_authenticationStrategy = Lens.lens (\DescribeBrokerResponse' {authenticationStrategy} -> authenticationStrategy) (\s@DescribeBrokerResponse' {} a -> s {authenticationStrategy = a} :: DescribeBrokerResponse)

-- | The broker\'s host instance type to upgrade to. For a list of supported
-- instance types, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker.html#broker-instance-types Broker instance types>.
describeBrokerResponse_pendingHostInstanceType :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_pendingHostInstanceType = Lens.lens (\DescribeBrokerResponse' {pendingHostInstanceType} -> pendingHostInstanceType) (\s@DescribeBrokerResponse' {} a -> s {pendingHostInstanceType = a} :: DescribeBrokerResponse)

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
describeBrokerResponse_ldapServerMetadata :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe LdapServerMetadataOutput)
describeBrokerResponse_ldapServerMetadata = Lens.lens (\DescribeBrokerResponse' {ldapServerMetadata} -> ldapServerMetadata) (\s@DescribeBrokerResponse' {} a -> s {ldapServerMetadata = a} :: DescribeBrokerResponse)

-- | The parameters that determine the WeeklyStartTime.
describeBrokerResponse_maintenanceWindowStartTime :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe WeeklyStartTime)
describeBrokerResponse_maintenanceWindowStartTime = Lens.lens (\DescribeBrokerResponse' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@DescribeBrokerResponse' {} a -> s {maintenanceWindowStartTime = a} :: DescribeBrokerResponse)

-- | The list of information about logs currently enabled and pending to be
-- deployed for the specified broker.
describeBrokerResponse_logs :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe LogsSummary)
describeBrokerResponse_logs = Lens.lens (\DescribeBrokerResponse' {logs} -> logs) (\s@DescribeBrokerResponse' {} a -> s {logs = a} :: DescribeBrokerResponse)

-- | Encryption options for the broker. Does not apply to RabbitMQ brokers.
describeBrokerResponse_encryptionOptions :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe EncryptionOptions)
describeBrokerResponse_encryptionOptions = Lens.lens (\DescribeBrokerResponse' {encryptionOptions} -> encryptionOptions) (\s@DescribeBrokerResponse' {} a -> s {encryptionOptions = a} :: DescribeBrokerResponse)

-- | The broker\'s deployment mode.
describeBrokerResponse_deploymentMode :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe DeploymentMode)
describeBrokerResponse_deploymentMode = Lens.lens (\DescribeBrokerResponse' {deploymentMode} -> deploymentMode) (\s@DescribeBrokerResponse' {} a -> s {deploymentMode = a} :: DescribeBrokerResponse)

-- | The broker engine version to upgrade to. For a list of supported engine
-- versions, see
-- <https://docs.aws.amazon.com//amazon-mq/latest/developer-guide/broker-engine.html Supported engines>.
describeBrokerResponse_pendingEngineVersion :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_pendingEngineVersion = Lens.lens (\DescribeBrokerResponse' {pendingEngineVersion} -> pendingEngineVersion) (\s@DescribeBrokerResponse' {} a -> s {pendingEngineVersion = a} :: DescribeBrokerResponse)

-- | The unique ID that Amazon MQ generates for the broker.
describeBrokerResponse_brokerId :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_brokerId = Lens.lens (\DescribeBrokerResponse' {brokerId} -> brokerId) (\s@DescribeBrokerResponse' {} a -> s {brokerId = a} :: DescribeBrokerResponse)

-- | The metadata of the LDAP server that will be used to authenticate and
-- authorize connections to the broker after it is rebooted.
describeBrokerResponse_pendingLdapServerMetadata :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe LdapServerMetadataOutput)
describeBrokerResponse_pendingLdapServerMetadata = Lens.lens (\DescribeBrokerResponse' {pendingLdapServerMetadata} -> pendingLdapServerMetadata) (\s@DescribeBrokerResponse' {} a -> s {pendingLdapServerMetadata = a} :: DescribeBrokerResponse)

-- | The type of broker engine. Currently, Amazon MQ supports ACTIVEMQ and
-- RABBITMQ.
describeBrokerResponse_engineType :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe EngineType)
describeBrokerResponse_engineType = Lens.lens (\DescribeBrokerResponse' {engineType} -> engineType) (\s@DescribeBrokerResponse' {} a -> s {engineType = a} :: DescribeBrokerResponse)

-- | The broker\'s Amazon Resource Name (ARN).
describeBrokerResponse_brokerArn :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_brokerArn = Lens.lens (\DescribeBrokerResponse' {brokerArn} -> brokerArn) (\s@DescribeBrokerResponse' {} a -> s {brokerArn = a} :: DescribeBrokerResponse)

-- | The list of all tags associated with this broker.
describeBrokerResponse_tags :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeBrokerResponse_tags = Lens.lens (\DescribeBrokerResponse' {tags} -> tags) (\s@DescribeBrokerResponse' {} a -> s {tags = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about allocated brokers.
describeBrokerResponse_brokerInstances :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe [BrokerInstance])
describeBrokerResponse_brokerInstances = Lens.lens (\DescribeBrokerResponse' {brokerInstances} -> brokerInstances) (\s@DescribeBrokerResponse' {} a -> s {brokerInstances = a} :: DescribeBrokerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s instance type.
describeBrokerResponse_hostInstanceType :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe Prelude.Text)
describeBrokerResponse_hostInstanceType = Lens.lens (\DescribeBrokerResponse' {hostInstanceType} -> hostInstanceType) (\s@DescribeBrokerResponse' {} a -> s {hostInstanceType = a} :: DescribeBrokerResponse)

-- | The broker\'s storage type.
describeBrokerResponse_storageType :: Lens.Lens' DescribeBrokerResponse (Prelude.Maybe BrokerStorageType)
describeBrokerResponse_storageType = Lens.lens (\DescribeBrokerResponse' {storageType} -> storageType) (\s@DescribeBrokerResponse' {} a -> s {storageType = a} :: DescribeBrokerResponse)

-- | The response's http status code.
describeBrokerResponse_httpStatus :: Lens.Lens' DescribeBrokerResponse Prelude.Int
describeBrokerResponse_httpStatus = Lens.lens (\DescribeBrokerResponse' {httpStatus} -> httpStatus) (\s@DescribeBrokerResponse' {} a -> s {httpStatus = a} :: DescribeBrokerResponse)

instance Prelude.NFData DescribeBrokerResponse where
  rnf DescribeBrokerResponse' {..} =
    Prelude.rnf brokerName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf hostInstanceType
      `Prelude.seq` Prelude.rnf brokerInstances
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf brokerArn
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf pendingLdapServerMetadata
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf pendingEngineVersion
      `Prelude.seq` Prelude.rnf deploymentMode
      `Prelude.seq` Prelude.rnf encryptionOptions
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf maintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf ldapServerMetadata
      `Prelude.seq` Prelude.rnf pendingHostInstanceType
      `Prelude.seq` Prelude.rnf authenticationStrategy
      `Prelude.seq` Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf pendingSecurityGroups
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf brokerState
      `Prelude.seq` Prelude.rnf pendingAuthenticationStrategy
      `Prelude.seq` Prelude.rnf engineVersion
