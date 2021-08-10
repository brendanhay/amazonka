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
-- Module      : Network.AWS.MQ.CreateBroker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a broker. Note: This API is asynchronous.
module Network.AWS.MQ.CreateBroker
  ( -- * Creating a Request
    CreateBroker (..),
    newCreateBroker,

    -- * Request Lenses
    createBroker_encryptionOptions,
    createBroker_brokerName,
    createBroker_storageType,
    createBroker_ldapServerMetadata,
    createBroker_engineType,
    createBroker_authenticationStrategy,
    createBroker_configuration,
    createBroker_creatorRequestId,
    createBroker_subnetIds,
    createBroker_publiclyAccessible,
    createBroker_securityGroups,
    createBroker_logs,
    createBroker_maintenanceWindowStartTime,
    createBroker_engineVersion,
    createBroker_tags,
    createBroker_hostInstanceType,
    createBroker_autoMinorVersionUpgrade,
    createBroker_users,
    createBroker_deploymentMode,

    -- * Destructuring the Response
    CreateBrokerResponse (..),
    newCreateBrokerResponse,

    -- * Response Lenses
    createBrokerResponse_brokerId,
    createBrokerResponse_brokerArn,
    createBrokerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a broker using the specified properties.
--
-- /See:/ 'newCreateBroker' smart constructor.
data CreateBroker = CreateBroker'
  { -- | Encryption options for the broker.
    encryptionOptions :: Prelude.Maybe EncryptionOptions,
    -- | Required. The name of the broker. This value must be unique in your AWS
    -- account, 1-50 characters long, must contain only letters, numbers,
    -- dashes, and underscores, and must not contain whitespaces, brackets,
    -- wildcard characters, or special characters.
    brokerName :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s storage type.
    storageType :: Prelude.Maybe BrokerStorageType,
    -- | The metadata of the LDAP server used to authenticate and authorize
    -- connections to the broker.
    ldapServerMetadata :: Prelude.Maybe LdapServerMetadataInput,
    -- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
    -- ACTIVEMQ and RABBITMQ.
    engineType :: Prelude.Maybe EngineType,
    -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Prelude.Maybe AuthenticationStrategy,
    -- | A list of information about the configuration.
    configuration :: Prelude.Maybe ConfigurationId,
    -- | The unique ID that the requester receives for the created broker. Amazon
    -- MQ passes your ID with the API action. Note: We recommend using a
    -- Universally Unique Identifier (UUID) for the creatorRequestId. You may
    -- omit the creatorRequestId if your application doesn\'t require
    -- idempotency.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The list of groups that define which subnets and IP ranges the broker
    -- can use from different Availability Zones. A SINGLE_INSTANCE deployment
    -- requires one subnet (for example, the default subnet). An
    -- ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A
    -- CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when
    -- deployed with public accessibility, deployment without public
    -- accessibility requires at least one subnet.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | Required. Enables connections from applications outside of the VPC that
    -- hosts the broker\'s subnets.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Enables Amazon CloudWatch logging for brokers.
    logs :: Prelude.Maybe Logs,
    -- | The parameters that determine the WeeklyStartTime.
    maintenanceWindowStartTime :: Prelude.Maybe WeeklyStartTime,
    -- | Required. The version of the broker engine. For a list of supported
    -- engine versions, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Create tags when creating the broker.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Required. The broker\'s instance type.
    hostInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Required. Enables automatic upgrades to new minor versions for brokers,
    -- as Apache releases the versions. The automatic upgrades occur during the
    -- maintenance window of the broker or after a manual broker reboot.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Required. The list of broker users (persons or applications) who can
    -- access queues and topics. For RabbitMQ brokers, one and only one
    -- administrative user is accepted and created when a broker is first
    -- provisioned. All subsequent broker users are created by making RabbitMQ
    -- API calls directly to brokers or via the RabbitMQ Web Console. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    users :: Prelude.Maybe [User],
    -- | Required. The deployment mode of the broker.
    deploymentMode :: Prelude.Maybe DeploymentMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionOptions', 'createBroker_encryptionOptions' - Encryption options for the broker.
--
-- 'brokerName', 'createBroker_brokerName' - Required. The name of the broker. This value must be unique in your AWS
-- account, 1-50 characters long, must contain only letters, numbers,
-- dashes, and underscores, and must not contain whitespaces, brackets,
-- wildcard characters, or special characters.
--
-- 'storageType', 'createBroker_storageType' - The broker\'s storage type.
--
-- 'ldapServerMetadata', 'createBroker_ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- 'engineType', 'createBroker_engineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
--
-- 'authenticationStrategy', 'createBroker_authenticationStrategy' - The authentication strategy used to secure the broker.
--
-- 'configuration', 'createBroker_configuration' - A list of information about the configuration.
--
-- 'creatorRequestId', 'createBroker_creatorRequestId' - The unique ID that the requester receives for the created broker. Amazon
-- MQ passes your ID with the API action. Note: We recommend using a
-- Universally Unique Identifier (UUID) for the creatorRequestId. You may
-- omit the creatorRequestId if your application doesn\'t require
-- idempotency.
--
-- 'subnetIds', 'createBroker_subnetIds' - The list of groups that define which subnets and IP ranges the broker
-- can use from different Availability Zones. A SINGLE_INSTANCE deployment
-- requires one subnet (for example, the default subnet). An
-- ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A
-- CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when
-- deployed with public accessibility, deployment without public
-- accessibility requires at least one subnet.
--
-- 'publiclyAccessible', 'createBroker_publiclyAccessible' - Required. Enables connections from applications outside of the VPC that
-- hosts the broker\'s subnets.
--
-- 'securityGroups', 'createBroker_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'logs', 'createBroker_logs' - Enables Amazon CloudWatch logging for brokers.
--
-- 'maintenanceWindowStartTime', 'createBroker_maintenanceWindowStartTime' - The parameters that determine the WeeklyStartTime.
--
-- 'engineVersion', 'createBroker_engineVersion' - Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
--
-- 'tags', 'createBroker_tags' - Create tags when creating the broker.
--
-- 'hostInstanceType', 'createBroker_hostInstanceType' - Required. The broker\'s instance type.
--
-- 'autoMinorVersionUpgrade', 'createBroker_autoMinorVersionUpgrade' - Required. Enables automatic upgrades to new minor versions for brokers,
-- as Apache releases the versions. The automatic upgrades occur during the
-- maintenance window of the broker or after a manual broker reboot.
--
-- 'users', 'createBroker_users' - Required. The list of broker users (persons or applications) who can
-- access queues and topics. For RabbitMQ brokers, one and only one
-- administrative user is accepted and created when a broker is first
-- provisioned. All subsequent broker users are created by making RabbitMQ
-- API calls directly to brokers or via the RabbitMQ Web Console. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'deploymentMode', 'createBroker_deploymentMode' - Required. The deployment mode of the broker.
newCreateBroker ::
  CreateBroker
newCreateBroker =
  CreateBroker'
    { encryptionOptions = Prelude.Nothing,
      brokerName = Prelude.Nothing,
      storageType = Prelude.Nothing,
      ldapServerMetadata = Prelude.Nothing,
      engineType = Prelude.Nothing,
      authenticationStrategy = Prelude.Nothing,
      configuration = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      logs = Prelude.Nothing,
      maintenanceWindowStartTime = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      users = Prelude.Nothing,
      deploymentMode = Prelude.Nothing
    }

-- | Encryption options for the broker.
createBroker_encryptionOptions :: Lens.Lens' CreateBroker (Prelude.Maybe EncryptionOptions)
createBroker_encryptionOptions = Lens.lens (\CreateBroker' {encryptionOptions} -> encryptionOptions) (\s@CreateBroker' {} a -> s {encryptionOptions = a} :: CreateBroker)

-- | Required. The name of the broker. This value must be unique in your AWS
-- account, 1-50 characters long, must contain only letters, numbers,
-- dashes, and underscores, and must not contain whitespaces, brackets,
-- wildcard characters, or special characters.
createBroker_brokerName :: Lens.Lens' CreateBroker (Prelude.Maybe Prelude.Text)
createBroker_brokerName = Lens.lens (\CreateBroker' {brokerName} -> brokerName) (\s@CreateBroker' {} a -> s {brokerName = a} :: CreateBroker)

-- | The broker\'s storage type.
createBroker_storageType :: Lens.Lens' CreateBroker (Prelude.Maybe BrokerStorageType)
createBroker_storageType = Lens.lens (\CreateBroker' {storageType} -> storageType) (\s@CreateBroker' {} a -> s {storageType = a} :: CreateBroker)

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
createBroker_ldapServerMetadata :: Lens.Lens' CreateBroker (Prelude.Maybe LdapServerMetadataInput)
createBroker_ldapServerMetadata = Lens.lens (\CreateBroker' {ldapServerMetadata} -> ldapServerMetadata) (\s@CreateBroker' {} a -> s {ldapServerMetadata = a} :: CreateBroker)

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports
-- ACTIVEMQ and RABBITMQ.
createBroker_engineType :: Lens.Lens' CreateBroker (Prelude.Maybe EngineType)
createBroker_engineType = Lens.lens (\CreateBroker' {engineType} -> engineType) (\s@CreateBroker' {} a -> s {engineType = a} :: CreateBroker)

-- | The authentication strategy used to secure the broker.
createBroker_authenticationStrategy :: Lens.Lens' CreateBroker (Prelude.Maybe AuthenticationStrategy)
createBroker_authenticationStrategy = Lens.lens (\CreateBroker' {authenticationStrategy} -> authenticationStrategy) (\s@CreateBroker' {} a -> s {authenticationStrategy = a} :: CreateBroker)

-- | A list of information about the configuration.
createBroker_configuration :: Lens.Lens' CreateBroker (Prelude.Maybe ConfigurationId)
createBroker_configuration = Lens.lens (\CreateBroker' {configuration} -> configuration) (\s@CreateBroker' {} a -> s {configuration = a} :: CreateBroker)

-- | The unique ID that the requester receives for the created broker. Amazon
-- MQ passes your ID with the API action. Note: We recommend using a
-- Universally Unique Identifier (UUID) for the creatorRequestId. You may
-- omit the creatorRequestId if your application doesn\'t require
-- idempotency.
createBroker_creatorRequestId :: Lens.Lens' CreateBroker (Prelude.Maybe Prelude.Text)
createBroker_creatorRequestId = Lens.lens (\CreateBroker' {creatorRequestId} -> creatorRequestId) (\s@CreateBroker' {} a -> s {creatorRequestId = a} :: CreateBroker)

-- | The list of groups that define which subnets and IP ranges the broker
-- can use from different Availability Zones. A SINGLE_INSTANCE deployment
-- requires one subnet (for example, the default subnet). An
-- ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A
-- CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when
-- deployed with public accessibility, deployment without public
-- accessibility requires at least one subnet.
createBroker_subnetIds :: Lens.Lens' CreateBroker (Prelude.Maybe [Prelude.Text])
createBroker_subnetIds = Lens.lens (\CreateBroker' {subnetIds} -> subnetIds) (\s@CreateBroker' {} a -> s {subnetIds = a} :: CreateBroker) Prelude.. Lens.mapping Lens._Coerce

-- | Required. Enables connections from applications outside of the VPC that
-- hosts the broker\'s subnets.
createBroker_publiclyAccessible :: Lens.Lens' CreateBroker (Prelude.Maybe Prelude.Bool)
createBroker_publiclyAccessible = Lens.lens (\CreateBroker' {publiclyAccessible} -> publiclyAccessible) (\s@CreateBroker' {} a -> s {publiclyAccessible = a} :: CreateBroker)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
createBroker_securityGroups :: Lens.Lens' CreateBroker (Prelude.Maybe [Prelude.Text])
createBroker_securityGroups = Lens.lens (\CreateBroker' {securityGroups} -> securityGroups) (\s@CreateBroker' {} a -> s {securityGroups = a} :: CreateBroker) Prelude.. Lens.mapping Lens._Coerce

-- | Enables Amazon CloudWatch logging for brokers.
createBroker_logs :: Lens.Lens' CreateBroker (Prelude.Maybe Logs)
createBroker_logs = Lens.lens (\CreateBroker' {logs} -> logs) (\s@CreateBroker' {} a -> s {logs = a} :: CreateBroker)

-- | The parameters that determine the WeeklyStartTime.
createBroker_maintenanceWindowStartTime :: Lens.Lens' CreateBroker (Prelude.Maybe WeeklyStartTime)
createBroker_maintenanceWindowStartTime = Lens.lens (\CreateBroker' {maintenanceWindowStartTime} -> maintenanceWindowStartTime) (\s@CreateBroker' {} a -> s {maintenanceWindowStartTime = a} :: CreateBroker)

-- | Required. The version of the broker engine. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
createBroker_engineVersion :: Lens.Lens' CreateBroker (Prelude.Maybe Prelude.Text)
createBroker_engineVersion = Lens.lens (\CreateBroker' {engineVersion} -> engineVersion) (\s@CreateBroker' {} a -> s {engineVersion = a} :: CreateBroker)

-- | Create tags when creating the broker.
createBroker_tags :: Lens.Lens' CreateBroker (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBroker_tags = Lens.lens (\CreateBroker' {tags} -> tags) (\s@CreateBroker' {} a -> s {tags = a} :: CreateBroker) Prelude.. Lens.mapping Lens._Coerce

-- | Required. The broker\'s instance type.
createBroker_hostInstanceType :: Lens.Lens' CreateBroker (Prelude.Maybe Prelude.Text)
createBroker_hostInstanceType = Lens.lens (\CreateBroker' {hostInstanceType} -> hostInstanceType) (\s@CreateBroker' {} a -> s {hostInstanceType = a} :: CreateBroker)

-- | Required. Enables automatic upgrades to new minor versions for brokers,
-- as Apache releases the versions. The automatic upgrades occur during the
-- maintenance window of the broker or after a manual broker reboot.
createBroker_autoMinorVersionUpgrade :: Lens.Lens' CreateBroker (Prelude.Maybe Prelude.Bool)
createBroker_autoMinorVersionUpgrade = Lens.lens (\CreateBroker' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateBroker' {} a -> s {autoMinorVersionUpgrade = a} :: CreateBroker)

-- | Required. The list of broker users (persons or applications) who can
-- access queues and topics. For RabbitMQ brokers, one and only one
-- administrative user is accepted and created when a broker is first
-- provisioned. All subsequent broker users are created by making RabbitMQ
-- API calls directly to brokers or via the RabbitMQ Web Console. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
createBroker_users :: Lens.Lens' CreateBroker (Prelude.Maybe [User])
createBroker_users = Lens.lens (\CreateBroker' {users} -> users) (\s@CreateBroker' {} a -> s {users = a} :: CreateBroker) Prelude.. Lens.mapping Lens._Coerce

-- | Required. The deployment mode of the broker.
createBroker_deploymentMode :: Lens.Lens' CreateBroker (Prelude.Maybe DeploymentMode)
createBroker_deploymentMode = Lens.lens (\CreateBroker' {deploymentMode} -> deploymentMode) (\s@CreateBroker' {} a -> s {deploymentMode = a} :: CreateBroker)

instance Core.AWSRequest CreateBroker where
  type AWSResponse CreateBroker = CreateBrokerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBrokerResponse'
            Prelude.<$> (x Core..?> "brokerId")
            Prelude.<*> (x Core..?> "brokerArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBroker

instance Prelude.NFData CreateBroker

instance Core.ToHeaders CreateBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBroker where
  toJSON CreateBroker' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionOptions" Core..=)
              Prelude.<$> encryptionOptions,
            ("brokerName" Core..=) Prelude.<$> brokerName,
            ("storageType" Core..=) Prelude.<$> storageType,
            ("ldapServerMetadata" Core..=)
              Prelude.<$> ldapServerMetadata,
            ("engineType" Core..=) Prelude.<$> engineType,
            ("authenticationStrategy" Core..=)
              Prelude.<$> authenticationStrategy,
            ("configuration" Core..=) Prelude.<$> configuration,
            ("creatorRequestId" Core..=)
              Prelude.<$> creatorRequestId,
            ("subnetIds" Core..=) Prelude.<$> subnetIds,
            ("publiclyAccessible" Core..=)
              Prelude.<$> publiclyAccessible,
            ("securityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("logs" Core..=) Prelude.<$> logs,
            ("maintenanceWindowStartTime" Core..=)
              Prelude.<$> maintenanceWindowStartTime,
            ("engineVersion" Core..=) Prelude.<$> engineVersion,
            ("tags" Core..=) Prelude.<$> tags,
            ("hostInstanceType" Core..=)
              Prelude.<$> hostInstanceType,
            ("autoMinorVersionUpgrade" Core..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("users" Core..=) Prelude.<$> users,
            ("deploymentMode" Core..=)
              Prelude.<$> deploymentMode
          ]
      )

instance Core.ToPath CreateBroker where
  toPath = Prelude.const "/v1/brokers"

instance Core.ToQuery CreateBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBrokerResponse' smart constructor.
data CreateBrokerResponse = CreateBrokerResponse'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the broker.
    brokerArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'createBrokerResponse_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'brokerArn', 'createBrokerResponse_brokerArn' - The Amazon Resource Name (ARN) of the broker.
--
-- 'httpStatus', 'createBrokerResponse_httpStatus' - The response's http status code.
newCreateBrokerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBrokerResponse
newCreateBrokerResponse pHttpStatus_ =
  CreateBrokerResponse'
    { brokerId = Prelude.Nothing,
      brokerArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID that Amazon MQ generates for the broker.
createBrokerResponse_brokerId :: Lens.Lens' CreateBrokerResponse (Prelude.Maybe Prelude.Text)
createBrokerResponse_brokerId = Lens.lens (\CreateBrokerResponse' {brokerId} -> brokerId) (\s@CreateBrokerResponse' {} a -> s {brokerId = a} :: CreateBrokerResponse)

-- | The Amazon Resource Name (ARN) of the broker.
createBrokerResponse_brokerArn :: Lens.Lens' CreateBrokerResponse (Prelude.Maybe Prelude.Text)
createBrokerResponse_brokerArn = Lens.lens (\CreateBrokerResponse' {brokerArn} -> brokerArn) (\s@CreateBrokerResponse' {} a -> s {brokerArn = a} :: CreateBrokerResponse)

-- | The response's http status code.
createBrokerResponse_httpStatus :: Lens.Lens' CreateBrokerResponse Prelude.Int
createBrokerResponse_httpStatus = Lens.lens (\CreateBrokerResponse' {httpStatus} -> httpStatus) (\s@CreateBrokerResponse' {} a -> s {httpStatus = a} :: CreateBrokerResponse)

instance Prelude.NFData CreateBrokerResponse
