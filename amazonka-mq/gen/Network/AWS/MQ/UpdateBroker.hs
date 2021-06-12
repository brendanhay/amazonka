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
-- Module      : Network.AWS.MQ.UpdateBroker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a pending configuration change to a broker.
module Network.AWS.MQ.UpdateBroker
  ( -- * Creating a Request
    UpdateBroker (..),
    newUpdateBroker,

    -- * Request Lenses
    updateBroker_ldapServerMetadata,
    updateBroker_authenticationStrategy,
    updateBroker_configuration,
    updateBroker_securityGroups,
    updateBroker_logs,
    updateBroker_engineVersion,
    updateBroker_hostInstanceType,
    updateBroker_autoMinorVersionUpgrade,
    updateBroker_brokerId,

    -- * Destructuring the Response
    UpdateBrokerResponse (..),
    newUpdateBrokerResponse,

    -- * Response Lenses
    updateBrokerResponse_ldapServerMetadata,
    updateBrokerResponse_brokerId,
    updateBrokerResponse_authenticationStrategy,
    updateBrokerResponse_configuration,
    updateBrokerResponse_securityGroups,
    updateBrokerResponse_logs,
    updateBrokerResponse_engineVersion,
    updateBrokerResponse_hostInstanceType,
    updateBrokerResponse_autoMinorVersionUpgrade,
    updateBrokerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates the broker using the specified properties.
--
-- /See:/ 'newUpdateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { -- | The metadata of the LDAP server used to authenticate and authorize
    -- connections to the broker.
    ldapServerMetadata :: Core.Maybe LdapServerMetadataInput,
    -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Core.Maybe AuthenticationStrategy,
    -- | A list of information about the configuration.
    configuration :: Core.Maybe ConfigurationId,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Core.Maybe [Core.Text],
    -- | Enables Amazon CloudWatch logging for brokers.
    logs :: Core.Maybe Logs,
    -- | The version of the broker engine. For a list of supported engine
    -- versions, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | The host instance type of the broker to upgrade to. For a list of
    -- supported instance types, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/\/broker.html#broker-instance-types
    hostInstanceType :: Core.Maybe Core.Text,
    -- | Enables automatic upgrades to new minor versions for brokers, as Apache
    -- releases the versions. The automatic upgrades occur during the
    -- maintenance window of the broker or after a manual broker reboot.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ldapServerMetadata', 'updateBroker_ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- 'authenticationStrategy', 'updateBroker_authenticationStrategy' - The authentication strategy used to secure the broker.
--
-- 'configuration', 'updateBroker_configuration' - A list of information about the configuration.
--
-- 'securityGroups', 'updateBroker_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'logs', 'updateBroker_logs' - Enables Amazon CloudWatch logging for brokers.
--
-- 'engineVersion', 'updateBroker_engineVersion' - The version of the broker engine. For a list of supported engine
-- versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
--
-- 'hostInstanceType', 'updateBroker_hostInstanceType' - The host instance type of the broker to upgrade to. For a list of
-- supported instance types, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/\/broker.html#broker-instance-types
--
-- 'autoMinorVersionUpgrade', 'updateBroker_autoMinorVersionUpgrade' - Enables automatic upgrades to new minor versions for brokers, as Apache
-- releases the versions. The automatic upgrades occur during the
-- maintenance window of the broker or after a manual broker reboot.
--
-- 'brokerId', 'updateBroker_brokerId' - The unique ID that Amazon MQ generates for the broker.
newUpdateBroker ::
  -- | 'brokerId'
  Core.Text ->
  UpdateBroker
newUpdateBroker pBrokerId_ =
  UpdateBroker'
    { ldapServerMetadata = Core.Nothing,
      authenticationStrategy = Core.Nothing,
      configuration = Core.Nothing,
      securityGroups = Core.Nothing,
      logs = Core.Nothing,
      engineVersion = Core.Nothing,
      hostInstanceType = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      brokerId = pBrokerId_
    }

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
updateBroker_ldapServerMetadata :: Lens.Lens' UpdateBroker (Core.Maybe LdapServerMetadataInput)
updateBroker_ldapServerMetadata = Lens.lens (\UpdateBroker' {ldapServerMetadata} -> ldapServerMetadata) (\s@UpdateBroker' {} a -> s {ldapServerMetadata = a} :: UpdateBroker)

-- | The authentication strategy used to secure the broker.
updateBroker_authenticationStrategy :: Lens.Lens' UpdateBroker (Core.Maybe AuthenticationStrategy)
updateBroker_authenticationStrategy = Lens.lens (\UpdateBroker' {authenticationStrategy} -> authenticationStrategy) (\s@UpdateBroker' {} a -> s {authenticationStrategy = a} :: UpdateBroker)

-- | A list of information about the configuration.
updateBroker_configuration :: Lens.Lens' UpdateBroker (Core.Maybe ConfigurationId)
updateBroker_configuration = Lens.lens (\UpdateBroker' {configuration} -> configuration) (\s@UpdateBroker' {} a -> s {configuration = a} :: UpdateBroker)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
updateBroker_securityGroups :: Lens.Lens' UpdateBroker (Core.Maybe [Core.Text])
updateBroker_securityGroups = Lens.lens (\UpdateBroker' {securityGroups} -> securityGroups) (\s@UpdateBroker' {} a -> s {securityGroups = a} :: UpdateBroker) Core.. Lens.mapping Lens._Coerce

-- | Enables Amazon CloudWatch logging for brokers.
updateBroker_logs :: Lens.Lens' UpdateBroker (Core.Maybe Logs)
updateBroker_logs = Lens.lens (\UpdateBroker' {logs} -> logs) (\s@UpdateBroker' {} a -> s {logs = a} :: UpdateBroker)

-- | The version of the broker engine. For a list of supported engine
-- versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
updateBroker_engineVersion :: Lens.Lens' UpdateBroker (Core.Maybe Core.Text)
updateBroker_engineVersion = Lens.lens (\UpdateBroker' {engineVersion} -> engineVersion) (\s@UpdateBroker' {} a -> s {engineVersion = a} :: UpdateBroker)

-- | The host instance type of the broker to upgrade to. For a list of
-- supported instance types, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/\/broker.html#broker-instance-types
updateBroker_hostInstanceType :: Lens.Lens' UpdateBroker (Core.Maybe Core.Text)
updateBroker_hostInstanceType = Lens.lens (\UpdateBroker' {hostInstanceType} -> hostInstanceType) (\s@UpdateBroker' {} a -> s {hostInstanceType = a} :: UpdateBroker)

-- | Enables automatic upgrades to new minor versions for brokers, as Apache
-- releases the versions. The automatic upgrades occur during the
-- maintenance window of the broker or after a manual broker reboot.
updateBroker_autoMinorVersionUpgrade :: Lens.Lens' UpdateBroker (Core.Maybe Core.Bool)
updateBroker_autoMinorVersionUpgrade = Lens.lens (\UpdateBroker' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@UpdateBroker' {} a -> s {autoMinorVersionUpgrade = a} :: UpdateBroker)

-- | The unique ID that Amazon MQ generates for the broker.
updateBroker_brokerId :: Lens.Lens' UpdateBroker Core.Text
updateBroker_brokerId = Lens.lens (\UpdateBroker' {brokerId} -> brokerId) (\s@UpdateBroker' {} a -> s {brokerId = a} :: UpdateBroker)

instance Core.AWSRequest UpdateBroker where
  type AWSResponse UpdateBroker = UpdateBrokerResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBrokerResponse'
            Core.<$> (x Core..?> "ldapServerMetadata")
            Core.<*> (x Core..?> "brokerId")
            Core.<*> (x Core..?> "authenticationStrategy")
            Core.<*> (x Core..?> "configuration")
            Core.<*> (x Core..?> "securityGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "logs")
            Core.<*> (x Core..?> "engineVersion")
            Core.<*> (x Core..?> "hostInstanceType")
            Core.<*> (x Core..?> "autoMinorVersionUpgrade")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateBroker

instance Core.NFData UpdateBroker

instance Core.ToHeaders UpdateBroker where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBroker where
  toJSON UpdateBroker' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ldapServerMetadata" Core..=)
              Core.<$> ldapServerMetadata,
            ("authenticationStrategy" Core..=)
              Core.<$> authenticationStrategy,
            ("configuration" Core..=) Core.<$> configuration,
            ("securityGroups" Core..=) Core.<$> securityGroups,
            ("logs" Core..=) Core.<$> logs,
            ("engineVersion" Core..=) Core.<$> engineVersion,
            ("hostInstanceType" Core..=)
              Core.<$> hostInstanceType,
            ("autoMinorVersionUpgrade" Core..=)
              Core.<$> autoMinorVersionUpgrade
          ]
      )

instance Core.ToPath UpdateBroker where
  toPath UpdateBroker' {..} =
    Core.mconcat ["/v1/brokers/", Core.toBS brokerId]

instance Core.ToQuery UpdateBroker where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { -- | The metadata of the LDAP server used to authenticate and authorize
    -- connections to the broker.
    ldapServerMetadata :: Core.Maybe LdapServerMetadataOutput,
    -- | Required. The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | The authentication strategy used to secure the broker.
    authenticationStrategy :: Core.Maybe AuthenticationStrategy,
    -- | The ID of the updated configuration.
    configuration :: Core.Maybe ConfigurationId,
    -- | The list of security groups (1 minimum, 5 maximum) that authorizes
    -- connections to brokers.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The list of information about logs to be enabled for the specified
    -- broker.
    logs :: Core.Maybe Logs,
    -- | The version of the broker engine to upgrade to. For a list of supported
    -- engine versions, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
    engineVersion :: Core.Maybe Core.Text,
    -- | The host instance type of the broker to upgrade to. For a list of
    -- supported instance types, see
    -- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/\/broker.html#broker-instance-types
    hostInstanceType :: Core.Maybe Core.Text,
    -- | The new value of automatic upgrades to new minor version for brokers.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ldapServerMetadata', 'updateBrokerResponse_ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- 'brokerId', 'updateBrokerResponse_brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- 'authenticationStrategy', 'updateBrokerResponse_authenticationStrategy' - The authentication strategy used to secure the broker.
--
-- 'configuration', 'updateBrokerResponse_configuration' - The ID of the updated configuration.
--
-- 'securityGroups', 'updateBrokerResponse_securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
--
-- 'logs', 'updateBrokerResponse_logs' - The list of information about logs to be enabled for the specified
-- broker.
--
-- 'engineVersion', 'updateBrokerResponse_engineVersion' - The version of the broker engine to upgrade to. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
--
-- 'hostInstanceType', 'updateBrokerResponse_hostInstanceType' - The host instance type of the broker to upgrade to. For a list of
-- supported instance types, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/\/broker.html#broker-instance-types
--
-- 'autoMinorVersionUpgrade', 'updateBrokerResponse_autoMinorVersionUpgrade' - The new value of automatic upgrades to new minor version for brokers.
--
-- 'httpStatus', 'updateBrokerResponse_httpStatus' - The response's http status code.
newUpdateBrokerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateBrokerResponse
newUpdateBrokerResponse pHttpStatus_ =
  UpdateBrokerResponse'
    { ldapServerMetadata =
        Core.Nothing,
      brokerId = Core.Nothing,
      authenticationStrategy = Core.Nothing,
      configuration = Core.Nothing,
      securityGroups = Core.Nothing,
      logs = Core.Nothing,
      engineVersion = Core.Nothing,
      hostInstanceType = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
updateBrokerResponse_ldapServerMetadata :: Lens.Lens' UpdateBrokerResponse (Core.Maybe LdapServerMetadataOutput)
updateBrokerResponse_ldapServerMetadata = Lens.lens (\UpdateBrokerResponse' {ldapServerMetadata} -> ldapServerMetadata) (\s@UpdateBrokerResponse' {} a -> s {ldapServerMetadata = a} :: UpdateBrokerResponse)

-- | Required. The unique ID that Amazon MQ generates for the broker.
updateBrokerResponse_brokerId :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
updateBrokerResponse_brokerId = Lens.lens (\UpdateBrokerResponse' {brokerId} -> brokerId) (\s@UpdateBrokerResponse' {} a -> s {brokerId = a} :: UpdateBrokerResponse)

-- | The authentication strategy used to secure the broker.
updateBrokerResponse_authenticationStrategy :: Lens.Lens' UpdateBrokerResponse (Core.Maybe AuthenticationStrategy)
updateBrokerResponse_authenticationStrategy = Lens.lens (\UpdateBrokerResponse' {authenticationStrategy} -> authenticationStrategy) (\s@UpdateBrokerResponse' {} a -> s {authenticationStrategy = a} :: UpdateBrokerResponse)

-- | The ID of the updated configuration.
updateBrokerResponse_configuration :: Lens.Lens' UpdateBrokerResponse (Core.Maybe ConfigurationId)
updateBrokerResponse_configuration = Lens.lens (\UpdateBrokerResponse' {configuration} -> configuration) (\s@UpdateBrokerResponse' {} a -> s {configuration = a} :: UpdateBrokerResponse)

-- | The list of security groups (1 minimum, 5 maximum) that authorizes
-- connections to brokers.
updateBrokerResponse_securityGroups :: Lens.Lens' UpdateBrokerResponse (Core.Maybe [Core.Text])
updateBrokerResponse_securityGroups = Lens.lens (\UpdateBrokerResponse' {securityGroups} -> securityGroups) (\s@UpdateBrokerResponse' {} a -> s {securityGroups = a} :: UpdateBrokerResponse) Core.. Lens.mapping Lens._Coerce

-- | The list of information about logs to be enabled for the specified
-- broker.
updateBrokerResponse_logs :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Logs)
updateBrokerResponse_logs = Lens.lens (\UpdateBrokerResponse' {logs} -> logs) (\s@UpdateBrokerResponse' {} a -> s {logs = a} :: UpdateBrokerResponse)

-- | The version of the broker engine to upgrade to. For a list of supported
-- engine versions, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/broker-engine.html
updateBrokerResponse_engineVersion :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
updateBrokerResponse_engineVersion = Lens.lens (\UpdateBrokerResponse' {engineVersion} -> engineVersion) (\s@UpdateBrokerResponse' {} a -> s {engineVersion = a} :: UpdateBrokerResponse)

-- | The host instance type of the broker to upgrade to. For a list of
-- supported instance types, see
-- https:\/\/docs.aws.amazon.com\/amazon-mq\/latest\/developer-guide\/\/broker.html#broker-instance-types
updateBrokerResponse_hostInstanceType :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Text)
updateBrokerResponse_hostInstanceType = Lens.lens (\UpdateBrokerResponse' {hostInstanceType} -> hostInstanceType) (\s@UpdateBrokerResponse' {} a -> s {hostInstanceType = a} :: UpdateBrokerResponse)

-- | The new value of automatic upgrades to new minor version for brokers.
updateBrokerResponse_autoMinorVersionUpgrade :: Lens.Lens' UpdateBrokerResponse (Core.Maybe Core.Bool)
updateBrokerResponse_autoMinorVersionUpgrade = Lens.lens (\UpdateBrokerResponse' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@UpdateBrokerResponse' {} a -> s {autoMinorVersionUpgrade = a} :: UpdateBrokerResponse)

-- | The response's http status code.
updateBrokerResponse_httpStatus :: Lens.Lens' UpdateBrokerResponse Core.Int
updateBrokerResponse_httpStatus = Lens.lens (\UpdateBrokerResponse' {httpStatus} -> httpStatus) (\s@UpdateBrokerResponse' {} a -> s {httpStatus = a} :: UpdateBrokerResponse)

instance Core.NFData UpdateBrokerResponse
