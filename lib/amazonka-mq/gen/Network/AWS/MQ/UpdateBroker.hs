{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.UpdateBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a pending configuration change to a broker.
module Network.AWS.MQ.UpdateBroker
  ( -- * Creating a request
    UpdateBroker (..),
    mkUpdateBroker,

    -- ** Request lenses
    ubEngineVersion,
    ubAutoMinorVersionUpgrade,
    ubSecurityGroups,
    ubAuthenticationStrategy,
    ubLdapServerMetadata,
    ubLogs,
    ubConfiguration,
    ubHostInstanceType,
    ubBrokerId,

    -- * Destructuring the response
    UpdateBrokerResponse (..),
    mkUpdateBrokerResponse,

    -- ** Response lenses
    ubrsEngineVersion,
    ubrsAutoMinorVersionUpgrade,
    ubrsSecurityGroups,
    ubrsAuthenticationStrategy,
    ubrsLdapServerMetadata,
    ubrsLogs,
    ubrsConfiguration,
    ubrsBrokerId,
    ubrsHostInstanceType,
    ubrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Updates the broker using the specified properties.
--
-- /See:/ 'mkUpdateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    securityGroups :: Lude.Maybe [Lude.Text],
    authenticationStrategy :: Lude.Maybe AuthenticationStrategy,
    ldapServerMetadata :: Lude.Maybe LdapServerMetadataInput,
    logs :: Lude.Maybe Logs,
    configuration :: Lude.Maybe ConfigurationId,
    hostInstanceType :: Lude.Maybe Lude.Text,
    brokerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBroker' with the minimum fields required to make a request.
--
-- * 'authenticationStrategy' - The authentication strategy used to secure the broker.
-- * 'autoMinorVersionUpgrade' - Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
-- * 'brokerId' - The unique ID that Amazon MQ generates for the broker.
-- * 'configuration' - A list of information about the configuration.
-- * 'engineVersion' - The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'hostInstanceType' - The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
-- * 'ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize connections to the broker.
-- * 'logs' - Enables Amazon CloudWatch logging for brokers.
-- * 'securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
mkUpdateBroker ::
  -- | 'brokerId'
  Lude.Text ->
  UpdateBroker
mkUpdateBroker pBrokerId_ =
  UpdateBroker'
    { engineVersion = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      securityGroups = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      ldapServerMetadata = Lude.Nothing,
      logs = Lude.Nothing,
      configuration = Lude.Nothing,
      hostInstanceType = Lude.Nothing,
      brokerId = pBrokerId_
    }

-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubEngineVersion :: Lens.Lens' UpdateBroker (Lude.Maybe Lude.Text)
ubEngineVersion = Lens.lens (engineVersion :: UpdateBroker -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: UpdateBroker)
{-# DEPRECATED ubEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAutoMinorVersionUpgrade :: Lens.Lens' UpdateBroker (Lude.Maybe Lude.Bool)
ubAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: UpdateBroker -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: UpdateBroker)
{-# DEPRECATED ubAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubSecurityGroups :: Lens.Lens' UpdateBroker (Lude.Maybe [Lude.Text])
ubSecurityGroups = Lens.lens (securityGroups :: UpdateBroker -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: UpdateBroker)
{-# DEPRECATED ubSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAuthenticationStrategy :: Lens.Lens' UpdateBroker (Lude.Maybe AuthenticationStrategy)
ubAuthenticationStrategy = Lens.lens (authenticationStrategy :: UpdateBroker -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: UpdateBroker)
{-# DEPRECATED ubAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubLdapServerMetadata :: Lens.Lens' UpdateBroker (Lude.Maybe LdapServerMetadataInput)
ubLdapServerMetadata = Lens.lens (ldapServerMetadata :: UpdateBroker -> Lude.Maybe LdapServerMetadataInput) (\s a -> s {ldapServerMetadata = a} :: UpdateBroker)
{-# DEPRECATED ubLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | Enables Amazon CloudWatch logging for brokers.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubLogs :: Lens.Lens' UpdateBroker (Lude.Maybe Logs)
ubLogs = Lens.lens (logs :: UpdateBroker -> Lude.Maybe Logs) (\s a -> s {logs = a} :: UpdateBroker)
{-# DEPRECATED ubLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | A list of information about the configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubConfiguration :: Lens.Lens' UpdateBroker (Lude.Maybe ConfigurationId)
ubConfiguration = Lens.lens (configuration :: UpdateBroker -> Lude.Maybe ConfigurationId) (\s a -> s {configuration = a} :: UpdateBroker)
{-# DEPRECATED ubConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubHostInstanceType :: Lens.Lens' UpdateBroker (Lude.Maybe Lude.Text)
ubHostInstanceType = Lens.lens (hostInstanceType :: UpdateBroker -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: UpdateBroker)
{-# DEPRECATED ubHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubBrokerId :: Lens.Lens' UpdateBroker Lude.Text
ubBrokerId = Lens.lens (brokerId :: UpdateBroker -> Lude.Text) (\s a -> s {brokerId = a} :: UpdateBroker)
{-# DEPRECATED ubBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

instance Lude.AWSRequest UpdateBroker where
  type Rs UpdateBroker = UpdateBrokerResponse
  request = Req.putJSON mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBrokerResponse'
            Lude.<$> (x Lude..?> "engineVersion")
            Lude.<*> (x Lude..?> "autoMinorVersionUpgrade")
            Lude.<*> (x Lude..?> "securityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "authenticationStrategy")
            Lude.<*> (x Lude..?> "ldapServerMetadata")
            Lude.<*> (x Lude..?> "logs")
            Lude.<*> (x Lude..?> "configuration")
            Lude.<*> (x Lude..?> "brokerId")
            Lude.<*> (x Lude..?> "hostInstanceType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBroker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBroker where
  toJSON UpdateBroker' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("engineVersion" Lude..=) Lude.<$> engineVersion,
            ("autoMinorVersionUpgrade" Lude..=)
              Lude.<$> autoMinorVersionUpgrade,
            ("securityGroups" Lude..=) Lude.<$> securityGroups,
            ("authenticationStrategy" Lude..=) Lude.<$> authenticationStrategy,
            ("ldapServerMetadata" Lude..=) Lude.<$> ldapServerMetadata,
            ("logs" Lude..=) Lude.<$> logs,
            ("configuration" Lude..=) Lude.<$> configuration,
            ("hostInstanceType" Lude..=) Lude.<$> hostInstanceType
          ]
      )

instance Lude.ToPath UpdateBroker where
  toPath UpdateBroker' {..} =
    Lude.mconcat ["/v1/brokers/", Lude.toBS brokerId]

instance Lude.ToQuery UpdateBroker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    securityGroups :: Lude.Maybe [Lude.Text],
    authenticationStrategy ::
      Lude.Maybe AuthenticationStrategy,
    ldapServerMetadata ::
      Lude.Maybe LdapServerMetadataOutput,
    logs :: Lude.Maybe Logs,
    configuration :: Lude.Maybe ConfigurationId,
    brokerId :: Lude.Maybe Lude.Text,
    hostInstanceType :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBrokerResponse' with the minimum fields required to make a request.
--
-- * 'authenticationStrategy' - The authentication strategy used to secure the broker.
-- * 'autoMinorVersionUpgrade' - The new value of automatic upgrades to new minor version for brokers.
-- * 'brokerId' - Required. The unique ID that Amazon MQ generates for the broker.
-- * 'configuration' - The ID of the updated configuration.
-- * 'engineVersion' - The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
-- * 'hostInstanceType' - The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
-- * 'ldapServerMetadata' - The metadata of the LDAP server used to authenticate and authorize connections to the broker.
-- * 'logs' - The list of information about logs to be enabled for the specified broker.
-- * 'responseStatus' - The response status code.
-- * 'securityGroups' - The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
mkUpdateBrokerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBrokerResponse
mkUpdateBrokerResponse pResponseStatus_ =
  UpdateBrokerResponse'
    { engineVersion = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      securityGroups = Lude.Nothing,
      authenticationStrategy = Lude.Nothing,
      ldapServerMetadata = Lude.Nothing,
      logs = Lude.Nothing,
      configuration = Lude.Nothing,
      brokerId = Lude.Nothing,
      hostInstanceType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsEngineVersion :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe Lude.Text)
ubrsEngineVersion = Lens.lens (engineVersion :: UpdateBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The new value of automatic upgrades to new minor version for brokers.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsAutoMinorVersionUpgrade :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe Lude.Bool)
ubrsAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: UpdateBrokerResponse -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsSecurityGroups :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe [Lude.Text])
ubrsSecurityGroups = Lens.lens (securityGroups :: UpdateBrokerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsAuthenticationStrategy :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe AuthenticationStrategy)
ubrsAuthenticationStrategy = Lens.lens (authenticationStrategy :: UpdateBrokerResponse -> Lude.Maybe AuthenticationStrategy) (\s a -> s {authenticationStrategy = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsAuthenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead." #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsLdapServerMetadata :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe LdapServerMetadataOutput)
ubrsLdapServerMetadata = Lens.lens (ldapServerMetadata :: UpdateBrokerResponse -> Lude.Maybe LdapServerMetadataOutput) (\s a -> s {ldapServerMetadata = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsLdapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead." #-}

-- | The list of information about logs to be enabled for the specified broker.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsLogs :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe Logs)
ubrsLogs = Lens.lens (logs :: UpdateBrokerResponse -> Lude.Maybe Logs) (\s a -> s {logs = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | The ID of the updated configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsConfiguration :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe ConfigurationId)
ubrsConfiguration = Lens.lens (configuration :: UpdateBrokerResponse -> Lude.Maybe ConfigurationId) (\s a -> s {configuration = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsBrokerId :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe Lude.Text)
ubrsBrokerId = Lens.lens (brokerId :: UpdateBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {brokerId = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsBrokerId "Use generic-lens or generic-optics with 'brokerId' instead." #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsHostInstanceType :: Lens.Lens' UpdateBrokerResponse (Lude.Maybe Lude.Text)
ubrsHostInstanceType = Lens.lens (hostInstanceType :: UpdateBrokerResponse -> Lude.Maybe Lude.Text) (\s a -> s {hostInstanceType = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsResponseStatus :: Lens.Lens' UpdateBrokerResponse Lude.Int
ubrsResponseStatus = Lens.lens (responseStatus :: UpdateBrokerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBrokerResponse)
{-# DEPRECATED ubrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
