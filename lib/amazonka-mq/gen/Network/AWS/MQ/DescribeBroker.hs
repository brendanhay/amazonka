{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified broker.
module Network.AWS.MQ.DescribeBroker
    (
    -- * Creating a request
      DescribeBroker (..)
    , mkDescribeBroker
    -- ** Request lenses
    , dbfBrokerId

    -- * Destructuring the response
    , DescribeBrokerResponse (..)
    , mkDescribeBrokerResponse
    -- ** Response lenses
    , dbrrsAuthenticationStrategy
    , dbrrsAutoMinorVersionUpgrade
    , dbrrsBrokerArn
    , dbrrsBrokerId
    , dbrrsBrokerInstances
    , dbrrsBrokerName
    , dbrrsBrokerState
    , dbrrsConfigurations
    , dbrrsCreated
    , dbrrsDeploymentMode
    , dbrrsEncryptionOptions
    , dbrrsEngineType
    , dbrrsEngineVersion
    , dbrrsHostInstanceType
    , dbrrsLdapServerMetadata
    , dbrrsLogs
    , dbrrsMaintenanceWindowStartTime
    , dbrrsPendingAuthenticationStrategy
    , dbrrsPendingEngineVersion
    , dbrrsPendingHostInstanceType
    , dbrrsPendingLdapServerMetadata
    , dbrrsPendingSecurityGroups
    , dbrrsPubliclyAccessible
    , dbrrsSecurityGroups
    , dbrrsStorageType
    , dbrrsSubnetIds
    , dbrrsTags
    , dbrrsUsers
    , dbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBroker' smart constructor.
newtype DescribeBroker = DescribeBroker'
  { brokerId :: Core.Text
    -- ^ The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBroker' value with any optional fields omitted.
mkDescribeBroker
    :: Core.Text -- ^ 'brokerId'
    -> DescribeBroker
mkDescribeBroker brokerId = DescribeBroker'{brokerId}

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfBrokerId :: Lens.Lens' DescribeBroker Core.Text
dbfBrokerId = Lens.field @"brokerId"
{-# INLINEABLE dbfBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

instance Core.ToQuery DescribeBroker where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBroker where
        toHeaders DescribeBroker{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeBroker where
        type Rs DescribeBroker = DescribeBrokerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/v1/brokers/" Core.<> Core.toText brokerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBrokerResponse' Core.<$>
                   (x Core..:? "authenticationStrategy") Core.<*>
                     x Core..:? "autoMinorVersionUpgrade"
                     Core.<*> x Core..:? "brokerArn"
                     Core.<*> x Core..:? "brokerId"
                     Core.<*> x Core..:? "brokerInstances"
                     Core.<*> x Core..:? "brokerName"
                     Core.<*> x Core..:? "brokerState"
                     Core.<*> x Core..:? "configurations"
                     Core.<*> x Core..:? "created"
                     Core.<*> x Core..:? "deploymentMode"
                     Core.<*> x Core..:? "encryptionOptions"
                     Core.<*> x Core..:? "engineType"
                     Core.<*> x Core..:? "engineVersion"
                     Core.<*> x Core..:? "hostInstanceType"
                     Core.<*> x Core..:? "ldapServerMetadata"
                     Core.<*> x Core..:? "logs"
                     Core.<*> x Core..:? "maintenanceWindowStartTime"
                     Core.<*> x Core..:? "pendingAuthenticationStrategy"
                     Core.<*> x Core..:? "pendingEngineVersion"
                     Core.<*> x Core..:? "pendingHostInstanceType"
                     Core.<*> x Core..:? "pendingLdapServerMetadata"
                     Core.<*> x Core..:? "pendingSecurityGroups"
                     Core.<*> x Core..:? "publiclyAccessible"
                     Core.<*> x Core..:? "securityGroups"
                     Core.<*> x Core..:? "storageType"
                     Core.<*> x Core..:? "subnetIds"
                     Core.<*> x Core..:? "tags"
                     Core.<*> x Core..:? "users"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeBrokerResponse' smart constructor.
data DescribeBrokerResponse = DescribeBrokerResponse'
  { authenticationStrategy :: Core.Maybe Types.AuthenticationStrategy
    -- ^ The authentication strategy used to secure the broker.
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
  , brokerArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the broker.
  , brokerId :: Core.Maybe Core.Text
    -- ^ The unique ID that Amazon MQ generates for the broker.
  , brokerInstances :: Core.Maybe [Types.BrokerInstance]
    -- ^ A list of information about allocated brokers.
  , brokerName :: Core.Maybe Core.Text
    -- ^ The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
  , brokerState :: Core.Maybe Types.BrokerState
    -- ^ The status of the broker.
  , configurations :: Core.Maybe Types.Configurations
    -- ^ The list of all revisions for the specified configuration.
  , created :: Core.Maybe Core.UTCTime
    -- ^ The time when the broker was created.
  , deploymentMode :: Core.Maybe Types.DeploymentMode
    -- ^ Required. The deployment mode of the broker.
  , encryptionOptions :: Core.Maybe Types.EncryptionOptions
    -- ^ Encryption options for the broker.
  , engineType :: Core.Maybe Types.EngineType
    -- ^ Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
  , hostInstanceType :: Core.Maybe Core.Text
    -- ^ The broker's instance type.
  , ldapServerMetadata :: Core.Maybe Types.LdapServerMetadataOutput
    -- ^ The metadata of the LDAP server used to authenticate and authorize connections to the broker.
  , logs :: Core.Maybe Types.LogsSummary
    -- ^ The list of information about logs currently enabled and pending to be deployed for the specified broker.
  , maintenanceWindowStartTime :: Core.Maybe Types.WeeklyStartTime
    -- ^ The parameters that determine the WeeklyStartTime.
  , pendingAuthenticationStrategy :: Core.Maybe Types.AuthenticationStrategy
    -- ^ The authentication strategy that will be applied when the broker is rebooted.
  , pendingEngineVersion :: Core.Maybe Core.Text
    -- ^ The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
  , pendingHostInstanceType :: Core.Maybe Core.Text
    -- ^ The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
  , pendingLdapServerMetadata :: Core.Maybe Types.LdapServerMetadataOutput
    -- ^ The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
  , pendingSecurityGroups :: Core.Maybe [Core.Text]
    -- ^ The list of pending security groups to authorize connections to brokers.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
  , storageType :: Core.Maybe Types.BrokerStorageType
    -- ^ The broker's storage type.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The list of all tags associated with this broker.
  , users :: Core.Maybe [Types.UserSummary]
    -- ^ The list of all broker usernames for the specified broker.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBrokerResponse' value with any optional fields omitted.
mkDescribeBrokerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBrokerResponse
mkDescribeBrokerResponse responseStatus
  = DescribeBrokerResponse'{authenticationStrategy = Core.Nothing,
                            autoMinorVersionUpgrade = Core.Nothing, brokerArn = Core.Nothing,
                            brokerId = Core.Nothing, brokerInstances = Core.Nothing,
                            brokerName = Core.Nothing, brokerState = Core.Nothing,
                            configurations = Core.Nothing, created = Core.Nothing,
                            deploymentMode = Core.Nothing, encryptionOptions = Core.Nothing,
                            engineType = Core.Nothing, engineVersion = Core.Nothing,
                            hostInstanceType = Core.Nothing, ldapServerMetadata = Core.Nothing,
                            logs = Core.Nothing, maintenanceWindowStartTime = Core.Nothing,
                            pendingAuthenticationStrategy = Core.Nothing,
                            pendingEngineVersion = Core.Nothing,
                            pendingHostInstanceType = Core.Nothing,
                            pendingLdapServerMetadata = Core.Nothing,
                            pendingSecurityGroups = Core.Nothing,
                            publiclyAccessible = Core.Nothing, securityGroups = Core.Nothing,
                            storageType = Core.Nothing, subnetIds = Core.Nothing,
                            tags = Core.Nothing, users = Core.Nothing, responseStatus}

-- | The authentication strategy used to secure the broker.
--
-- /Note:/ Consider using 'authenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsAuthenticationStrategy :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.AuthenticationStrategy)
dbrrsAuthenticationStrategy = Lens.field @"authenticationStrategy"
{-# INLINEABLE dbrrsAuthenticationStrategy #-}
{-# DEPRECATED authenticationStrategy "Use generic-lens or generic-optics with 'authenticationStrategy' instead"  #-}

-- | Required. Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsAutoMinorVersionUpgrade :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Bool)
dbrrsAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE dbrrsAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | The Amazon Resource Name (ARN) of the broker.
--
-- /Note:/ Consider using 'brokerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBrokerArn :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsBrokerArn = Lens.field @"brokerArn"
{-# INLINEABLE dbrrsBrokerArn #-}
{-# DEPRECATED brokerArn "Use generic-lens or generic-optics with 'brokerArn' instead"  #-}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBrokerId :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsBrokerId = Lens.field @"brokerId"
{-# INLINEABLE dbrrsBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

-- | A list of information about allocated brokers.
--
-- /Note:/ Consider using 'brokerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBrokerInstances :: Lens.Lens' DescribeBrokerResponse (Core.Maybe [Types.BrokerInstance])
dbrrsBrokerInstances = Lens.field @"brokerInstances"
{-# INLINEABLE dbrrsBrokerInstances #-}
{-# DEPRECATED brokerInstances "Use generic-lens or generic-optics with 'brokerInstances' instead"  #-}

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
--
-- /Note:/ Consider using 'brokerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBrokerName :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsBrokerName = Lens.field @"brokerName"
{-# INLINEABLE dbrrsBrokerName #-}
{-# DEPRECATED brokerName "Use generic-lens or generic-optics with 'brokerName' instead"  #-}

-- | The status of the broker.
--
-- /Note:/ Consider using 'brokerState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBrokerState :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.BrokerState)
dbrrsBrokerState = Lens.field @"brokerState"
{-# INLINEABLE dbrrsBrokerState #-}
{-# DEPRECATED brokerState "Use generic-lens or generic-optics with 'brokerState' instead"  #-}

-- | The list of all revisions for the specified configuration.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsConfigurations :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.Configurations)
dbrrsConfigurations = Lens.field @"configurations"
{-# INLINEABLE dbrrsConfigurations #-}
{-# DEPRECATED configurations "Use generic-lens or generic-optics with 'configurations' instead"  #-}

-- | The time when the broker was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsCreated :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.UTCTime)
dbrrsCreated = Lens.field @"created"
{-# INLINEABLE dbrrsCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Required. The deployment mode of the broker.
--
-- /Note:/ Consider using 'deploymentMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsDeploymentMode :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.DeploymentMode)
dbrrsDeploymentMode = Lens.field @"deploymentMode"
{-# INLINEABLE dbrrsDeploymentMode #-}
{-# DEPRECATED deploymentMode "Use generic-lens or generic-optics with 'deploymentMode' instead"  #-}

-- | Encryption options for the broker.
--
-- /Note:/ Consider using 'encryptionOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsEncryptionOptions :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.EncryptionOptions)
dbrrsEncryptionOptions = Lens.field @"encryptionOptions"
{-# INLINEABLE dbrrsEncryptionOptions #-}
{-# DEPRECATED encryptionOptions "Use generic-lens or generic-optics with 'encryptionOptions' instead"  #-}

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports ACTIVEMQ and RABBITMQ.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsEngineType :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.EngineType)
dbrrsEngineType = Lens.field @"engineType"
{-# INLINEABLE dbrrsEngineType #-}
{-# DEPRECATED engineType "Use generic-lens or generic-optics with 'engineType' instead"  #-}

-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsEngineVersion :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE dbrrsEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The broker's instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsHostInstanceType :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsHostInstanceType = Lens.field @"hostInstanceType"
{-# INLINEABLE dbrrsHostInstanceType #-}
{-# DEPRECATED hostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead"  #-}

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /Note:/ Consider using 'ldapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsLdapServerMetadata :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.LdapServerMetadataOutput)
dbrrsLdapServerMetadata = Lens.field @"ldapServerMetadata"
{-# INLINEABLE dbrrsLdapServerMetadata #-}
{-# DEPRECATED ldapServerMetadata "Use generic-lens or generic-optics with 'ldapServerMetadata' instead"  #-}

-- | The list of information about logs currently enabled and pending to be deployed for the specified broker.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsLogs :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.LogsSummary)
dbrrsLogs = Lens.field @"logs"
{-# INLINEABLE dbrrsLogs #-}
{-# DEPRECATED logs "Use generic-lens or generic-optics with 'logs' instead"  #-}

-- | The parameters that determine the WeeklyStartTime.
--
-- /Note:/ Consider using 'maintenanceWindowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsMaintenanceWindowStartTime :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.WeeklyStartTime)
dbrrsMaintenanceWindowStartTime = Lens.field @"maintenanceWindowStartTime"
{-# INLINEABLE dbrrsMaintenanceWindowStartTime #-}
{-# DEPRECATED maintenanceWindowStartTime "Use generic-lens or generic-optics with 'maintenanceWindowStartTime' instead"  #-}

-- | The authentication strategy that will be applied when the broker is rebooted.
--
-- /Note:/ Consider using 'pendingAuthenticationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsPendingAuthenticationStrategy :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.AuthenticationStrategy)
dbrrsPendingAuthenticationStrategy = Lens.field @"pendingAuthenticationStrategy"
{-# INLINEABLE dbrrsPendingAuthenticationStrategy #-}
{-# DEPRECATED pendingAuthenticationStrategy "Use generic-lens or generic-optics with 'pendingAuthenticationStrategy' instead"  #-}

-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- /Note:/ Consider using 'pendingEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsPendingEngineVersion :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsPendingEngineVersion = Lens.field @"pendingEngineVersion"
{-# INLINEABLE dbrrsPendingEngineVersion #-}
{-# DEPRECATED pendingEngineVersion "Use generic-lens or generic-optics with 'pendingEngineVersion' instead"  #-}

-- | The host instance type of the broker to upgrade to. For a list of supported instance types, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide//broker.html#broker-instance-types
--
-- /Note:/ Consider using 'pendingHostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsPendingHostInstanceType :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Text)
dbrrsPendingHostInstanceType = Lens.field @"pendingHostInstanceType"
{-# INLINEABLE dbrrsPendingHostInstanceType #-}
{-# DEPRECATED pendingHostInstanceType "Use generic-lens or generic-optics with 'pendingHostInstanceType' instead"  #-}

-- | The metadata of the LDAP server that will be used to authenticate and authorize connections to the broker once it is rebooted.
--
-- /Note:/ Consider using 'pendingLdapServerMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsPendingLdapServerMetadata :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.LdapServerMetadataOutput)
dbrrsPendingLdapServerMetadata = Lens.field @"pendingLdapServerMetadata"
{-# INLINEABLE dbrrsPendingLdapServerMetadata #-}
{-# DEPRECATED pendingLdapServerMetadata "Use generic-lens or generic-optics with 'pendingLdapServerMetadata' instead"  #-}

-- | The list of pending security groups to authorize connections to brokers.
--
-- /Note:/ Consider using 'pendingSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsPendingSecurityGroups :: Lens.Lens' DescribeBrokerResponse (Core.Maybe [Core.Text])
dbrrsPendingSecurityGroups = Lens.field @"pendingSecurityGroups"
{-# INLINEABLE dbrrsPendingSecurityGroups #-}
{-# DEPRECATED pendingSecurityGroups "Use generic-lens or generic-optics with 'pendingSecurityGroups' instead"  #-}

-- | Required. Enables connections from applications outside of the VPC that hosts the broker's subnets.
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsPubliclyAccessible :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Core.Bool)
dbrrsPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE dbrrsPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | The list of security groups (1 minimum, 5 maximum) that authorizes connections to brokers.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsSecurityGroups :: Lens.Lens' DescribeBrokerResponse (Core.Maybe [Core.Text])
dbrrsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE dbrrsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The broker's storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsStorageType :: Lens.Lens' DescribeBrokerResponse (Core.Maybe Types.BrokerStorageType)
dbrrsStorageType = Lens.field @"storageType"
{-# INLINEABLE dbrrsStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | The list of groups that define which subnets and IP ranges the broker can use from different Availability Zones. A SINGLE_INSTANCE deployment requires one subnet (for example, the default subnet). An ACTIVE_STANDBY_MULTI_AZ deployment (ACTIVEMQ) requires two subnets. A CLUSTER_MULTI_AZ deployment (RABBITMQ) has no subnet requirements when deployed with public accessibility, deployment without public accessibility requires at least one subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsSubnetIds :: Lens.Lens' DescribeBrokerResponse (Core.Maybe [Core.Text])
dbrrsSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE dbrrsSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The list of all tags associated with this broker.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsTags :: Lens.Lens' DescribeBrokerResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dbrrsTags = Lens.field @"tags"
{-# INLINEABLE dbrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The list of all broker usernames for the specified broker.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsUsers :: Lens.Lens' DescribeBrokerResponse (Core.Maybe [Types.UserSummary])
dbrrsUsers = Lens.field @"users"
{-# INLINEABLE dbrrsUsers #-}
{-# DEPRECATED users "Use generic-lens or generic-optics with 'users' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsResponseStatus :: Lens.Lens' DescribeBrokerResponse Core.Int
dbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
