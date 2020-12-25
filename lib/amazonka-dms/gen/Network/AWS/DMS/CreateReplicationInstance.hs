{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CreateReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the replication instance using the specified parameters.
--
-- AWS DMS requires that your account have certain roles with appropriate permissions before you can create a replication instance. For information on the required roles, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.APIRole Creating the IAM Roles to Use With the AWS CLI and AWS DMS API> . For information on the required permissions, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.IAMPermissions IAM Permissions Needed to Use AWS DMS> .
module Network.AWS.DMS.CreateReplicationInstance
  ( -- * Creating a request
    CreateReplicationInstance (..),
    mkCreateReplicationInstance,

    -- ** Request lenses
    criReplicationInstanceIdentifier,
    criReplicationInstanceClass,
    criAllocatedStorage,
    criAutoMinorVersionUpgrade,
    criAvailabilityZone,
    criDnsNameServers,
    criEngineVersion,
    criKmsKeyId,
    criMultiAZ,
    criPreferredMaintenanceWindow,
    criPubliclyAccessible,
    criReplicationSubnetGroupIdentifier,
    criResourceIdentifier,
    criTags,
    criVpcSecurityGroupIds,

    -- * Destructuring the response
    CreateReplicationInstanceResponse (..),
    mkCreateReplicationInstanceResponse,

    -- ** Response lenses
    crirrsReplicationInstance,
    crirrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateReplicationInstance' smart constructor.
data CreateReplicationInstance = CreateReplicationInstance'
  { -- | The replication instance identifier. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain 1-63 alphanumeric characters or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @myrepinstance@
    replicationInstanceIdentifier :: Types.String,
    -- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
    --
    -- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
    replicationInstanceClass :: Types.String,
    -- | The amount of storage (in gigabytes) to be initially allocated for the replication instance.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ .
    --
    -- Default: @true@
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
    availabilityZone :: Core.Maybe Types.String,
    -- | A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
    dnsNameServers :: Core.Maybe Types.String,
    -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
    engineVersion :: Core.Maybe Types.String,
    -- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
    --
    -- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
    -- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Types.String,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
    multiAZ :: Core.Maybe Core.Bool,
    -- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week.
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | A subnet group to associate with the replication instance.
    replicationSubnetGroupIdentifier :: Core.Maybe Types.String,
    -- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
    resourceIdentifier :: Core.Maybe Types.String,
    -- | One or more tags to be assigned to the replication instance.
    tags :: Core.Maybe [Types.Tag],
    -- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
    vpcSecurityGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationInstance' value with any optional fields omitted.
mkCreateReplicationInstance ::
  -- | 'replicationInstanceIdentifier'
  Types.String ->
  -- | 'replicationInstanceClass'
  Types.String ->
  CreateReplicationInstance
mkCreateReplicationInstance
  replicationInstanceIdentifier
  replicationInstanceClass =
    CreateReplicationInstance'
      { replicationInstanceIdentifier,
        replicationInstanceClass,
        allocatedStorage = Core.Nothing,
        autoMinorVersionUpgrade = Core.Nothing,
        availabilityZone = Core.Nothing,
        dnsNameServers = Core.Nothing,
        engineVersion = Core.Nothing,
        kmsKeyId = Core.Nothing,
        multiAZ = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        publiclyAccessible = Core.Nothing,
        replicationSubnetGroupIdentifier = Core.Nothing,
        resourceIdentifier = Core.Nothing,
        tags = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing
      }

-- | The replication instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain 1-63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @myrepinstance@
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criReplicationInstanceIdentifier :: Lens.Lens' CreateReplicationInstance Types.String
criReplicationInstanceIdentifier = Lens.field @"replicationInstanceIdentifier"
{-# DEPRECATED criReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criReplicationInstanceClass :: Lens.Lens' CreateReplicationInstance Types.String
criReplicationInstanceClass = Lens.field @"replicationInstanceClass"
{-# DEPRECATED criReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

-- | The amount of storage (in gigabytes) to be initially allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criAllocatedStorage :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Int)
criAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED criAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ .
--
-- Default: @true@
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criAutoMinorVersionUpgrade :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Bool)
criAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED criAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criAvailabilityZone :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED criAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
--
-- /Note:/ Consider using 'dnsNameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criDnsNameServers :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criDnsNameServers = Lens.field @"dnsNameServers"
{-# DEPRECATED criDnsNameServers "Use generic-lens or generic-optics with 'dnsNameServers' instead." #-}

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criEngineVersion :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED criEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criKmsKeyId :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED criKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criMultiAZ :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Bool)
criMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED criMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week.
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criPreferredMaintenanceWindow :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED criPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criPubliclyAccessible :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Bool)
criPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED criPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A subnet group to associate with the replication instance.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criReplicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# DEPRECATED criReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criResourceIdentifier :: Lens.Lens' CreateReplicationInstance (Core.Maybe Types.String)
criResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED criResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | One or more tags to be assigned to the replication instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criTags :: Lens.Lens' CreateReplicationInstance (Core.Maybe [Types.Tag])
criTags = Lens.field @"tags"
{-# DEPRECATED criTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criVpcSecurityGroupIds :: Lens.Lens' CreateReplicationInstance (Core.Maybe [Types.String])
criVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# DEPRECATED criVpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

instance Core.FromJSON CreateReplicationInstance where
  toJSON CreateReplicationInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationInstanceIdentifier"
                  Core..= replicationInstanceIdentifier
              ),
            Core.Just
              ("ReplicationInstanceClass" Core..= replicationInstanceClass),
            ("AllocatedStorage" Core..=) Core.<$> allocatedStorage,
            ("AutoMinorVersionUpgrade" Core..=)
              Core.<$> autoMinorVersionUpgrade,
            ("AvailabilityZone" Core..=) Core.<$> availabilityZone,
            ("DnsNameServers" Core..=) Core.<$> dnsNameServers,
            ("EngineVersion" Core..=) Core.<$> engineVersion,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("MultiAZ" Core..=) Core.<$> multiAZ,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("PubliclyAccessible" Core..=) Core.<$> publiclyAccessible,
            ("ReplicationSubnetGroupIdentifier" Core..=)
              Core.<$> replicationSubnetGroupIdentifier,
            ("ResourceIdentifier" Core..=) Core.<$> resourceIdentifier,
            ("Tags" Core..=) Core.<$> tags,
            ("VpcSecurityGroupIds" Core..=) Core.<$> vpcSecurityGroupIds
          ]
      )

instance Core.AWSRequest CreateReplicationInstance where
  type
    Rs CreateReplicationInstance =
      CreateReplicationInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.CreateReplicationInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationInstanceResponse'
            Core.<$> (x Core..:? "ReplicationInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkCreateReplicationInstanceResponse' smart constructor.
data CreateReplicationInstanceResponse = CreateReplicationInstanceResponse'
  { -- | The replication instance that was created.
    replicationInstance :: Core.Maybe Types.ReplicationInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateReplicationInstanceResponse' value with any optional fields omitted.
mkCreateReplicationInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateReplicationInstanceResponse
mkCreateReplicationInstanceResponse responseStatus =
  CreateReplicationInstanceResponse'
    { replicationInstance =
        Core.Nothing,
      responseStatus
    }

-- | The replication instance that was created.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crirrsReplicationInstance :: Lens.Lens' CreateReplicationInstanceResponse (Core.Maybe Types.ReplicationInstance)
crirrsReplicationInstance = Lens.field @"replicationInstance"
{-# DEPRECATED crirrsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crirrsResponseStatus :: Lens.Lens' CreateReplicationInstanceResponse Core.Int
crirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
