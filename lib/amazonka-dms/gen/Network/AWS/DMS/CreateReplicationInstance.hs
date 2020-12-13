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
    criEngineVersion,
    criPubliclyAccessible,
    criAutoMinorVersionUpgrade,
    criReplicationSubnetGroupIdentifier,
    criPreferredMaintenanceWindow,
    criKMSKeyId,
    criAvailabilityZone,
    criVPCSecurityGroupIds,
    criMultiAZ,
    criAllocatedStorage,
    criDNSNameServers,
    criReplicationInstanceClass,
    criReplicationInstanceIdentifier,
    criResourceIdentifier,
    criTags,

    -- * Destructuring the response
    CreateReplicationInstanceResponse (..),
    mkCreateReplicationInstanceResponse,

    -- ** Response lenses
    crirsReplicationInstance,
    crirsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateReplicationInstance' smart constructor.
data CreateReplicationInstance = CreateReplicationInstance'
  { -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ .
    --
    -- Default: @true@
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | A subnet group to associate with the replication instance.
    replicationSubnetGroupIdentifier :: Lude.Maybe Lude.Text,
    -- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    -- Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week.
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
    --
    -- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
    -- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The amount of storage (in gigabytes) to be initially allocated for the replication instance.
    allocatedStorage :: Lude.Maybe Lude.Int,
    -- | A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
    dnsNameServers :: Lude.Maybe Lude.Text,
    -- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
    --
    -- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
    replicationInstanceClass :: Lude.Text,
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
    replicationInstanceIdentifier :: Lude.Text,
    -- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
    resourceIdentifier :: Lude.Maybe Lude.Text,
    -- | One or more tags to be assigned to the replication instance.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationInstance' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
-- * 'publiclyAccessible' - Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
-- * 'autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ .
--
-- Default: @true@
-- * 'replicationSubnetGroupIdentifier' - A subnet group to associate with the replication instance.
-- * 'preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week.
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
-- Constraints: Minimum 30-minute window.
-- * 'kmsKeyId' - An AWS KMS key identifier that is used to encrypt the data on the replication instance.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'availabilityZone' - The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
-- * 'vpcSecurityGroupIds' - Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
-- * 'multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
-- * 'allocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the replication instance.
-- * 'dnsNameServers' - A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
-- * 'replicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
-- * 'replicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a lowercase string.
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
-- * 'resourceIdentifier' - A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
-- * 'tags' - One or more tags to be assigned to the replication instance.
mkCreateReplicationInstance ::
  -- | 'replicationInstanceClass'
  Lude.Text ->
  -- | 'replicationInstanceIdentifier'
  Lude.Text ->
  CreateReplicationInstance
mkCreateReplicationInstance
  pReplicationInstanceClass_
  pReplicationInstanceIdentifier_ =
    CreateReplicationInstance'
      { engineVersion = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        autoMinorVersionUpgrade = Lude.Nothing,
        replicationSubnetGroupIdentifier = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        multiAZ = Lude.Nothing,
        allocatedStorage = Lude.Nothing,
        dnsNameServers = Lude.Nothing,
        replicationInstanceClass = pReplicationInstanceClass_,
        replicationInstanceIdentifier = pReplicationInstanceIdentifier_,
        resourceIdentifier = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criEngineVersion :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criEngineVersion = Lens.lens (engineVersion :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateReplicationInstance)
{-# DEPRECATED criEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criPubliclyAccessible :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Bool)
criPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateReplicationInstance)
{-# DEPRECATED criPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the replication instance during the maintenance window. This parameter defaults to @true@ .
--
-- Default: @true@
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criAutoMinorVersionUpgrade :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Bool)
criAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CreateReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CreateReplicationInstance)
{-# DEPRECATED criAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | A subnet group to associate with the replication instance.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criReplicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criReplicationSubnetGroupIdentifier = Lens.lens (replicationSubnetGroupIdentifier :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationInstance)
{-# DEPRECATED criReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
-- Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week.
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
-- Constraints: Minimum 30-minute window.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criPreferredMaintenanceWindow :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateReplicationInstance)
{-# DEPRECATED criPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criKMSKeyId :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criKMSKeyId = Lens.lens (kmsKeyId :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateReplicationInstance)
{-# DEPRECATED criKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Availability Zone where the replication instance will be created. The default value is a random, system-chosen Availability Zone in the endpoint's AWS Region, for example: @us-east-1d@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criAvailabilityZone :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criAvailabilityZone = Lens.lens (availabilityZone :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateReplicationInstance)
{-# DEPRECATED criAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Specifies the VPC security group to be used with the replication instance. The VPC security group must work with the VPC containing the replication instance.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criVPCSecurityGroupIds :: Lens.Lens' CreateReplicationInstance (Lude.Maybe [Lude.Text])
criVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: CreateReplicationInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: CreateReplicationInstance)
{-# DEPRECATED criVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criMultiAZ :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Bool)
criMultiAZ = Lens.lens (multiAZ :: CreateReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: CreateReplicationInstance)
{-# DEPRECATED criMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The amount of storage (in gigabytes) to be initially allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criAllocatedStorage :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Int)
criAllocatedStorage = Lens.lens (allocatedStorage :: CreateReplicationInstance -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: CreateReplicationInstance)
{-# DEPRECATED criAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | A list of custom DNS name servers supported for the replication instance to access your on-premise source or target database. This list overrides the default name servers supported by the replication instance. You can specify a comma-separated list of internet addresses for up to four on-premise DNS name servers. For example: @"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4"@
--
-- /Note:/ Consider using 'dnsNameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criDNSNameServers :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criDNSNameServers = Lens.lens (dnsNameServers :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {dnsNameServers = a} :: CreateReplicationInstance)
{-# DEPRECATED criDNSNameServers "Use generic-lens or generic-optics with 'dnsNameServers' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criReplicationInstanceClass :: Lens.Lens' CreateReplicationInstance Lude.Text
criReplicationInstanceClass = Lens.lens (replicationInstanceClass :: CreateReplicationInstance -> Lude.Text) (\s a -> s {replicationInstanceClass = a} :: CreateReplicationInstance)
{-# DEPRECATED criReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

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
criReplicationInstanceIdentifier :: Lens.Lens' CreateReplicationInstance Lude.Text
criReplicationInstanceIdentifier = Lens.lens (replicationInstanceIdentifier :: CreateReplicationInstance -> Lude.Text) (\s a -> s {replicationInstanceIdentifier = a} :: CreateReplicationInstance)
{-# DEPRECATED criReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criResourceIdentifier :: Lens.Lens' CreateReplicationInstance (Lude.Maybe Lude.Text)
criResourceIdentifier = Lens.lens (resourceIdentifier :: CreateReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdentifier = a} :: CreateReplicationInstance)
{-# DEPRECATED criResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | One or more tags to be assigned to the replication instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
criTags :: Lens.Lens' CreateReplicationInstance (Lude.Maybe [Tag])
criTags = Lens.lens (tags :: CreateReplicationInstance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateReplicationInstance)
{-# DEPRECATED criTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateReplicationInstance where
  type
    Rs CreateReplicationInstance =
      CreateReplicationInstanceResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateReplicationInstanceResponse'
            Lude.<$> (x Lude..?> "ReplicationInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReplicationInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.CreateReplicationInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateReplicationInstance where
  toJSON CreateReplicationInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EngineVersion" Lude..=) Lude.<$> engineVersion,
            ("PubliclyAccessible" Lude..=) Lude.<$> publiclyAccessible,
            ("AutoMinorVersionUpgrade" Lude..=)
              Lude.<$> autoMinorVersionUpgrade,
            ("ReplicationSubnetGroupIdentifier" Lude..=)
              Lude.<$> replicationSubnetGroupIdentifier,
            ("PreferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("AvailabilityZone" Lude..=) Lude.<$> availabilityZone,
            ("VpcSecurityGroupIds" Lude..=) Lude.<$> vpcSecurityGroupIds,
            ("MultiAZ" Lude..=) Lude.<$> multiAZ,
            ("AllocatedStorage" Lude..=) Lude.<$> allocatedStorage,
            ("DnsNameServers" Lude..=) Lude.<$> dnsNameServers,
            Lude.Just
              ("ReplicationInstanceClass" Lude..= replicationInstanceClass),
            Lude.Just
              ( "ReplicationInstanceIdentifier"
                  Lude..= replicationInstanceIdentifier
              ),
            ("ResourceIdentifier" Lude..=) Lude.<$> resourceIdentifier,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateReplicationInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReplicationInstance where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkCreateReplicationInstanceResponse' smart constructor.
data CreateReplicationInstanceResponse = CreateReplicationInstanceResponse'
  { -- | The replication instance that was created.
    replicationInstance :: Lude.Maybe ReplicationInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- * 'replicationInstance' - The replication instance that was created.
-- * 'responseStatus' - The response status code.
mkCreateReplicationInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReplicationInstanceResponse
mkCreateReplicationInstanceResponse pResponseStatus_ =
  CreateReplicationInstanceResponse'
    { replicationInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication instance that was created.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crirsReplicationInstance :: Lens.Lens' CreateReplicationInstanceResponse (Lude.Maybe ReplicationInstance)
crirsReplicationInstance = Lens.lens (replicationInstance :: CreateReplicationInstanceResponse -> Lude.Maybe ReplicationInstance) (\s a -> s {replicationInstance = a} :: CreateReplicationInstanceResponse)
{-# DEPRECATED crirsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crirsResponseStatus :: Lens.Lens' CreateReplicationInstanceResponse Lude.Int
crirsResponseStatus = Lens.lens (responseStatus :: CreateReplicationInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReplicationInstanceResponse)
{-# DEPRECATED crirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
