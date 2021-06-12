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
-- Module      : Network.AWS.DMS.CreateReplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the replication instance using the specified parameters.
--
-- AWS DMS requires that your account have certain roles with appropriate
-- permissions before you can create a replication instance. For
-- information on the required roles, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.APIRole Creating the IAM Roles to Use With the AWS CLI and AWS DMS API>.
-- For information on the required permissions, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.IAMPermissions IAM Permissions Needed to Use AWS DMS>.
module Network.AWS.DMS.CreateReplicationInstance
  ( -- * Creating a Request
    CreateReplicationInstance (..),
    newCreateReplicationInstance,

    -- * Request Lenses
    createReplicationInstance_replicationSubnetGroupIdentifier,
    createReplicationInstance_multiAZ,
    createReplicationInstance_publiclyAccessible,
    createReplicationInstance_vpcSecurityGroupIds,
    createReplicationInstance_kmsKeyId,
    createReplicationInstance_availabilityZone,
    createReplicationInstance_engineVersion,
    createReplicationInstance_preferredMaintenanceWindow,
    createReplicationInstance_tags,
    createReplicationInstance_resourceIdentifier,
    createReplicationInstance_dnsNameServers,
    createReplicationInstance_allocatedStorage,
    createReplicationInstance_autoMinorVersionUpgrade,
    createReplicationInstance_replicationInstanceIdentifier,
    createReplicationInstance_replicationInstanceClass,

    -- * Destructuring the Response
    CreateReplicationInstanceResponse (..),
    newCreateReplicationInstanceResponse,

    -- * Response Lenses
    createReplicationInstanceResponse_replicationInstance,
    createReplicationInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateReplicationInstance' smart constructor.
data CreateReplicationInstance = CreateReplicationInstance'
  { -- | A subnet group to associate with the replication instance.
    replicationSubnetGroupIdentifier :: Core.Maybe Core.Text,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You
    -- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
    -- set to @true@.
    multiAZ :: Core.Maybe Core.Bool,
    -- | Specifies the accessibility options for the replication instance. A
    -- value of @true@ represents an instance with a public IP address. A value
    -- of @false@ represents an instance with a private IP address. The default
    -- value is @true@.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | Specifies the VPC security group to be used with the replication
    -- instance. The VPC security group must work with the VPC containing the
    -- replication instance.
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | An AWS KMS key identifier that is used to encrypt the data on the
    -- replication instance.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
    -- uses your default encryption key.
    --
    -- AWS KMS creates the default encryption key for your AWS account. Your
    -- AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The Availability Zone where the replication instance will be created.
    -- The default value is a random, system-chosen Availability Zone in the
    -- endpoint\'s AWS Region, for example: @us-east-1d@
    availabilityZone :: Core.Maybe Core.Text,
    -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance
    -- is created, the default is the latest engine version available.
    engineVersion :: Core.Maybe Core.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- Default: A 30-minute window selected at random from an 8-hour block of
    -- time per AWS Region, occurring on a random day of the week.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | One or more tags to be assigned to the replication instance.
    tags :: Core.Maybe [Tag],
    -- | A friendly name for the resource identifier at the end of the
    -- @EndpointArn@ response parameter that is returned in the created
    -- @Endpoint@ object. The value for this parameter can have up to 31
    -- characters. It can contain only ASCII letters, digits, and hyphen
    -- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
    -- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
    -- For example, this value might result in the @EndpointArn@ value
    -- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
    -- specify a @ResourceIdentifier@ value, AWS DMS generates a default
    -- identifier value for the end of @EndpointArn@.
    resourceIdentifier :: Core.Maybe Core.Text,
    -- | A list of custom DNS name servers supported for the replication instance
    -- to access your on-premise source or target database. This list overrides
    -- the default name servers supported by the replication instance. You can
    -- specify a comma-separated list of internet addresses for up to four
    -- on-premise DNS name servers. For example:
    -- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
    dnsNameServers :: Core.Maybe Core.Text,
    -- | The amount of storage (in gigabytes) to be initially allocated for the
    -- replication instance.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the replication instance during the maintenance window.
    -- This parameter defaults to @true@.
    --
    -- Default: @true@
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The replication instance identifier. This parameter is stored as a
    -- lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain 1-63 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @myrepinstance@
    replicationInstanceIdentifier :: Core.Text,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class. For example to specify the
    -- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
    replicationInstanceClass :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroupIdentifier', 'createReplicationInstance_replicationSubnetGroupIdentifier' - A subnet group to associate with the replication instance.
--
-- 'multiAZ', 'createReplicationInstance_multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
--
-- 'publiclyAccessible', 'createReplicationInstance_publiclyAccessible' - Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
--
-- 'vpcSecurityGroupIds', 'createReplicationInstance_vpcSecurityGroupIds' - Specifies the VPC security group to be used with the replication
-- instance. The VPC security group must work with the VPC containing the
-- replication instance.
--
-- 'kmsKeyId', 'createReplicationInstance_kmsKeyId' - An AWS KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
--
-- 'availabilityZone', 'createReplicationInstance_availabilityZone' - The Availability Zone where the replication instance will be created.
-- The default value is a random, system-chosen Availability Zone in the
-- endpoint\'s AWS Region, for example: @us-east-1d@
--
-- 'engineVersion', 'createReplicationInstance_engineVersion' - The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
--
-- 'preferredMaintenanceWindow', 'createReplicationInstance_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per AWS Region, occurring on a random day of the week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
--
-- 'tags', 'createReplicationInstance_tags' - One or more tags to be assigned to the replication instance.
--
-- 'resourceIdentifier', 'createReplicationInstance_resourceIdentifier' - A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, AWS DMS generates a default
-- identifier value for the end of @EndpointArn@.
--
-- 'dnsNameServers', 'createReplicationInstance_dnsNameServers' - A list of custom DNS name servers supported for the replication instance
-- to access your on-premise source or target database. This list overrides
-- the default name servers supported by the replication instance. You can
-- specify a comma-separated list of internet addresses for up to four
-- on-premise DNS name servers. For example:
-- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
--
-- 'allocatedStorage', 'createReplicationInstance_allocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the
-- replication instance.
--
-- 'autoMinorVersionUpgrade', 'createReplicationInstance_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the replication instance during the maintenance window.
-- This parameter defaults to @true@.
--
-- Default: @true@
--
-- 'replicationInstanceIdentifier', 'createReplicationInstance_replicationInstanceIdentifier' - The replication instance identifier. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain 1-63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @myrepinstance@
--
-- 'replicationInstanceClass', 'createReplicationInstance_replicationInstanceClass' - The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
newCreateReplicationInstance ::
  -- | 'replicationInstanceIdentifier'
  Core.Text ->
  -- | 'replicationInstanceClass'
  Core.Text ->
  CreateReplicationInstance
newCreateReplicationInstance
  pReplicationInstanceIdentifier_
  pReplicationInstanceClass_ =
    CreateReplicationInstance'
      { replicationSubnetGroupIdentifier =
          Core.Nothing,
        multiAZ = Core.Nothing,
        publiclyAccessible = Core.Nothing,
        vpcSecurityGroupIds = Core.Nothing,
        kmsKeyId = Core.Nothing,
        availabilityZone = Core.Nothing,
        engineVersion = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        tags = Core.Nothing,
        resourceIdentifier = Core.Nothing,
        dnsNameServers = Core.Nothing,
        allocatedStorage = Core.Nothing,
        autoMinorVersionUpgrade = Core.Nothing,
        replicationInstanceIdentifier =
          pReplicationInstanceIdentifier_,
        replicationInstanceClass =
          pReplicationInstanceClass_
      }

-- | A subnet group to associate with the replication instance.
createReplicationInstance_replicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_replicationSubnetGroupIdentifier = Lens.lens (\CreateReplicationInstance' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@CreateReplicationInstance' {} a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationInstance)

-- | Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
createReplicationInstance_multiAZ :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Bool)
createReplicationInstance_multiAZ = Lens.lens (\CreateReplicationInstance' {multiAZ} -> multiAZ) (\s@CreateReplicationInstance' {} a -> s {multiAZ = a} :: CreateReplicationInstance)

-- | Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
createReplicationInstance_publiclyAccessible :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Bool)
createReplicationInstance_publiclyAccessible = Lens.lens (\CreateReplicationInstance' {publiclyAccessible} -> publiclyAccessible) (\s@CreateReplicationInstance' {} a -> s {publiclyAccessible = a} :: CreateReplicationInstance)

-- | Specifies the VPC security group to be used with the replication
-- instance. The VPC security group must work with the VPC containing the
-- replication instance.
createReplicationInstance_vpcSecurityGroupIds :: Lens.Lens' CreateReplicationInstance (Core.Maybe [Core.Text])
createReplicationInstance_vpcSecurityGroupIds = Lens.lens (\CreateReplicationInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateReplicationInstance' {} a -> s {vpcSecurityGroupIds = a} :: CreateReplicationInstance) Core.. Lens.mapping Lens._Coerce

-- | An AWS KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
createReplicationInstance_kmsKeyId :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_kmsKeyId = Lens.lens (\CreateReplicationInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateReplicationInstance' {} a -> s {kmsKeyId = a} :: CreateReplicationInstance)

-- | The Availability Zone where the replication instance will be created.
-- The default value is a random, system-chosen Availability Zone in the
-- endpoint\'s AWS Region, for example: @us-east-1d@
createReplicationInstance_availabilityZone :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_availabilityZone = Lens.lens (\CreateReplicationInstance' {availabilityZone} -> availabilityZone) (\s@CreateReplicationInstance' {} a -> s {availabilityZone = a} :: CreateReplicationInstance)

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
createReplicationInstance_engineVersion :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_engineVersion = Lens.lens (\CreateReplicationInstance' {engineVersion} -> engineVersion) (\s@CreateReplicationInstance' {} a -> s {engineVersion = a} :: CreateReplicationInstance)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per AWS Region, occurring on a random day of the week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
createReplicationInstance_preferredMaintenanceWindow :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_preferredMaintenanceWindow = Lens.lens (\CreateReplicationInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateReplicationInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateReplicationInstance)

-- | One or more tags to be assigned to the replication instance.
createReplicationInstance_tags :: Lens.Lens' CreateReplicationInstance (Core.Maybe [Tag])
createReplicationInstance_tags = Lens.lens (\CreateReplicationInstance' {tags} -> tags) (\s@CreateReplicationInstance' {} a -> s {tags = a} :: CreateReplicationInstance) Core.. Lens.mapping Lens._Coerce

-- | A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, AWS DMS generates a default
-- identifier value for the end of @EndpointArn@.
createReplicationInstance_resourceIdentifier :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_resourceIdentifier = Lens.lens (\CreateReplicationInstance' {resourceIdentifier} -> resourceIdentifier) (\s@CreateReplicationInstance' {} a -> s {resourceIdentifier = a} :: CreateReplicationInstance)

-- | A list of custom DNS name servers supported for the replication instance
-- to access your on-premise source or target database. This list overrides
-- the default name servers supported by the replication instance. You can
-- specify a comma-separated list of internet addresses for up to four
-- on-premise DNS name servers. For example:
-- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
createReplicationInstance_dnsNameServers :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Text)
createReplicationInstance_dnsNameServers = Lens.lens (\CreateReplicationInstance' {dnsNameServers} -> dnsNameServers) (\s@CreateReplicationInstance' {} a -> s {dnsNameServers = a} :: CreateReplicationInstance)

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- replication instance.
createReplicationInstance_allocatedStorage :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Int)
createReplicationInstance_allocatedStorage = Lens.lens (\CreateReplicationInstance' {allocatedStorage} -> allocatedStorage) (\s@CreateReplicationInstance' {} a -> s {allocatedStorage = a} :: CreateReplicationInstance)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the replication instance during the maintenance window.
-- This parameter defaults to @true@.
--
-- Default: @true@
createReplicationInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateReplicationInstance (Core.Maybe Core.Bool)
createReplicationInstance_autoMinorVersionUpgrade = Lens.lens (\CreateReplicationInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateReplicationInstance' {} a -> s {autoMinorVersionUpgrade = a} :: CreateReplicationInstance)

-- | The replication instance identifier. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain 1-63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @myrepinstance@
createReplicationInstance_replicationInstanceIdentifier :: Lens.Lens' CreateReplicationInstance Core.Text
createReplicationInstance_replicationInstanceIdentifier = Lens.lens (\CreateReplicationInstance' {replicationInstanceIdentifier} -> replicationInstanceIdentifier) (\s@CreateReplicationInstance' {} a -> s {replicationInstanceIdentifier = a} :: CreateReplicationInstance)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
createReplicationInstance_replicationInstanceClass :: Lens.Lens' CreateReplicationInstance Core.Text
createReplicationInstance_replicationInstanceClass = Lens.lens (\CreateReplicationInstance' {replicationInstanceClass} -> replicationInstanceClass) (\s@CreateReplicationInstance' {} a -> s {replicationInstanceClass = a} :: CreateReplicationInstance)

instance Core.AWSRequest CreateReplicationInstance where
  type
    AWSResponse CreateReplicationInstance =
      CreateReplicationInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationInstanceResponse'
            Core.<$> (x Core..?> "ReplicationInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateReplicationInstance

instance Core.NFData CreateReplicationInstance

instance Core.ToHeaders CreateReplicationInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.CreateReplicationInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateReplicationInstance where
  toJSON CreateReplicationInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ReplicationSubnetGroupIdentifier" Core..=)
              Core.<$> replicationSubnetGroupIdentifier,
            ("MultiAZ" Core..=) Core.<$> multiAZ,
            ("PubliclyAccessible" Core..=)
              Core.<$> publiclyAccessible,
            ("VpcSecurityGroupIds" Core..=)
              Core.<$> vpcSecurityGroupIds,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("AvailabilityZone" Core..=)
              Core.<$> availabilityZone,
            ("EngineVersion" Core..=) Core.<$> engineVersion,
            ("PreferredMaintenanceWindow" Core..=)
              Core.<$> preferredMaintenanceWindow,
            ("Tags" Core..=) Core.<$> tags,
            ("ResourceIdentifier" Core..=)
              Core.<$> resourceIdentifier,
            ("DnsNameServers" Core..=) Core.<$> dnsNameServers,
            ("AllocatedStorage" Core..=)
              Core.<$> allocatedStorage,
            ("AutoMinorVersionUpgrade" Core..=)
              Core.<$> autoMinorVersionUpgrade,
            Core.Just
              ( "ReplicationInstanceIdentifier"
                  Core..= replicationInstanceIdentifier
              ),
            Core.Just
              ( "ReplicationInstanceClass"
                  Core..= replicationInstanceClass
              )
          ]
      )

instance Core.ToPath CreateReplicationInstance where
  toPath = Core.const "/"

instance Core.ToQuery CreateReplicationInstance where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newCreateReplicationInstanceResponse' smart constructor.
data CreateReplicationInstanceResponse = CreateReplicationInstanceResponse'
  { -- | The replication instance that was created.
    replicationInstance :: Core.Maybe ReplicationInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicationInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstance', 'createReplicationInstanceResponse_replicationInstance' - The replication instance that was created.
--
-- 'httpStatus', 'createReplicationInstanceResponse_httpStatus' - The response's http status code.
newCreateReplicationInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateReplicationInstanceResponse
newCreateReplicationInstanceResponse pHttpStatus_ =
  CreateReplicationInstanceResponse'
    { replicationInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication instance that was created.
createReplicationInstanceResponse_replicationInstance :: Lens.Lens' CreateReplicationInstanceResponse (Core.Maybe ReplicationInstance)
createReplicationInstanceResponse_replicationInstance = Lens.lens (\CreateReplicationInstanceResponse' {replicationInstance} -> replicationInstance) (\s@CreateReplicationInstanceResponse' {} a -> s {replicationInstance = a} :: CreateReplicationInstanceResponse)

-- | The response's http status code.
createReplicationInstanceResponse_httpStatus :: Lens.Lens' CreateReplicationInstanceResponse Core.Int
createReplicationInstanceResponse_httpStatus = Lens.lens (\CreateReplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationInstanceResponse' {} a -> s {httpStatus = a} :: CreateReplicationInstanceResponse)

instance
  Core.NFData
    CreateReplicationInstanceResponse
