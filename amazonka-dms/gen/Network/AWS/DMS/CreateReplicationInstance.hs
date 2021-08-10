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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateReplicationInstance' smart constructor.
data CreateReplicationInstance = CreateReplicationInstance'
  { -- | A subnet group to associate with the replication instance.
    replicationSubnetGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You
    -- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
    -- set to @true@.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the accessibility options for the replication instance. A
    -- value of @true@ represents an instance with a public IP address. A value
    -- of @false@ represents an instance with a private IP address. The default
    -- value is @true@.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the VPC security group to be used with the replication
    -- instance. The VPC security group must work with the VPC containing the
    -- replication instance.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | An AWS KMS key identifier that is used to encrypt the data on the
    -- replication instance.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
    -- uses your default encryption key.
    --
    -- AWS KMS creates the default encryption key for your AWS account. Your
    -- AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone where the replication instance will be created.
    -- The default value is a random, system-chosen Availability Zone in the
    -- endpoint\'s AWS Region, for example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance
    -- is created, the default is the latest engine version available.
    engineVersion :: Prelude.Maybe Prelude.Text,
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
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | One or more tags to be assigned to the replication instance.
    tags :: Prelude.Maybe [Tag],
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
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A list of custom DNS name servers supported for the replication instance
    -- to access your on-premise source or target database. This list overrides
    -- the default name servers supported by the replication instance. You can
    -- specify a comma-separated list of internet addresses for up to four
    -- on-premise DNS name servers. For example:
    -- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
    dnsNameServers :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) to be initially allocated for the
    -- replication instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the replication instance during the maintenance window.
    -- This parameter defaults to @true@.
    --
    -- Default: @true@
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
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
    replicationInstanceIdentifier :: Prelude.Text,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class. For example to specify the
    -- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
    replicationInstanceClass :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'replicationInstanceClass'
  Prelude.Text ->
  CreateReplicationInstance
newCreateReplicationInstance
  pReplicationInstanceIdentifier_
  pReplicationInstanceClass_ =
    CreateReplicationInstance'
      { replicationSubnetGroupIdentifier =
          Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        tags = Prelude.Nothing,
        resourceIdentifier = Prelude.Nothing,
        dnsNameServers = Prelude.Nothing,
        allocatedStorage = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        replicationInstanceIdentifier =
          pReplicationInstanceIdentifier_,
        replicationInstanceClass =
          pReplicationInstanceClass_
      }

-- | A subnet group to associate with the replication instance.
createReplicationInstance_replicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_replicationSubnetGroupIdentifier = Lens.lens (\CreateReplicationInstance' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@CreateReplicationInstance' {} a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationInstance)

-- | Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
createReplicationInstance_multiAZ :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Bool)
createReplicationInstance_multiAZ = Lens.lens (\CreateReplicationInstance' {multiAZ} -> multiAZ) (\s@CreateReplicationInstance' {} a -> s {multiAZ = a} :: CreateReplicationInstance)

-- | Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
createReplicationInstance_publiclyAccessible :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Bool)
createReplicationInstance_publiclyAccessible = Lens.lens (\CreateReplicationInstance' {publiclyAccessible} -> publiclyAccessible) (\s@CreateReplicationInstance' {} a -> s {publiclyAccessible = a} :: CreateReplicationInstance)

-- | Specifies the VPC security group to be used with the replication
-- instance. The VPC security group must work with the VPC containing the
-- replication instance.
createReplicationInstance_vpcSecurityGroupIds :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe [Prelude.Text])
createReplicationInstance_vpcSecurityGroupIds = Lens.lens (\CreateReplicationInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateReplicationInstance' {} a -> s {vpcSecurityGroupIds = a} :: CreateReplicationInstance) Prelude.. Lens.mapping Lens._Coerce

-- | An AWS KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
createReplicationInstance_kmsKeyId :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_kmsKeyId = Lens.lens (\CreateReplicationInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateReplicationInstance' {} a -> s {kmsKeyId = a} :: CreateReplicationInstance)

-- | The Availability Zone where the replication instance will be created.
-- The default value is a random, system-chosen Availability Zone in the
-- endpoint\'s AWS Region, for example: @us-east-1d@
createReplicationInstance_availabilityZone :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_availabilityZone = Lens.lens (\CreateReplicationInstance' {availabilityZone} -> availabilityZone) (\s@CreateReplicationInstance' {} a -> s {availabilityZone = a} :: CreateReplicationInstance)

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
createReplicationInstance_engineVersion :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
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
createReplicationInstance_preferredMaintenanceWindow :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_preferredMaintenanceWindow = Lens.lens (\CreateReplicationInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateReplicationInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateReplicationInstance)

-- | One or more tags to be assigned to the replication instance.
createReplicationInstance_tags :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe [Tag])
createReplicationInstance_tags = Lens.lens (\CreateReplicationInstance' {tags} -> tags) (\s@CreateReplicationInstance' {} a -> s {tags = a} :: CreateReplicationInstance) Prelude.. Lens.mapping Lens._Coerce

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
createReplicationInstance_resourceIdentifier :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_resourceIdentifier = Lens.lens (\CreateReplicationInstance' {resourceIdentifier} -> resourceIdentifier) (\s@CreateReplicationInstance' {} a -> s {resourceIdentifier = a} :: CreateReplicationInstance)

-- | A list of custom DNS name servers supported for the replication instance
-- to access your on-premise source or target database. This list overrides
-- the default name servers supported by the replication instance. You can
-- specify a comma-separated list of internet addresses for up to four
-- on-premise DNS name servers. For example:
-- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
createReplicationInstance_dnsNameServers :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_dnsNameServers = Lens.lens (\CreateReplicationInstance' {dnsNameServers} -> dnsNameServers) (\s@CreateReplicationInstance' {} a -> s {dnsNameServers = a} :: CreateReplicationInstance)

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- replication instance.
createReplicationInstance_allocatedStorage :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Int)
createReplicationInstance_allocatedStorage = Lens.lens (\CreateReplicationInstance' {allocatedStorage} -> allocatedStorage) (\s@CreateReplicationInstance' {} a -> s {allocatedStorage = a} :: CreateReplicationInstance)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the replication instance during the maintenance window.
-- This parameter defaults to @true@.
--
-- Default: @true@
createReplicationInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Bool)
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
createReplicationInstance_replicationInstanceIdentifier :: Lens.Lens' CreateReplicationInstance Prelude.Text
createReplicationInstance_replicationInstanceIdentifier = Lens.lens (\CreateReplicationInstance' {replicationInstanceIdentifier} -> replicationInstanceIdentifier) (\s@CreateReplicationInstance' {} a -> s {replicationInstanceIdentifier = a} :: CreateReplicationInstance)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. For example to specify the
-- instance class dms.c4.large, set this parameter to @\"dms.c4.large\"@.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
createReplicationInstance_replicationInstanceClass :: Lens.Lens' CreateReplicationInstance Prelude.Text
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
            Prelude.<$> (x Core..?> "ReplicationInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplicationInstance

instance Prelude.NFData CreateReplicationInstance

instance Core.ToHeaders CreateReplicationInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.CreateReplicationInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateReplicationInstance where
  toJSON CreateReplicationInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ReplicationSubnetGroupIdentifier" Core..=)
              Prelude.<$> replicationSubnetGroupIdentifier,
            ("MultiAZ" Core..=) Prelude.<$> multiAZ,
            ("PubliclyAccessible" Core..=)
              Prelude.<$> publiclyAccessible,
            ("VpcSecurityGroupIds" Core..=)
              Prelude.<$> vpcSecurityGroupIds,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("PreferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("Tags" Core..=) Prelude.<$> tags,
            ("ResourceIdentifier" Core..=)
              Prelude.<$> resourceIdentifier,
            ("DnsNameServers" Core..=)
              Prelude.<$> dnsNameServers,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("AutoMinorVersionUpgrade" Core..=)
              Prelude.<$> autoMinorVersionUpgrade,
            Prelude.Just
              ( "ReplicationInstanceIdentifier"
                  Core..= replicationInstanceIdentifier
              ),
            Prelude.Just
              ( "ReplicationInstanceClass"
                  Core..= replicationInstanceClass
              )
          ]
      )

instance Core.ToPath CreateReplicationInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateReplicationInstance where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCreateReplicationInstanceResponse' smart constructor.
data CreateReplicationInstanceResponse = CreateReplicationInstanceResponse'
  { -- | The replication instance that was created.
    replicationInstance :: Prelude.Maybe ReplicationInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateReplicationInstanceResponse
newCreateReplicationInstanceResponse pHttpStatus_ =
  CreateReplicationInstanceResponse'
    { replicationInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication instance that was created.
createReplicationInstanceResponse_replicationInstance :: Lens.Lens' CreateReplicationInstanceResponse (Prelude.Maybe ReplicationInstance)
createReplicationInstanceResponse_replicationInstance = Lens.lens (\CreateReplicationInstanceResponse' {replicationInstance} -> replicationInstance) (\s@CreateReplicationInstanceResponse' {} a -> s {replicationInstance = a} :: CreateReplicationInstanceResponse)

-- | The response's http status code.
createReplicationInstanceResponse_httpStatus :: Lens.Lens' CreateReplicationInstanceResponse Prelude.Int
createReplicationInstanceResponse_httpStatus = Lens.lens (\CreateReplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationInstanceResponse' {} a -> s {httpStatus = a} :: CreateReplicationInstanceResponse)

instance
  Prelude.NFData
    CreateReplicationInstanceResponse
