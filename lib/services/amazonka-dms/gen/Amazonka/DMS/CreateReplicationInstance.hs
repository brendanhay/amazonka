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
-- Module      : Amazonka.DMS.CreateReplicationInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the replication instance using the specified parameters.
--
-- DMS requires that your account have certain roles with appropriate
-- permissions before you can create a replication instance. For
-- information on the required roles, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.APIRole Creating the IAM Roles to Use With the CLI and DMS API>.
-- For information on the required permissions, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#CHAP_Security.IAMPermissions IAM Permissions Needed to Use DMS>.
module Amazonka.DMS.CreateReplicationInstance
  ( -- * Creating a Request
    CreateReplicationInstance (..),
    newCreateReplicationInstance,

    -- * Request Lenses
    createReplicationInstance_tags,
    createReplicationInstance_vpcSecurityGroupIds,
    createReplicationInstance_autoMinorVersionUpgrade,
    createReplicationInstance_availabilityZone,
    createReplicationInstance_publiclyAccessible,
    createReplicationInstance_resourceIdentifier,
    createReplicationInstance_kmsKeyId,
    createReplicationInstance_allocatedStorage,
    createReplicationInstance_preferredMaintenanceWindow,
    createReplicationInstance_dnsNameServers,
    createReplicationInstance_replicationSubnetGroupIdentifier,
    createReplicationInstance_engineVersion,
    createReplicationInstance_networkType,
    createReplicationInstance_multiAZ,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateReplicationInstance' smart constructor.
data CreateReplicationInstance = CreateReplicationInstance'
  { -- | One or more tags to be assigned to the replication instance.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the VPC security group to be used with the replication
    -- instance. The VPC security group must work with the VPC containing the
    -- replication instance.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the replication instance during the maintenance window.
    -- This parameter defaults to @true@.
    --
    -- Default: @true@
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone where the replication instance will be created.
    -- The default value is a random, system-chosen Availability Zone in the
    -- endpoint\'s Amazon Web Services Region, for example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the accessibility options for the replication instance. A
    -- value of @true@ represents an instance with a public IP address. A value
    -- of @false@ represents an instance with a private IP address. The default
    -- value is @true@.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A friendly name for the resource identifier at the end of the
    -- @EndpointArn@ response parameter that is returned in the created
    -- @Endpoint@ object. The value for this parameter can have up to 31
    -- characters. It can contain only ASCII letters, digits, and hyphen
    -- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
    -- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
    -- For example, this value might result in the @EndpointArn@ value
    -- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
    -- specify a @ResourceIdentifier@ value, DMS generates a default identifier
    -- value for the end of @EndpointArn@.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | An KMS key identifier that is used to encrypt the data on the
    -- replication instance.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
    -- uses your default encryption key.
    --
    -- KMS creates the default encryption key for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default
    -- encryption key for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) to be initially allocated for the
    -- replication instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- Default: A 30-minute window selected at random from an 8-hour block of
    -- time per Amazon Web Services Region, occurring on a random day of the
    -- week.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of custom DNS name servers supported for the replication instance
    -- to access your on-premise source or target database. This list overrides
    -- the default name servers supported by the replication instance. You can
    -- specify a comma-separated list of internet addresses for up to four
    -- on-premise DNS name servers. For example:
    -- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
    dnsNameServers :: Prelude.Maybe Prelude.Text,
    -- | A subnet group to associate with the replication instance.
    replicationSubnetGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance
    -- is created, the default is the latest engine version available.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of IP address protocol used by a replication instance, such as
    -- IPv4 only or Dual-stack that supports both IPv4 and IPv6 addressing.
    -- IPv6 only is not yet supported.
    networkType :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You
    -- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
    -- set to @true@.
    multiAZ :: Prelude.Maybe Prelude.Bool,
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
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
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
-- 'tags', 'createReplicationInstance_tags' - One or more tags to be assigned to the replication instance.
--
-- 'vpcSecurityGroupIds', 'createReplicationInstance_vpcSecurityGroupIds' - Specifies the VPC security group to be used with the replication
-- instance. The VPC security group must work with the VPC containing the
-- replication instance.
--
-- 'autoMinorVersionUpgrade', 'createReplicationInstance_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the replication instance during the maintenance window.
-- This parameter defaults to @true@.
--
-- Default: @true@
--
-- 'availabilityZone', 'createReplicationInstance_availabilityZone' - The Availability Zone where the replication instance will be created.
-- The default value is a random, system-chosen Availability Zone in the
-- endpoint\'s Amazon Web Services Region, for example: @us-east-1d@
--
-- 'publiclyAccessible', 'createReplicationInstance_publiclyAccessible' - Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
--
-- 'resourceIdentifier', 'createReplicationInstance_resourceIdentifier' - A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, DMS generates a default identifier
-- value for the end of @EndpointArn@.
--
-- 'kmsKeyId', 'createReplicationInstance_kmsKeyId' - An KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
--
-- 'allocatedStorage', 'createReplicationInstance_allocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the
-- replication instance.
--
-- 'preferredMaintenanceWindow', 'createReplicationInstance_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per Amazon Web Services Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
--
-- 'dnsNameServers', 'createReplicationInstance_dnsNameServers' - A list of custom DNS name servers supported for the replication instance
-- to access your on-premise source or target database. This list overrides
-- the default name servers supported by the replication instance. You can
-- specify a comma-separated list of internet addresses for up to four
-- on-premise DNS name servers. For example:
-- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
--
-- 'replicationSubnetGroupIdentifier', 'createReplicationInstance_replicationSubnetGroupIdentifier' - A subnet group to associate with the replication instance.
--
-- 'engineVersion', 'createReplicationInstance_engineVersion' - The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
--
-- 'networkType', 'createReplicationInstance_networkType' - The type of IP address protocol used by a replication instance, such as
-- IPv4 only or Dual-stack that supports both IPv4 and IPv6 addressing.
-- IPv6 only is not yet supported.
--
-- 'multiAZ', 'createReplicationInstance_multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
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
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
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
      { tags = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        resourceIdentifier = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        allocatedStorage = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        dnsNameServers = Prelude.Nothing,
        replicationSubnetGroupIdentifier =
          Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        networkType = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        replicationInstanceIdentifier =
          pReplicationInstanceIdentifier_,
        replicationInstanceClass =
          pReplicationInstanceClass_
      }

-- | One or more tags to be assigned to the replication instance.
createReplicationInstance_tags :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe [Tag])
createReplicationInstance_tags = Lens.lens (\CreateReplicationInstance' {tags} -> tags) (\s@CreateReplicationInstance' {} a -> s {tags = a} :: CreateReplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the VPC security group to be used with the replication
-- instance. The VPC security group must work with the VPC containing the
-- replication instance.
createReplicationInstance_vpcSecurityGroupIds :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe [Prelude.Text])
createReplicationInstance_vpcSecurityGroupIds = Lens.lens (\CreateReplicationInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateReplicationInstance' {} a -> s {vpcSecurityGroupIds = a} :: CreateReplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the replication instance during the maintenance window.
-- This parameter defaults to @true@.
--
-- Default: @true@
createReplicationInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Bool)
createReplicationInstance_autoMinorVersionUpgrade = Lens.lens (\CreateReplicationInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateReplicationInstance' {} a -> s {autoMinorVersionUpgrade = a} :: CreateReplicationInstance)

-- | The Availability Zone where the replication instance will be created.
-- The default value is a random, system-chosen Availability Zone in the
-- endpoint\'s Amazon Web Services Region, for example: @us-east-1d@
createReplicationInstance_availabilityZone :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_availabilityZone = Lens.lens (\CreateReplicationInstance' {availabilityZone} -> availabilityZone) (\s@CreateReplicationInstance' {} a -> s {availabilityZone = a} :: CreateReplicationInstance)

-- | Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
createReplicationInstance_publiclyAccessible :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Bool)
createReplicationInstance_publiclyAccessible = Lens.lens (\CreateReplicationInstance' {publiclyAccessible} -> publiclyAccessible) (\s@CreateReplicationInstance' {} a -> s {publiclyAccessible = a} :: CreateReplicationInstance)

-- | A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, DMS generates a default identifier
-- value for the end of @EndpointArn@.
createReplicationInstance_resourceIdentifier :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_resourceIdentifier = Lens.lens (\CreateReplicationInstance' {resourceIdentifier} -> resourceIdentifier) (\s@CreateReplicationInstance' {} a -> s {resourceIdentifier = a} :: CreateReplicationInstance)

-- | An KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
createReplicationInstance_kmsKeyId :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_kmsKeyId = Lens.lens (\CreateReplicationInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateReplicationInstance' {} a -> s {kmsKeyId = a} :: CreateReplicationInstance)

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- replication instance.
createReplicationInstance_allocatedStorage :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Int)
createReplicationInstance_allocatedStorage = Lens.lens (\CreateReplicationInstance' {allocatedStorage} -> allocatedStorage) (\s@CreateReplicationInstance' {} a -> s {allocatedStorage = a} :: CreateReplicationInstance)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per Amazon Web Services Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
createReplicationInstance_preferredMaintenanceWindow :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_preferredMaintenanceWindow = Lens.lens (\CreateReplicationInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateReplicationInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateReplicationInstance)

-- | A list of custom DNS name servers supported for the replication instance
-- to access your on-premise source or target database. This list overrides
-- the default name servers supported by the replication instance. You can
-- specify a comma-separated list of internet addresses for up to four
-- on-premise DNS name servers. For example:
-- @\"1.1.1.1,2.2.2.2,3.3.3.3,4.4.4.4\"@
createReplicationInstance_dnsNameServers :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_dnsNameServers = Lens.lens (\CreateReplicationInstance' {dnsNameServers} -> dnsNameServers) (\s@CreateReplicationInstance' {} a -> s {dnsNameServers = a} :: CreateReplicationInstance)

-- | A subnet group to associate with the replication instance.
createReplicationInstance_replicationSubnetGroupIdentifier :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_replicationSubnetGroupIdentifier = Lens.lens (\CreateReplicationInstance' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@CreateReplicationInstance' {} a -> s {replicationSubnetGroupIdentifier = a} :: CreateReplicationInstance)

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
createReplicationInstance_engineVersion :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_engineVersion = Lens.lens (\CreateReplicationInstance' {engineVersion} -> engineVersion) (\s@CreateReplicationInstance' {} a -> s {engineVersion = a} :: CreateReplicationInstance)

-- | The type of IP address protocol used by a replication instance, such as
-- IPv4 only or Dual-stack that supports both IPv4 and IPv6 addressing.
-- IPv6 only is not yet supported.
createReplicationInstance_networkType :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Text)
createReplicationInstance_networkType = Lens.lens (\CreateReplicationInstance' {networkType} -> networkType) (\s@CreateReplicationInstance' {} a -> s {networkType = a} :: CreateReplicationInstance)

-- | Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
createReplicationInstance_multiAZ :: Lens.Lens' CreateReplicationInstance (Prelude.Maybe Prelude.Bool)
createReplicationInstance_multiAZ = Lens.lens (\CreateReplicationInstance' {multiAZ} -> multiAZ) (\s@CreateReplicationInstance' {} a -> s {multiAZ = a} :: CreateReplicationInstance)

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
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
createReplicationInstance_replicationInstanceClass :: Lens.Lens' CreateReplicationInstance Prelude.Text
createReplicationInstance_replicationInstanceClass = Lens.lens (\CreateReplicationInstance' {replicationInstanceClass} -> replicationInstanceClass) (\s@CreateReplicationInstance' {} a -> s {replicationInstanceClass = a} :: CreateReplicationInstance)

instance Core.AWSRequest CreateReplicationInstance where
  type
    AWSResponse CreateReplicationInstance =
      CreateReplicationInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationInstanceResponse'
            Prelude.<$> (x Core..?> "ReplicationInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplicationInstance where
  hashWithSalt _salt CreateReplicationInstance' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` dnsNameServers
      `Prelude.hashWithSalt` replicationSubnetGroupIdentifier
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` replicationInstanceIdentifier
      `Prelude.hashWithSalt` replicationInstanceClass

instance Prelude.NFData CreateReplicationInstance where
  rnf CreateReplicationInstance' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf dnsNameServers
      `Prelude.seq` Prelude.rnf replicationSubnetGroupIdentifier
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf
        replicationInstanceIdentifier
      `Prelude.seq` Prelude.rnf replicationInstanceClass

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
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("VpcSecurityGroupIds" Core..=)
              Prelude.<$> vpcSecurityGroupIds,
            ("AutoMinorVersionUpgrade" Core..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("PubliclyAccessible" Core..=)
              Prelude.<$> publiclyAccessible,
            ("ResourceIdentifier" Core..=)
              Prelude.<$> resourceIdentifier,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("PreferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("DnsNameServers" Core..=)
              Prelude.<$> dnsNameServers,
            ("ReplicationSubnetGroupIdentifier" Core..=)
              Prelude.<$> replicationSubnetGroupIdentifier,
            ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("NetworkType" Core..=) Prelude.<$> networkType,
            ("MultiAZ" Core..=) Prelude.<$> multiAZ,
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
  where
  rnf CreateReplicationInstanceResponse' {..} =
    Prelude.rnf replicationInstance
      `Prelude.seq` Prelude.rnf httpStatus
