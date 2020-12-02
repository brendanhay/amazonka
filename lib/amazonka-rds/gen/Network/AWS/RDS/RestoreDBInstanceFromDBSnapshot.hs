{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance from a DB snapshot. The target database is created from the source database restore point with the most of original configuration with the default security group and the default DB parameter group. By default, the new DB instance is created as a single-AZ deployment except when the instance is a SQL Server instance that has an option group that is associated with mirroring; in this case, the instance becomes a mirrored AZ deployment and not a single-AZ deployment.
--
--
-- If your intent is to replace your original DB instance with the new, restored DB instance, then rename your original DB instance before you call the RestoreDBInstanceFromDBSnapshot action. RDS doesn't allow two DB instances with the same name. Once you have renamed your original DB instance with a different identifier, then you can pass the original name of the DB instance as the DBInstanceIdentifier in the call to the RestoreDBInstanceFromDBSnapshot action. The result is that you will replace the original DB instance with the DB instance created from the snapshot.
--
-- If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
  ( -- * Creating a Request
    restoreDBInstanceFromDBSnapshot,
    RestoreDBInstanceFromDBSnapshot,

    -- * Request Lenses
    rdifdsDeletionProtection,
    rdifdsPubliclyAccessible,
    rdifdsAutoMinorVersionUpgrade,
    rdifdsDBSubnetGroupName,
    rdifdsIOPS,
    rdifdsDomain,
    rdifdsEngine,
    rdifdsTDECredentialPassword,
    rdifdsProcessorFeatures,
    rdifdsDBInstanceClass,
    rdifdsLicenseModel,
    rdifdsDBParameterGroupName,
    rdifdsAvailabilityZone,
    rdifdsVPCSecurityGroupIds,
    rdifdsMultiAZ,
    rdifdsOptionGroupName,
    rdifdsCopyTagsToSnapshot,
    rdifdsTDECredentialARN,
    rdifdsDomainIAMRoleName,
    rdifdsTags,
    rdifdsPort,
    rdifdsEnableIAMDatabaseAuthentication,
    rdifdsUseDefaultProcessorFeatures,
    rdifdsStorageType,
    rdifdsEnableCloudwatchLogsExports,
    rdifdsDBName,
    rdifdsDBInstanceIdentifier,
    rdifdsDBSnapshotIdentifier,

    -- * Destructuring the Response
    restoreDBInstanceFromDBSnapshotResponse,
    RestoreDBInstanceFromDBSnapshotResponse,

    -- * Response Lenses
    rdifdsrsDBInstance,
    rdifdsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'restoreDBInstanceFromDBSnapshot' smart constructor.
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
  { _rdifdsDeletionProtection ::
      !(Maybe Bool),
    _rdifdsPubliclyAccessible ::
      !(Maybe Bool),
    _rdifdsAutoMinorVersionUpgrade ::
      !(Maybe Bool),
    _rdifdsDBSubnetGroupName ::
      !(Maybe Text),
    _rdifdsIOPS :: !(Maybe Int),
    _rdifdsDomain ::
      !(Maybe Text),
    _rdifdsEngine ::
      !(Maybe Text),
    _rdifdsTDECredentialPassword ::
      !(Maybe Text),
    _rdifdsProcessorFeatures ::
      !(Maybe [ProcessorFeature]),
    _rdifdsDBInstanceClass ::
      !(Maybe Text),
    _rdifdsLicenseModel ::
      !(Maybe Text),
    _rdifdsDBParameterGroupName ::
      !(Maybe Text),
    _rdifdsAvailabilityZone ::
      !(Maybe Text),
    _rdifdsVPCSecurityGroupIds ::
      !(Maybe [Text]),
    _rdifdsMultiAZ ::
      !(Maybe Bool),
    _rdifdsOptionGroupName ::
      !(Maybe Text),
    _rdifdsCopyTagsToSnapshot ::
      !(Maybe Bool),
    _rdifdsTDECredentialARN ::
      !(Maybe Text),
    _rdifdsDomainIAMRoleName ::
      !(Maybe Text),
    _rdifdsTags ::
      !(Maybe [Tag]),
    _rdifdsPort :: !(Maybe Int),
    _rdifdsEnableIAMDatabaseAuthentication ::
      !(Maybe Bool),
    _rdifdsUseDefaultProcessorFeatures ::
      !(Maybe Bool),
    _rdifdsStorageType ::
      !(Maybe Text),
    _rdifdsEnableCloudwatchLogsExports ::
      !(Maybe [Text]),
    _rdifdsDBName ::
      !(Maybe Text),
    _rdifdsDBInstanceIdentifier ::
      !Text,
    _rdifdsDBSnapshotIdentifier ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreDBInstanceFromDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdifdsDeletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- * 'rdifdsPubliclyAccessible' - A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
--
-- * 'rdifdsAutoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- * 'rdifdsDBSubnetGroupName' - The DB subnet group name to use for the new instance. Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetgroup@
--
-- * 'rdifdsIOPS' - Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter isn't specified, the IOPS value is taken from the backup. If this parameter is set to 0, the new instance is converted to a non-PIOPS instance. The conversion takes additional time, though your DB instance is available for connections before the conversion starts.  The provisioned IOPS value must follow the requirements for your database engine. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./  Constraints: Must be an integer greater than 1000.
--
-- * 'rdifdsDomain' - Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- * 'rdifdsEngine' - The database engine to use for the new instance. Default: The same as source Constraint: Must be compatible with the engine of the source. For example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6 snapshot. Valid Values:     * @mariadb@      * @mysql@      * @oracle-ee@      * @oracle-se2@      * @oracle-se1@      * @oracle-se@      * @postgres@      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
--
-- * 'rdifdsTDECredentialPassword' - The password for the given ARN from the key store in order to access the device.
--
-- * 'rdifdsProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- * 'rdifdsDBInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  Default: The same DBInstanceClass as the original DB instance.
--
-- * 'rdifdsLicenseModel' - License model information for the restored DB instance. Default: Same as source. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- * 'rdifdsDBParameterGroupName' - The name of the DB parameter group to associate with this DB instance. If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used. Constraints:     * If supplied, must match the name of an existing DBParameterGroup.     * Must be 1 to 255 letters, numbers, or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens.
--
-- * 'rdifdsAvailabilityZone' - The Availability Zone (AZ) where the DB instance will be created. Default: A random, system-chosen Availability Zone. Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment. Example: @us-east-1a@
--
-- * 'rdifdsVPCSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.  Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- * 'rdifdsMultiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment. Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
--
-- * 'rdifdsOptionGroupName' - The name of the option group to be used for the restored DB instance. Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- * 'rdifdsCopyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
--
-- * 'rdifdsTDECredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
--
-- * 'rdifdsDomainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- * 'rdifdsTags' - Undocumented member.
--
-- * 'rdifdsPort' - The port number on which the database accepts connections. Default: The same port as the original DB instance Constraints: Value must be @1150-65535@
--
-- * 'rdifdsEnableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- * 'rdifdsUseDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- * 'rdifdsStorageType' - Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- * 'rdifdsEnableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
--
-- * 'rdifdsDBName' - The database name for the restored DB instance.
--
-- * 'rdifdsDBInstanceIdentifier' - Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive. Constraints:     * Must contain from 1 to 63 numbers, letters, or hyphens     * First character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
--
-- * 'rdifdsDBSnapshotIdentifier' - The identifier for the DB snapshot to restore from. Constraints:     * Must match the identifier of an existing DBSnapshot.     * If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
restoreDBInstanceFromDBSnapshot ::
  -- | 'rdifdsDBInstanceIdentifier'
  Text ->
  -- | 'rdifdsDBSnapshotIdentifier'
  Text ->
  RestoreDBInstanceFromDBSnapshot
restoreDBInstanceFromDBSnapshot
  pDBInstanceIdentifier_
  pDBSnapshotIdentifier_ =
    RestoreDBInstanceFromDBSnapshot'
      { _rdifdsDeletionProtection =
          Nothing,
        _rdifdsPubliclyAccessible = Nothing,
        _rdifdsAutoMinorVersionUpgrade = Nothing,
        _rdifdsDBSubnetGroupName = Nothing,
        _rdifdsIOPS = Nothing,
        _rdifdsDomain = Nothing,
        _rdifdsEngine = Nothing,
        _rdifdsTDECredentialPassword = Nothing,
        _rdifdsProcessorFeatures = Nothing,
        _rdifdsDBInstanceClass = Nothing,
        _rdifdsLicenseModel = Nothing,
        _rdifdsDBParameterGroupName = Nothing,
        _rdifdsAvailabilityZone = Nothing,
        _rdifdsVPCSecurityGroupIds = Nothing,
        _rdifdsMultiAZ = Nothing,
        _rdifdsOptionGroupName = Nothing,
        _rdifdsCopyTagsToSnapshot = Nothing,
        _rdifdsTDECredentialARN = Nothing,
        _rdifdsDomainIAMRoleName = Nothing,
        _rdifdsTags = Nothing,
        _rdifdsPort = Nothing,
        _rdifdsEnableIAMDatabaseAuthentication = Nothing,
        _rdifdsUseDefaultProcessorFeatures = Nothing,
        _rdifdsStorageType = Nothing,
        _rdifdsEnableCloudwatchLogsExports = Nothing,
        _rdifdsDBName = Nothing,
        _rdifdsDBInstanceIdentifier = pDBInstanceIdentifier_,
        _rdifdsDBSnapshotIdentifier = pDBSnapshotIdentifier_
      }

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
rdifdsDeletionProtection :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsDeletionProtection = lens _rdifdsDeletionProtection (\s a -> s {_rdifdsDeletionProtection = a})

-- | A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
rdifdsPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsPubliclyAccessible = lens _rdifdsPubliclyAccessible (\s a -> s {_rdifdsPubliclyAccessible = a})

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window.
rdifdsAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsAutoMinorVersionUpgrade = lens _rdifdsAutoMinorVersionUpgrade (\s a -> s {_rdifdsAutoMinorVersionUpgrade = a})

-- | The DB subnet group name to use for the new instance. Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetgroup@
rdifdsDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBSubnetGroupName = lens _rdifdsDBSubnetGroupName (\s a -> s {_rdifdsDBSubnetGroupName = a})

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter isn't specified, the IOPS value is taken from the backup. If this parameter is set to 0, the new instance is converted to a non-PIOPS instance. The conversion takes additional time, though your DB instance is available for connections before the conversion starts.  The provisioned IOPS value must follow the requirements for your database engine. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance> in the /Amazon RDS User Guide./  Constraints: Must be an integer greater than 1000.
rdifdsIOPS :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdifdsIOPS = lens _rdifdsIOPS (\s a -> s {_rdifdsIOPS = a})

-- | Specify the Active Directory directory ID to restore the DB instance in. The domain must be created prior to this operation. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
rdifdsDomain :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDomain = lens _rdifdsDomain (\s a -> s {_rdifdsDomain = a})

-- | The database engine to use for the new instance. Default: The same as source Constraint: Must be compatible with the engine of the source. For example, you can restore a MariaDB 10.1 DB instance from a MySQL 5.6 snapshot. Valid Values:     * @mariadb@      * @mysql@      * @oracle-ee@      * @oracle-se2@      * @oracle-se1@      * @oracle-se@      * @postgres@      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
rdifdsEngine :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsEngine = lens _rdifdsEngine (\s a -> s {_rdifdsEngine = a})

-- | The password for the given ARN from the key store in order to access the device.
rdifdsTDECredentialPassword :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsTDECredentialPassword = lens _rdifdsTDECredentialPassword (\s a -> s {_rdifdsTDECredentialPassword = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
rdifdsProcessorFeatures :: Lens' RestoreDBInstanceFromDBSnapshot [ProcessorFeature]
rdifdsProcessorFeatures = lens _rdifdsProcessorFeatures (\s a -> s {_rdifdsProcessorFeatures = a}) . _Default . _Coerce

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  Default: The same DBInstanceClass as the original DB instance.
rdifdsDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBInstanceClass = lens _rdifdsDBInstanceClass (\s a -> s {_rdifdsDBInstanceClass = a})

-- | License model information for the restored DB instance. Default: Same as source. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
rdifdsLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsLicenseModel = lens _rdifdsLicenseModel (\s a -> s {_rdifdsLicenseModel = a})

-- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value for @DBParameterGroupName@ , then the default @DBParameterGroup@ for the specified DB engine is used. Constraints:     * If supplied, must match the name of an existing DBParameterGroup.     * Must be 1 to 255 letters, numbers, or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens.
rdifdsDBParameterGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBParameterGroupName = lens _rdifdsDBParameterGroupName (\s a -> s {_rdifdsDBParameterGroupName = a})

-- | The Availability Zone (AZ) where the DB instance will be created. Default: A random, system-chosen Availability Zone. Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment. Example: @us-east-1a@
rdifdsAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsAvailabilityZone = lens _rdifdsAvailabilityZone (\s a -> s {_rdifdsAvailabilityZone = a})

-- | A list of EC2 VPC security groups to associate with this DB instance.  Default: The default EC2 VPC security group for the DB subnet group's VPC.
rdifdsVPCSecurityGroupIds :: Lens' RestoreDBInstanceFromDBSnapshot [Text]
rdifdsVPCSecurityGroupIds = lens _rdifdsVPCSecurityGroupIds (\s a -> s {_rdifdsVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. Constraint: You can't specify the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ deployment.
rdifdsMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsMultiAZ = lens _rdifdsMultiAZ (\s a -> s {_rdifdsMultiAZ = a})

-- | The name of the option group to be used for the restored DB instance. Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
rdifdsOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsOptionGroupName = lens _rdifdsOptionGroupName (\s a -> s {_rdifdsOptionGroupName = a})

-- | A value that indicates whether to copy all tags from the restored DB instance to snapshots of the DB instance. By default, tags are not copied.
rdifdsCopyTagsToSnapshot :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsCopyTagsToSnapshot = lens _rdifdsCopyTagsToSnapshot (\s a -> s {_rdifdsCopyTagsToSnapshot = a})

-- | The ARN from the key store with which to associate the instance for TDE encryption.
rdifdsTDECredentialARN :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsTDECredentialARN = lens _rdifdsTDECredentialARN (\s a -> s {_rdifdsTDECredentialARN = a})

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
rdifdsDomainIAMRoleName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDomainIAMRoleName = lens _rdifdsDomainIAMRoleName (\s a -> s {_rdifdsDomainIAMRoleName = a})

-- | Undocumented member.
rdifdsTags :: Lens' RestoreDBInstanceFromDBSnapshot [Tag]
rdifdsTags = lens _rdifdsTags (\s a -> s {_rdifdsTags = a}) . _Default . _Coerce

-- | The port number on which the database accepts connections. Default: The same port as the original DB instance Constraints: Value must be @1150-65535@
rdifdsPort :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdifdsPort = lens _rdifdsPort (\s a -> s {_rdifdsPort = a})

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
rdifdsEnableIAMDatabaseAuthentication :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsEnableIAMDatabaseAuthentication = lens _rdifdsEnableIAMDatabaseAuthentication (\s a -> s {_rdifdsEnableIAMDatabaseAuthentication = a})

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
rdifdsUseDefaultProcessorFeatures :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsUseDefaultProcessorFeatures = lens _rdifdsUseDefaultProcessorFeatures (\s a -> s {_rdifdsUseDefaultProcessorFeatures = a})

-- | Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
rdifdsStorageType :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsStorageType = lens _rdifdsStorageType (\s a -> s {_rdifdsStorageType = a})

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon RDS User Guide/ .
rdifdsEnableCloudwatchLogsExports :: Lens' RestoreDBInstanceFromDBSnapshot [Text]
rdifdsEnableCloudwatchLogsExports = lens _rdifdsEnableCloudwatchLogsExports (\s a -> s {_rdifdsEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The database name for the restored DB instance.
rdifdsDBName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBName = lens _rdifdsDBName (\s a -> s {_rdifdsDBName = a})

-- | Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive. Constraints:     * Must contain from 1 to 63 numbers, letters, or hyphens     * First character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
rdifdsDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdifdsDBInstanceIdentifier = lens _rdifdsDBInstanceIdentifier (\s a -> s {_rdifdsDBInstanceIdentifier = a})

-- | The identifier for the DB snapshot to restore from. Constraints:     * Must match the identifier of an existing DBSnapshot.     * If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
rdifdsDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdifdsDBSnapshotIdentifier = lens _rdifdsDBSnapshotIdentifier (\s a -> s {_rdifdsDBSnapshotIdentifier = a})

instance AWSRequest RestoreDBInstanceFromDBSnapshot where
  type
    Rs RestoreDBInstanceFromDBSnapshot =
      RestoreDBInstanceFromDBSnapshotResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "RestoreDBInstanceFromDBSnapshotResult"
      ( \s h x ->
          RestoreDBInstanceFromDBSnapshotResponse'
            <$> (x .@? "DBInstance") <*> (pure (fromEnum s))
      )

instance Hashable RestoreDBInstanceFromDBSnapshot

instance NFData RestoreDBInstanceFromDBSnapshot

instance ToHeaders RestoreDBInstanceFromDBSnapshot where
  toHeaders = const mempty

instance ToPath RestoreDBInstanceFromDBSnapshot where
  toPath = const "/"

instance ToQuery RestoreDBInstanceFromDBSnapshot where
  toQuery RestoreDBInstanceFromDBSnapshot' {..} =
    mconcat
      [ "Action" =: ("RestoreDBInstanceFromDBSnapshot" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DeletionProtection" =: _rdifdsDeletionProtection,
        "PubliclyAccessible" =: _rdifdsPubliclyAccessible,
        "AutoMinorVersionUpgrade" =: _rdifdsAutoMinorVersionUpgrade,
        "DBSubnetGroupName" =: _rdifdsDBSubnetGroupName,
        "Iops" =: _rdifdsIOPS,
        "Domain" =: _rdifdsDomain,
        "Engine" =: _rdifdsEngine,
        "TdeCredentialPassword" =: _rdifdsTDECredentialPassword,
        "ProcessorFeatures"
          =: toQuery
            (toQueryList "ProcessorFeature" <$> _rdifdsProcessorFeatures),
        "DBInstanceClass" =: _rdifdsDBInstanceClass,
        "LicenseModel" =: _rdifdsLicenseModel,
        "DBParameterGroupName" =: _rdifdsDBParameterGroupName,
        "AvailabilityZone" =: _rdifdsAvailabilityZone,
        "VpcSecurityGroupIds"
          =: toQuery
            (toQueryList "VpcSecurityGroupId" <$> _rdifdsVPCSecurityGroupIds),
        "MultiAZ" =: _rdifdsMultiAZ,
        "OptionGroupName" =: _rdifdsOptionGroupName,
        "CopyTagsToSnapshot" =: _rdifdsCopyTagsToSnapshot,
        "TdeCredentialArn" =: _rdifdsTDECredentialARN,
        "DomainIAMRoleName" =: _rdifdsDomainIAMRoleName,
        "Tags" =: toQuery (toQueryList "Tag" <$> _rdifdsTags),
        "Port" =: _rdifdsPort,
        "EnableIAMDatabaseAuthentication"
          =: _rdifdsEnableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures"
          =: _rdifdsUseDefaultProcessorFeatures,
        "StorageType" =: _rdifdsStorageType,
        "EnableCloudwatchLogsExports"
          =: toQuery
            (toQueryList "member" <$> _rdifdsEnableCloudwatchLogsExports),
        "DBName" =: _rdifdsDBName,
        "DBInstanceIdentifier" =: _rdifdsDBInstanceIdentifier,
        "DBSnapshotIdentifier" =: _rdifdsDBSnapshotIdentifier
      ]

-- | /See:/ 'restoreDBInstanceFromDBSnapshotResponse' smart constructor.
data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse'
  { _rdifdsrsDBInstance ::
      !( Maybe
           DBInstance
       ),
    _rdifdsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreDBInstanceFromDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdifdsrsDBInstance' - Undocumented member.
--
-- * 'rdifdsrsResponseStatus' - -- | The response status code.
restoreDBInstanceFromDBSnapshotResponse ::
  -- | 'rdifdsrsResponseStatus'
  Int ->
  RestoreDBInstanceFromDBSnapshotResponse
restoreDBInstanceFromDBSnapshotResponse pResponseStatus_ =
  RestoreDBInstanceFromDBSnapshotResponse'
    { _rdifdsrsDBInstance =
        Nothing,
      _rdifdsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rdifdsrsDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResponse (Maybe DBInstance)
rdifdsrsDBInstance = lens _rdifdsrsDBInstance (\s a -> s {_rdifdsrsDBInstance = a})

-- | -- | The response status code.
rdifdsrsResponseStatus :: Lens' RestoreDBInstanceFromDBSnapshotResponse Int
rdifdsrsResponseStatus = lens _rdifdsrsResponseStatus (\s a -> s {_rdifdsrsResponseStatus = a})

instance NFData RestoreDBInstanceFromDBSnapshotResponse
