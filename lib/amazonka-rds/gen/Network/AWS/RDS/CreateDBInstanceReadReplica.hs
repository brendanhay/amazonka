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
-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance that acts as a read replica for an existing source DB instance. You can create a read replica for a DB instance running MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReadRepl.html Working with Read Replicas> in the /Amazon RDS User Guide/ .
--
--
-- Amazon Aurora doesn't support this action. Call the @CreateDBInstance@ action to create a DB instance for an Aurora DB cluster.
--
-- All read replica DB instances are created with backups disabled. All other DB instance attributes (including DB security groups and DB parameter groups) are inherited from the source DB instance, except as specified.
--
-- /Important:/ Your source DB instance must have backup retention enabled.
module Network.AWS.RDS.CreateDBInstanceReadReplica
  ( -- * Creating a Request
    createDBInstanceReadReplica,
    CreateDBInstanceReadReplica,

    -- * Request Lenses
    cdirrDeletionProtection,
    cdirrPubliclyAccessible,
    cdirrAutoMinorVersionUpgrade,
    cdirrDBSubnetGroupName,
    cdirrMonitoringRoleARN,
    cdirrIOPS,
    cdirrDomain,
    cdirrReplicaMode,
    cdirrMonitoringInterval,
    cdirrPreSignedURL,
    cdirrProcessorFeatures,
    cdirrDBInstanceClass,
    cdirrPerformanceInsightsRetentionPeriod,
    cdirrMaxAllocatedStorage,
    cdirrEnablePerformanceInsights,
    cdirrKMSKeyId,
    cdirrDBParameterGroupName,
    cdirrAvailabilityZone,
    cdirrPerformanceInsightsKMSKeyId,
    cdirrVPCSecurityGroupIds,
    cdirrMultiAZ,
    cdirrOptionGroupName,
    cdirrCopyTagsToSnapshot,
    cdirrDomainIAMRoleName,
    cdirrTags,
    cdirrPort,
    cdirrEnableIAMDatabaseAuthentication,
    cdirrUseDefaultProcessorFeatures,
    cdirrStorageType,
    cdirrEnableCloudwatchLogsExports,
    cdirrDBInstanceIdentifier,
    cdirrSourceDBInstanceIdentifier,

    -- * Destructuring the Response
    createDBInstanceReadReplicaResponse,
    CreateDBInstanceReadReplicaResponse,

    -- * Response Lenses
    cdirrrsDBInstance,
    cdirrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { _cdirrDeletionProtection ::
      !(Maybe Bool),
    _cdirrPubliclyAccessible ::
      !(Maybe Bool),
    _cdirrAutoMinorVersionUpgrade ::
      !(Maybe Bool),
    _cdirrDBSubnetGroupName ::
      !(Maybe Text),
    _cdirrMonitoringRoleARN ::
      !(Maybe Text),
    _cdirrIOPS :: !(Maybe Int),
    _cdirrDomain :: !(Maybe Text),
    _cdirrReplicaMode ::
      !(Maybe ReplicaMode),
    _cdirrMonitoringInterval ::
      !(Maybe Int),
    _cdirrPreSignedURL :: !(Maybe Text),
    _cdirrProcessorFeatures ::
      !(Maybe [ProcessorFeature]),
    _cdirrDBInstanceClass ::
      !(Maybe Text),
    _cdirrPerformanceInsightsRetentionPeriod ::
      !(Maybe Int),
    _cdirrMaxAllocatedStorage ::
      !(Maybe Int),
    _cdirrEnablePerformanceInsights ::
      !(Maybe Bool),
    _cdirrKMSKeyId :: !(Maybe Text),
    _cdirrDBParameterGroupName ::
      !(Maybe Text),
    _cdirrAvailabilityZone ::
      !(Maybe Text),
    _cdirrPerformanceInsightsKMSKeyId ::
      !(Maybe Text),
    _cdirrVPCSecurityGroupIds ::
      !(Maybe [Text]),
    _cdirrMultiAZ :: !(Maybe Bool),
    _cdirrOptionGroupName ::
      !(Maybe Text),
    _cdirrCopyTagsToSnapshot ::
      !(Maybe Bool),
    _cdirrDomainIAMRoleName ::
      !(Maybe Text),
    _cdirrTags :: !(Maybe [Tag]),
    _cdirrPort :: !(Maybe Int),
    _cdirrEnableIAMDatabaseAuthentication ::
      !(Maybe Bool),
    _cdirrUseDefaultProcessorFeatures ::
      !(Maybe Bool),
    _cdirrStorageType :: !(Maybe Text),
    _cdirrEnableCloudwatchLogsExports ::
      !(Maybe [Text]),
    _cdirrDBInstanceIdentifier :: !Text,
    _cdirrSourceDBInstanceIdentifier ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDBInstanceReadReplica' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirrDeletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- * 'cdirrPubliclyAccessible' - A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
--
-- * 'cdirrAutoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window. Default: Inherits from the source DB instance
--
-- * 'cdirrDBSubnetGroupName' - Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance isn't created in a VPC. Constraints:     * Can only be specified if the source DB instance identifier specifies a DB instance in another AWS Region.     * If supplied, must match the name of an existing DBSubnetGroup.     * The specified DB subnet group must be in the same AWS Region in which the operation is running.     * All read replicas in one AWS Region that are created from the same source DB instance must either:>     * Specify DB subnet groups from the same VPC. All these read replicas are created in the same VPC.     * Not specify a DB subnet group. All these read replicas are created outside of any VPC. Example: @mySubnetgroup@
--
-- * 'cdirrMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- * 'cdirrIOPS' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- * 'cdirrDomain' - The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- * 'cdirrReplicaMode' - The open mode of the replica database: mounted or read-only. Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload. You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- * 'cdirrMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- * 'cdirrPreSignedURL' - The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance.  You must specify this parameter when you create an encrypted read replica from another AWS Region by using the Amazon RDS API. Don't specify @PreSignedUrl@ when you are creating an encrypted read replica in the same AWS Region. The presigned URL must be a valid request for the @CreateDBInstanceReadReplica@ API action that can be executed in the source AWS Region that contains the encrypted source DB instance. The presigned URL request must contain the following parameter values:      * @DestinationRegion@ - The AWS Region that the encrypted read replica is created in. This AWS Region is the same one where the @CreateDBInstanceReadReplica@ action is called that contains this presigned URL. For example, if you create an encrypted DB instance in the us-west-1 AWS Region, from a source DB instance in the us-east-2 AWS Region, then you call the @CreateDBInstanceReadReplica@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CreateDBInstanceReadReplica@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.      * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the read replica in the destination AWS Region. This is the same identifier for both the @CreateDBInstanceReadReplica@ action that is called in the destination AWS Region, and the action contained in the presigned URL.      * @SourceDBInstanceIdentifier@ - The DB instance identifier for the encrypted DB instance to be replicated. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are creating an encrypted read replica from a DB instance in the us-west-2 AWS Region, then your @SourceDBInstanceIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@ .  To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- * 'cdirrProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- * 'cdirrDBInstanceClass' - The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  Default: Inherits from the source DB instance.
--
-- * 'cdirrPerformanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- * 'cdirrMaxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- * 'cdirrEnablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the read replica.  For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ .
--
-- * 'cdirrKMSKeyId' - The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance. If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region. You can't create an encrypted read replica from an unencrypted DB instance.
--
-- * 'cdirrDBParameterGroupName' - The name of the DB parameter group to associate with this DB instance. If you do not specify a value for @DBParameterGroupName@ , then Amazon RDS uses the @DBParameterGroup@ of source DB instance for a same region read replica, or the default @DBParameterGroup@ for the specified DB engine for a cross region read replica. Constraints:     * Must be 1 to 255 letters, numbers, or hyphens.     * First character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens
--
-- * 'cdirrAvailabilityZone' - The Availability Zone (AZ) where the read replica will be created. Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@
--
-- * 'cdirrPerformanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'cdirrVPCSecurityGroupIds' - A list of EC2 VPC security groups to associate with the read replica.  Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- * 'cdirrMultiAZ' - A value that indicates whether the read replica is in a Multi-AZ deployment.  You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
--
-- * 'cdirrOptionGroupName' - The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
--
-- * 'cdirrCopyTagsToSnapshot' - A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
--
-- * 'cdirrDomainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- * 'cdirrTags' - Undocumented member.
--
-- * 'cdirrPort' - The port number that the DB instance uses for connections. Default: Inherits from the source DB instance Valid Values: @1150-65535@
--
-- * 'cdirrEnableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- * 'cdirrUseDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- * 'cdirrStorageType' - Specifies the storage type to be associated with the read replica. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- * 'cdirrEnableCloudwatchLogsExports' - The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
--
-- * 'cdirrDBInstanceIdentifier' - The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
--
-- * 'cdirrSourceDBInstanceIdentifier' - The identifier of the DB instance that will act as the source for the read replica. Each DB instance can have up to five read replicas. Constraints:     * Must be the identifier of an existing MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server DB instance.     * Can specify a DB instance that is a MySQL read replica only if the source is running MySQL 5.6 or later.     * For the limitations of Oracle read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle> in the /Amazon RDS User Guide/ .     * For the limitations of SQL Server read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server> in the /Amazon RDS User Guide/ .     * Can specify a PostgreSQL DB instance only if the source is running PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region replication).     * The specified DB instance must have automatic backups enabled, that is, its backup retention period must be greater than 0.     * If the source DB instance is in the same AWS Region as the read replica, specify a valid DB instance identifier.     * If the source DB instance is in a different AWS Region from the read replica, specify a valid DB instance ARN. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ . This doesn't apply to SQL Server, which doesn't support cross-region replicas.
createDBInstanceReadReplica ::
  -- | 'cdirrDBInstanceIdentifier'
  Text ->
  -- | 'cdirrSourceDBInstanceIdentifier'
  Text ->
  CreateDBInstanceReadReplica
createDBInstanceReadReplica
  pDBInstanceIdentifier_
  pSourceDBInstanceIdentifier_ =
    CreateDBInstanceReadReplica'
      { _cdirrDeletionProtection = Nothing,
        _cdirrPubliclyAccessible = Nothing,
        _cdirrAutoMinorVersionUpgrade = Nothing,
        _cdirrDBSubnetGroupName = Nothing,
        _cdirrMonitoringRoleARN = Nothing,
        _cdirrIOPS = Nothing,
        _cdirrDomain = Nothing,
        _cdirrReplicaMode = Nothing,
        _cdirrMonitoringInterval = Nothing,
        _cdirrPreSignedURL = Nothing,
        _cdirrProcessorFeatures = Nothing,
        _cdirrDBInstanceClass = Nothing,
        _cdirrPerformanceInsightsRetentionPeriod = Nothing,
        _cdirrMaxAllocatedStorage = Nothing,
        _cdirrEnablePerformanceInsights = Nothing,
        _cdirrKMSKeyId = Nothing,
        _cdirrDBParameterGroupName = Nothing,
        _cdirrAvailabilityZone = Nothing,
        _cdirrPerformanceInsightsKMSKeyId = Nothing,
        _cdirrVPCSecurityGroupIds = Nothing,
        _cdirrMultiAZ = Nothing,
        _cdirrOptionGroupName = Nothing,
        _cdirrCopyTagsToSnapshot = Nothing,
        _cdirrDomainIAMRoleName = Nothing,
        _cdirrTags = Nothing,
        _cdirrPort = Nothing,
        _cdirrEnableIAMDatabaseAuthentication = Nothing,
        _cdirrUseDefaultProcessorFeatures = Nothing,
        _cdirrStorageType = Nothing,
        _cdirrEnableCloudwatchLogsExports = Nothing,
        _cdirrDBInstanceIdentifier = pDBInstanceIdentifier_,
        _cdirrSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier_
      }

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
cdirrDeletionProtection :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrDeletionProtection = lens _cdirrDeletionProtection (\s a -> s {_cdirrDeletionProtection = a})

-- | A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it. When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address. For more information, see 'CreateDBInstance' .
cdirrPubliclyAccessible :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrPubliclyAccessible = lens _cdirrPubliclyAccessible (\s a -> s {_cdirrPubliclyAccessible = a})

-- | A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window. Default: Inherits from the source DB instance
cdirrAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrAutoMinorVersionUpgrade = lens _cdirrAutoMinorVersionUpgrade (\s a -> s {_cdirrAutoMinorVersionUpgrade = a})

-- | Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance isn't created in a VPC. Constraints:     * Can only be specified if the source DB instance identifier specifies a DB instance in another AWS Region.     * If supplied, must match the name of an existing DBSubnetGroup.     * The specified DB subnet group must be in the same AWS Region in which the operation is running.     * All read replicas in one AWS Region that are created from the same source DB instance must either:>     * Specify DB subnet groups from the same VPC. All these read replicas are created in the same VPC.     * Not specify a DB subnet group. All these read replicas are created outside of any VPC. Example: @mySubnetgroup@
cdirrDBSubnetGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBSubnetGroupName = lens _cdirrDBSubnetGroupName (\s a -> s {_cdirrDBSubnetGroupName = a})

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
cdirrMonitoringRoleARN :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrMonitoringRoleARN = lens _cdirrMonitoringRoleARN (\s a -> s {_cdirrMonitoringRoleARN = a})

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
cdirrIOPS :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrIOPS = lens _cdirrIOPS (\s a -> s {_cdirrIOPS = a})

-- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
cdirrDomain :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDomain = lens _cdirrDomain (\s a -> s {_cdirrDomain = a})

-- | The open mode of the replica database: mounted or read-only. Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload. You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
cdirrReplicaMode :: Lens' CreateDBInstanceReadReplica (Maybe ReplicaMode)
cdirrReplicaMode = lens _cdirrReplicaMode (\s a -> s {_cdirrReplicaMode = a})

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
cdirrMonitoringInterval :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrMonitoringInterval = lens _cdirrMonitoringInterval (\s a -> s {_cdirrMonitoringInterval = a})

-- | The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance.  You must specify this parameter when you create an encrypted read replica from another AWS Region by using the Amazon RDS API. Don't specify @PreSignedUrl@ when you are creating an encrypted read replica in the same AWS Region. The presigned URL must be a valid request for the @CreateDBInstanceReadReplica@ API action that can be executed in the source AWS Region that contains the encrypted source DB instance. The presigned URL request must contain the following parameter values:      * @DestinationRegion@ - The AWS Region that the encrypted read replica is created in. This AWS Region is the same one where the @CreateDBInstanceReadReplica@ action is called that contains this presigned URL. For example, if you create an encrypted DB instance in the us-west-1 AWS Region, from a source DB instance in the us-east-2 AWS Region, then you call the @CreateDBInstanceReadReplica@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CreateDBInstanceReadReplica@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.      * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the read replica in the destination AWS Region. This is the same identifier for both the @CreateDBInstanceReadReplica@ action that is called in the destination AWS Region, and the action contained in the presigned URL.      * @SourceDBInstanceIdentifier@ - The DB instance identifier for the encrypted DB instance to be replicated. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are creating an encrypted read replica from a DB instance in the us-west-2 AWS Region, then your @SourceDBInstanceIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@ .  To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
cdirrPreSignedURL :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrPreSignedURL = lens _cdirrPreSignedURL (\s a -> s {_cdirrPreSignedURL = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
cdirrProcessorFeatures :: Lens' CreateDBInstanceReadReplica [ProcessorFeature]
cdirrProcessorFeatures = lens _cdirrProcessorFeatures (\s a -> s {_cdirrProcessorFeatures = a}) . _Default . _Coerce

-- | The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  Default: Inherits from the source DB instance.
cdirrDBInstanceClass :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBInstanceClass = lens _cdirrDBInstanceClass (\s a -> s {_cdirrDBInstanceClass = a})

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
cdirrPerformanceInsightsRetentionPeriod :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrPerformanceInsightsRetentionPeriod = lens _cdirrPerformanceInsightsRetentionPeriod (\s a -> s {_cdirrPerformanceInsightsRetentionPeriod = a})

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
cdirrMaxAllocatedStorage :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrMaxAllocatedStorage = lens _cdirrMaxAllocatedStorage (\s a -> s {_cdirrMaxAllocatedStorage = a})

-- | A value that indicates whether to enable Performance Insights for the read replica.  For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ .
cdirrEnablePerformanceInsights :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrEnablePerformanceInsights = lens _cdirrEnablePerformanceInsights (\s a -> s {_cdirrEnablePerformanceInsights = a})

-- | The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance. If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region. You can't create an encrypted read replica from an unencrypted DB instance.
cdirrKMSKeyId :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrKMSKeyId = lens _cdirrKMSKeyId (\s a -> s {_cdirrKMSKeyId = a})

-- | The name of the DB parameter group to associate with this DB instance. If you do not specify a value for @DBParameterGroupName@ , then Amazon RDS uses the @DBParameterGroup@ of source DB instance for a same region read replica, or the default @DBParameterGroup@ for the specified DB engine for a cross region read replica. Constraints:     * Must be 1 to 255 letters, numbers, or hyphens.     * First character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens
cdirrDBParameterGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBParameterGroupName = lens _cdirrDBParameterGroupName (\s a -> s {_cdirrDBParameterGroupName = a})

-- | The Availability Zone (AZ) where the read replica will be created. Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@
cdirrAvailabilityZone :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrAvailabilityZone = lens _cdirrAvailabilityZone (\s a -> s {_cdirrAvailabilityZone = a})

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
cdirrPerformanceInsightsKMSKeyId :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrPerformanceInsightsKMSKeyId = lens _cdirrPerformanceInsightsKMSKeyId (\s a -> s {_cdirrPerformanceInsightsKMSKeyId = a})

-- | A list of EC2 VPC security groups to associate with the read replica.  Default: The default EC2 VPC security group for the DB subnet group's VPC.
cdirrVPCSecurityGroupIds :: Lens' CreateDBInstanceReadReplica [Text]
cdirrVPCSecurityGroupIds = lens _cdirrVPCSecurityGroupIds (\s a -> s {_cdirrVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | A value that indicates whether the read replica is in a Multi-AZ deployment.  You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
cdirrMultiAZ :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrMultiAZ = lens _cdirrMultiAZ (\s a -> s {_cdirrMultiAZ = a})

-- | The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
cdirrOptionGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrOptionGroupName = lens _cdirrOptionGroupName (\s a -> s {_cdirrOptionGroupName = a})

-- | A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
cdirrCopyTagsToSnapshot :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrCopyTagsToSnapshot = lens _cdirrCopyTagsToSnapshot (\s a -> s {_cdirrCopyTagsToSnapshot = a})

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
cdirrDomainIAMRoleName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDomainIAMRoleName = lens _cdirrDomainIAMRoleName (\s a -> s {_cdirrDomainIAMRoleName = a})

-- | Undocumented member.
cdirrTags :: Lens' CreateDBInstanceReadReplica [Tag]
cdirrTags = lens _cdirrTags (\s a -> s {_cdirrTags = a}) . _Default . _Coerce

-- | The port number that the DB instance uses for connections. Default: Inherits from the source DB instance Valid Values: @1150-65535@
cdirrPort :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrPort = lens _cdirrPort (\s a -> s {_cdirrPort = a})

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
cdirrEnableIAMDatabaseAuthentication :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrEnableIAMDatabaseAuthentication = lens _cdirrEnableIAMDatabaseAuthentication (\s a -> s {_cdirrEnableIAMDatabaseAuthentication = a})

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
cdirrUseDefaultProcessorFeatures :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrUseDefaultProcessorFeatures = lens _cdirrUseDefaultProcessorFeatures (\s a -> s {_cdirrUseDefaultProcessorFeatures = a})

-- | Specifies the storage type to be associated with the read replica. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
cdirrStorageType :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrStorageType = lens _cdirrStorageType (\s a -> s {_cdirrStorageType = a})

-- | The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
cdirrEnableCloudwatchLogsExports :: Lens' CreateDBInstanceReadReplica [Text]
cdirrEnableCloudwatchLogsExports = lens _cdirrEnableCloudwatchLogsExports (\s a -> s {_cdirrEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
cdirrDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrDBInstanceIdentifier = lens _cdirrDBInstanceIdentifier (\s a -> s {_cdirrDBInstanceIdentifier = a})

-- | The identifier of the DB instance that will act as the source for the read replica. Each DB instance can have up to five read replicas. Constraints:     * Must be the identifier of an existing MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server DB instance.     * Can specify a DB instance that is a MySQL read replica only if the source is running MySQL 5.6 or later.     * For the limitations of Oracle read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle> in the /Amazon RDS User Guide/ .     * For the limitations of SQL Server read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server> in the /Amazon RDS User Guide/ .     * Can specify a PostgreSQL DB instance only if the source is running PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region replication).     * The specified DB instance must have automatic backups enabled, that is, its backup retention period must be greater than 0.     * If the source DB instance is in the same AWS Region as the read replica, specify a valid DB instance identifier.     * If the source DB instance is in a different AWS Region from the read replica, specify a valid DB instance ARN. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ . This doesn't apply to SQL Server, which doesn't support cross-region replicas.
cdirrSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrSourceDBInstanceIdentifier = lens _cdirrSourceDBInstanceIdentifier (\s a -> s {_cdirrSourceDBInstanceIdentifier = a})

instance AWSRequest CreateDBInstanceReadReplica where
  type
    Rs CreateDBInstanceReadReplica =
      CreateDBInstanceReadReplicaResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "CreateDBInstanceReadReplicaResult"
      ( \s h x ->
          CreateDBInstanceReadReplicaResponse'
            <$> (x .@? "DBInstance") <*> (pure (fromEnum s))
      )

instance Hashable CreateDBInstanceReadReplica

instance NFData CreateDBInstanceReadReplica

instance ToHeaders CreateDBInstanceReadReplica where
  toHeaders = const mempty

instance ToPath CreateDBInstanceReadReplica where
  toPath = const "/"

instance ToQuery CreateDBInstanceReadReplica where
  toQuery CreateDBInstanceReadReplica' {..} =
    mconcat
      [ "Action" =: ("CreateDBInstanceReadReplica" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DeletionProtection" =: _cdirrDeletionProtection,
        "PubliclyAccessible" =: _cdirrPubliclyAccessible,
        "AutoMinorVersionUpgrade" =: _cdirrAutoMinorVersionUpgrade,
        "DBSubnetGroupName" =: _cdirrDBSubnetGroupName,
        "MonitoringRoleArn" =: _cdirrMonitoringRoleARN,
        "Iops" =: _cdirrIOPS,
        "Domain" =: _cdirrDomain,
        "ReplicaMode" =: _cdirrReplicaMode,
        "MonitoringInterval" =: _cdirrMonitoringInterval,
        "PreSignedUrl" =: _cdirrPreSignedURL,
        "ProcessorFeatures"
          =: toQuery
            (toQueryList "ProcessorFeature" <$> _cdirrProcessorFeatures),
        "DBInstanceClass" =: _cdirrDBInstanceClass,
        "PerformanceInsightsRetentionPeriod"
          =: _cdirrPerformanceInsightsRetentionPeriod,
        "MaxAllocatedStorage" =: _cdirrMaxAllocatedStorage,
        "EnablePerformanceInsights" =: _cdirrEnablePerformanceInsights,
        "KmsKeyId" =: _cdirrKMSKeyId,
        "DBParameterGroupName" =: _cdirrDBParameterGroupName,
        "AvailabilityZone" =: _cdirrAvailabilityZone,
        "PerformanceInsightsKMSKeyId" =: _cdirrPerformanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          =: toQuery
            (toQueryList "VpcSecurityGroupId" <$> _cdirrVPCSecurityGroupIds),
        "MultiAZ" =: _cdirrMultiAZ,
        "OptionGroupName" =: _cdirrOptionGroupName,
        "CopyTagsToSnapshot" =: _cdirrCopyTagsToSnapshot,
        "DomainIAMRoleName" =: _cdirrDomainIAMRoleName,
        "Tags" =: toQuery (toQueryList "Tag" <$> _cdirrTags),
        "Port" =: _cdirrPort,
        "EnableIAMDatabaseAuthentication"
          =: _cdirrEnableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures" =: _cdirrUseDefaultProcessorFeatures,
        "StorageType" =: _cdirrStorageType,
        "EnableCloudwatchLogsExports"
          =: toQuery
            (toQueryList "member" <$> _cdirrEnableCloudwatchLogsExports),
        "DBInstanceIdentifier" =: _cdirrDBInstanceIdentifier,
        "SourceDBInstanceIdentifier" =: _cdirrSourceDBInstanceIdentifier
      ]

-- | /See:/ 'createDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
  { _cdirrrsDBInstance ::
      !(Maybe DBInstance),
    _cdirrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDBInstanceReadReplicaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirrrsDBInstance' - Undocumented member.
--
-- * 'cdirrrsResponseStatus' - -- | The response status code.
createDBInstanceReadReplicaResponse ::
  -- | 'cdirrrsResponseStatus'
  Int ->
  CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplicaResponse pResponseStatus_ =
  CreateDBInstanceReadReplicaResponse'
    { _cdirrrsDBInstance =
        Nothing,
      _cdirrrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cdirrrsDBInstance :: Lens' CreateDBInstanceReadReplicaResponse (Maybe DBInstance)
cdirrrsDBInstance = lens _cdirrrsDBInstance (\s a -> s {_cdirrrsDBInstance = a})

-- | -- | The response status code.
cdirrrsResponseStatus :: Lens' CreateDBInstanceReadReplicaResponse Int
cdirrrsResponseStatus = lens _cdirrrsResponseStatus (\s a -> s {_cdirrrsResponseStatus = a})

instance NFData CreateDBInstanceReadReplicaResponse
