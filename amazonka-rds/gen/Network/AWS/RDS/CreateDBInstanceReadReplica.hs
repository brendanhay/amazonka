{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance that acts as a Read Replica for an existing source DB instance. You can create a Read Replica for a DB instance running MySQL, MariaDB, or PostgreSQL. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReadRepl.html Working with PostgreSQL, MySQL, and MariaDB Read Replicas> .
--
--
-- Amazon Aurora doesn't support this action. You must call the @CreateDBInstance@ action to create a DB instance for an Aurora DB cluster.
--
-- All Read Replica DB instances are created with backups disabled. All other DB instance attributes (including DB security groups and DB parameter groups) are inherited from the source DB instance, except as specified following.
--
-- /Important:/ Your source DB instance must have backup retention enabled.
--
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Creating a Request
      createDBInstanceReadReplica
    , CreateDBInstanceReadReplica
    -- * Request Lenses
    , cdirrPubliclyAccessible
    , cdirrAutoMinorVersionUpgrade
    , cdirrDBSubnetGroupName
    , cdirrMonitoringRoleARN
    , cdirrIOPS
    , cdirrMonitoringInterval
    , cdirrPreSignedURL
    , cdirrDBInstanceClass
    , cdirrEnablePerformanceInsights
    , cdirrKMSKeyId
    , cdirrAvailabilityZone
    , cdirrPerformanceInsightsKMSKeyId
    , cdirrMultiAZ
    , cdirrOptionGroupName
    , cdirrCopyTagsToSnapshot
    , cdirrTags
    , cdirrPort
    , cdirrEnableIAMDatabaseAuthentication
    , cdirrStorageType
    , cdirrEnableCloudwatchLogsExports
    , cdirrDBInstanceIdentifier
    , cdirrSourceDBInstanceIdentifier

    -- * Destructuring the Response
    , createDBInstanceReadReplicaResponse
    , CreateDBInstanceReadReplicaResponse
    -- * Response Lenses
    , cdirrrsDBInstance
    , cdirrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { _cdirrPubliclyAccessible              :: !(Maybe Bool)
  , _cdirrAutoMinorVersionUpgrade         :: !(Maybe Bool)
  , _cdirrDBSubnetGroupName               :: !(Maybe Text)
  , _cdirrMonitoringRoleARN               :: !(Maybe Text)
  , _cdirrIOPS                            :: !(Maybe Int)
  , _cdirrMonitoringInterval              :: !(Maybe Int)
  , _cdirrPreSignedURL                    :: !(Maybe Text)
  , _cdirrDBInstanceClass                 :: !(Maybe Text)
  , _cdirrEnablePerformanceInsights       :: !(Maybe Bool)
  , _cdirrKMSKeyId                        :: !(Maybe Text)
  , _cdirrAvailabilityZone                :: !(Maybe Text)
  , _cdirrPerformanceInsightsKMSKeyId     :: !(Maybe Text)
  , _cdirrMultiAZ                         :: !(Maybe Bool)
  , _cdirrOptionGroupName                 :: !(Maybe Text)
  , _cdirrCopyTagsToSnapshot              :: !(Maybe Bool)
  , _cdirrTags                            :: !(Maybe [Tag])
  , _cdirrPort                            :: !(Maybe Int)
  , _cdirrEnableIAMDatabaseAuthentication :: !(Maybe Bool)
  , _cdirrStorageType                     :: !(Maybe Text)
  , _cdirrEnableCloudwatchLogsExports     :: !(Maybe [Text])
  , _cdirrDBInstanceIdentifier            :: !Text
  , _cdirrSourceDBInstanceIdentifier      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBInstanceReadReplica' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirrPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is private.
--
-- * 'cdirrAutoMinorVersionUpgrade' - Indicates that minor engine upgrades are applied automatically to the Read Replica during the maintenance window. Default: Inherits from the source DB instance
--
-- * 'cdirrDBSubnetGroupName' - Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance is not created in a VPC. Constraints:     * Can only be specified if the source DB instance identifier specifies a DB instance in another AWS Region.     * If supplied, must match the name of an existing DBSubnetGroup.     * The specified DB subnet group must be in the same AWS Region in which the operation is running.     * All Read Replicas in one AWS Region that are created from the same source DB instance must either:>     * Specify DB subnet groups from the same VPC. All these Read Replicas are created in the same VPC.     * Not specify a DB subnet group. All these Read Replicas are created outside of any VPC. Example: @mySubnetgroup@
--
-- * 'cdirrMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- * 'cdirrIOPS' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- * 'cdirrMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the Read Replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- * 'cdirrPreSignedURL' - The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance.  You must specify this parameter when you create an encrypted Read Replica from another AWS Region by using the Amazon RDS API. You can specify the @--source-region@ option instead of this parameter when you create an encrypted Read Replica from another AWS Region by using the AWS CLI.  The presigned URL must be a valid request for the @CreateDBInstanceReadReplica@ API action that can be executed in the source AWS Region that contains the encrypted source DB instance. The presigned URL request must contain the following parameter values:      * @DestinationRegion@ - The AWS Region that the encrypted Read Replica is created in. This AWS Region is the same one where the @CreateDBInstanceReadReplica@ action is called that contains this presigned URL.  For example, if you create an encrypted DB instance in the us-west-1 AWS Region, from a source DB instance in the us-east-2 AWS Region, then you call the @CreateDBInstanceReadReplica@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CreateDBInstanceReadReplica@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.      * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the Read Replica in the destination AWS Region. This is the same identifier for both the @CreateDBInstanceReadReplica@ action that is called in the destination AWS Region, and the action contained in the presigned URL.      * @SourceDBInstanceIdentifier@ - The DB instance identifier for the encrypted DB instance to be replicated. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are creating an encrypted Read Replica from a DB instance in the us-west-2 AWS Region, then your @SourceDBInstanceIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@ .  To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- * 'cdirrDBInstanceClass' - The compute and memory capacity of the Read Replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the Amazon RDS User Guide.  Default: Inherits from the source DB instance.
--
-- * 'cdirrEnablePerformanceInsights' - True to enable Performance Insights for the read replica, and otherwise false.
--
-- * 'cdirrKMSKeyId' - The AWS KMS key ID for an encrypted Read Replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you specify this parameter when you create a Read Replica from an unencrypted DB instance, the Read Replica is encrypted.  If you create an encrypted Read Replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The Read Replica is encrypted with the same KMS key as the source DB instance.  If you create an encrypted Read Replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
--
-- * 'cdirrAvailabilityZone' - The Amazon EC2 Availability Zone that the Read Replica is created in. Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@
--
-- * 'cdirrPerformanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- * 'cdirrMultiAZ' - Specifies whether the Read Replica is in a Multi-AZ deployment.  You can create a Read Replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your Read Replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
--
-- * 'cdirrOptionGroupName' - The option group the DB instance is associated with. If omitted, the default option group for the engine specified is used.
--
-- * 'cdirrCopyTagsToSnapshot' - True to copy all tags from the Read Replica to snapshots of the Read Replica, and otherwise false. The default is false.
--
-- * 'cdirrTags' - Undocumented member.
--
-- * 'cdirrPort' - The port number that the DB instance uses for connections. Default: Inherits from the source DB instance Valid Values: @1150-65535@
--
-- * 'cdirrEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. You can enable IAM database authentication for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. Default: @false@
--
-- * 'cdirrStorageType' - Specifies the storage type to be associated with the Read Replica. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @standard@
--
-- * 'cdirrEnableCloudwatchLogsExports' - The list of logs that the new DB instance is to export to CloudWatch Logs.
--
-- * 'cdirrDBInstanceIdentifier' - The DB instance identifier of the Read Replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
--
-- * 'cdirrSourceDBInstanceIdentifier' - The identifier of the DB instance that will act as the source for the Read Replica. Each DB instance can have up to five Read Replicas. Constraints:     * Must be the identifier of an existing MySQL, MariaDB, or PostgreSQL DB instance.     * Can specify a DB instance that is a MySQL Read Replica only if the source is running MySQL 5.6.     * Can specify a DB instance that is a PostgreSQL DB instance only if the source is running PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region replication).     * The specified DB instance must have automatic backups enabled, its backup retention period must be greater than 0.     * If the source DB instance is in the same AWS Region as the Read Replica, specify a valid DB instance identifier.     * If the source DB instance is in a different AWS Region than the Read Replica, specify a valid DB instance ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing a Amazon RDS Amazon Resource Name (ARN)> .
createDBInstanceReadReplica
    :: Text -- ^ 'cdirrDBInstanceIdentifier'
    -> Text -- ^ 'cdirrSourceDBInstanceIdentifier'
    -> CreateDBInstanceReadReplica
createDBInstanceReadReplica pDBInstanceIdentifier_ pSourceDBInstanceIdentifier_ =
  CreateDBInstanceReadReplica'
    { _cdirrPubliclyAccessible = Nothing
    , _cdirrAutoMinorVersionUpgrade = Nothing
    , _cdirrDBSubnetGroupName = Nothing
    , _cdirrMonitoringRoleARN = Nothing
    , _cdirrIOPS = Nothing
    , _cdirrMonitoringInterval = Nothing
    , _cdirrPreSignedURL = Nothing
    , _cdirrDBInstanceClass = Nothing
    , _cdirrEnablePerformanceInsights = Nothing
    , _cdirrKMSKeyId = Nothing
    , _cdirrAvailabilityZone = Nothing
    , _cdirrPerformanceInsightsKMSKeyId = Nothing
    , _cdirrMultiAZ = Nothing
    , _cdirrOptionGroupName = Nothing
    , _cdirrCopyTagsToSnapshot = Nothing
    , _cdirrTags = Nothing
    , _cdirrPort = Nothing
    , _cdirrEnableIAMDatabaseAuthentication = Nothing
    , _cdirrStorageType = Nothing
    , _cdirrEnableCloudwatchLogsExports = Nothing
    , _cdirrDBInstanceIdentifier = pDBInstanceIdentifier_
    , _cdirrSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier_
    }


-- | Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is private.
cdirrPubliclyAccessible :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrPubliclyAccessible = lens _cdirrPubliclyAccessible (\ s a -> s{_cdirrPubliclyAccessible = a})

-- | Indicates that minor engine upgrades are applied automatically to the Read Replica during the maintenance window. Default: Inherits from the source DB instance
cdirrAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrAutoMinorVersionUpgrade = lens _cdirrAutoMinorVersionUpgrade (\ s a -> s{_cdirrAutoMinorVersionUpgrade = a})

-- | Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance is not created in a VPC. Constraints:     * Can only be specified if the source DB instance identifier specifies a DB instance in another AWS Region.     * If supplied, must match the name of an existing DBSubnetGroup.     * The specified DB subnet group must be in the same AWS Region in which the operation is running.     * All Read Replicas in one AWS Region that are created from the same source DB instance must either:>     * Specify DB subnet groups from the same VPC. All these Read Replicas are created in the same VPC.     * Not specify a DB subnet group. All these Read Replicas are created outside of any VPC. Example: @mySubnetgroup@
cdirrDBSubnetGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBSubnetGroupName = lens _cdirrDBSubnetGroupName (\ s a -> s{_cdirrDBSubnetGroupName = a})

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
cdirrMonitoringRoleARN :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrMonitoringRoleARN = lens _cdirrMonitoringRoleARN (\ s a -> s{_cdirrMonitoringRoleARN = a})

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
cdirrIOPS :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrIOPS = lens _cdirrIOPS (\ s a -> s{_cdirrIOPS = a})

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the Read Replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
cdirrMonitoringInterval :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrMonitoringInterval = lens _cdirrMonitoringInterval (\ s a -> s{_cdirrMonitoringInterval = a})

-- | The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance.  You must specify this parameter when you create an encrypted Read Replica from another AWS Region by using the Amazon RDS API. You can specify the @--source-region@ option instead of this parameter when you create an encrypted Read Replica from another AWS Region by using the AWS CLI.  The presigned URL must be a valid request for the @CreateDBInstanceReadReplica@ API action that can be executed in the source AWS Region that contains the encrypted source DB instance. The presigned URL request must contain the following parameter values:      * @DestinationRegion@ - The AWS Region that the encrypted Read Replica is created in. This AWS Region is the same one where the @CreateDBInstanceReadReplica@ action is called that contains this presigned URL.  For example, if you create an encrypted DB instance in the us-west-1 AWS Region, from a source DB instance in the us-east-2 AWS Region, then you call the @CreateDBInstanceReadReplica@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CreateDBInstanceReadReplica@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.      * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the Read Replica in the destination AWS Region. This is the same identifier for both the @CreateDBInstanceReadReplica@ action that is called in the destination AWS Region, and the action contained in the presigned URL.      * @SourceDBInstanceIdentifier@ - The DB instance identifier for the encrypted DB instance to be replicated. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are creating an encrypted Read Replica from a DB instance in the us-west-2 AWS Region, then your @SourceDBInstanceIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@ .  To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
cdirrPreSignedURL :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrPreSignedURL = lens _cdirrPreSignedURL (\ s a -> s{_cdirrPreSignedURL = a})

-- | The compute and memory capacity of the Read Replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the Amazon RDS User Guide.  Default: Inherits from the source DB instance.
cdirrDBInstanceClass :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBInstanceClass = lens _cdirrDBInstanceClass (\ s a -> s{_cdirrDBInstanceClass = a})

-- | True to enable Performance Insights for the read replica, and otherwise false.
cdirrEnablePerformanceInsights :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrEnablePerformanceInsights = lens _cdirrEnablePerformanceInsights (\ s a -> s{_cdirrEnablePerformanceInsights = a})

-- | The AWS KMS key ID for an encrypted Read Replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you specify this parameter when you create a Read Replica from an unencrypted DB instance, the Read Replica is encrypted.  If you create an encrypted Read Replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The Read Replica is encrypted with the same KMS key as the source DB instance.  If you create an encrypted Read Replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
cdirrKMSKeyId :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrKMSKeyId = lens _cdirrKMSKeyId (\ s a -> s{_cdirrKMSKeyId = a})

-- | The Amazon EC2 Availability Zone that the Read Replica is created in. Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@
cdirrAvailabilityZone :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrAvailabilityZone = lens _cdirrAvailabilityZone (\ s a -> s{_cdirrAvailabilityZone = a})

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
cdirrPerformanceInsightsKMSKeyId :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrPerformanceInsightsKMSKeyId = lens _cdirrPerformanceInsightsKMSKeyId (\ s a -> s{_cdirrPerformanceInsightsKMSKeyId = a})

-- | Specifies whether the Read Replica is in a Multi-AZ deployment.  You can create a Read Replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your Read Replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
cdirrMultiAZ :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrMultiAZ = lens _cdirrMultiAZ (\ s a -> s{_cdirrMultiAZ = a})

-- | The option group the DB instance is associated with. If omitted, the default option group for the engine specified is used.
cdirrOptionGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrOptionGroupName = lens _cdirrOptionGroupName (\ s a -> s{_cdirrOptionGroupName = a})

-- | True to copy all tags from the Read Replica to snapshots of the Read Replica, and otherwise false. The default is false.
cdirrCopyTagsToSnapshot :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrCopyTagsToSnapshot = lens _cdirrCopyTagsToSnapshot (\ s a -> s{_cdirrCopyTagsToSnapshot = a})

-- | Undocumented member.
cdirrTags :: Lens' CreateDBInstanceReadReplica [Tag]
cdirrTags = lens _cdirrTags (\ s a -> s{_cdirrTags = a}) . _Default . _Coerce

-- | The port number that the DB instance uses for connections. Default: Inherits from the source DB instance Valid Values: @1150-65535@
cdirrPort :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrPort = lens _cdirrPort (\ s a -> s{_cdirrPort = a})

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. You can enable IAM database authentication for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. Default: @false@
cdirrEnableIAMDatabaseAuthentication :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrEnableIAMDatabaseAuthentication = lens _cdirrEnableIAMDatabaseAuthentication (\ s a -> s{_cdirrEnableIAMDatabaseAuthentication = a})

-- | Specifies the storage type to be associated with the Read Replica. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @standard@
cdirrStorageType :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrStorageType = lens _cdirrStorageType (\ s a -> s{_cdirrStorageType = a})

-- | The list of logs that the new DB instance is to export to CloudWatch Logs.
cdirrEnableCloudwatchLogsExports :: Lens' CreateDBInstanceReadReplica [Text]
cdirrEnableCloudwatchLogsExports = lens _cdirrEnableCloudwatchLogsExports (\ s a -> s{_cdirrEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The DB instance identifier of the Read Replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
cdirrDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrDBInstanceIdentifier = lens _cdirrDBInstanceIdentifier (\ s a -> s{_cdirrDBInstanceIdentifier = a})

-- | The identifier of the DB instance that will act as the source for the Read Replica. Each DB instance can have up to five Read Replicas. Constraints:     * Must be the identifier of an existing MySQL, MariaDB, or PostgreSQL DB instance.     * Can specify a DB instance that is a MySQL Read Replica only if the source is running MySQL 5.6.     * Can specify a DB instance that is a PostgreSQL DB instance only if the source is running PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region replication).     * The specified DB instance must have automatic backups enabled, its backup retention period must be greater than 0.     * If the source DB instance is in the same AWS Region as the Read Replica, specify a valid DB instance identifier.     * If the source DB instance is in a different AWS Region than the Read Replica, specify a valid DB instance ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing a Amazon RDS Amazon Resource Name (ARN)> .
cdirrSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrSourceDBInstanceIdentifier = lens _cdirrSourceDBInstanceIdentifier (\ s a -> s{_cdirrSourceDBInstanceIdentifier = a})

instance AWSRequest CreateDBInstanceReadReplica where
        type Rs CreateDBInstanceReadReplica =
             CreateDBInstanceReadReplicaResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "CreateDBInstanceReadReplicaResult"
              (\ s h x ->
                 CreateDBInstanceReadReplicaResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable CreateDBInstanceReadReplica where

instance NFData CreateDBInstanceReadReplica where

instance ToHeaders CreateDBInstanceReadReplica where
        toHeaders = const mempty

instance ToPath CreateDBInstanceReadReplica where
        toPath = const "/"

instance ToQuery CreateDBInstanceReadReplica where
        toQuery CreateDBInstanceReadReplica'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBInstanceReadReplica" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PubliclyAccessible" =: _cdirrPubliclyAccessible,
               "AutoMinorVersionUpgrade" =:
                 _cdirrAutoMinorVersionUpgrade,
               "DBSubnetGroupName" =: _cdirrDBSubnetGroupName,
               "MonitoringRoleArn" =: _cdirrMonitoringRoleARN,
               "Iops" =: _cdirrIOPS,
               "MonitoringInterval" =: _cdirrMonitoringInterval,
               "PreSignedUrl" =: _cdirrPreSignedURL,
               "DBInstanceClass" =: _cdirrDBInstanceClass,
               "EnablePerformanceInsights" =:
                 _cdirrEnablePerformanceInsights,
               "KmsKeyId" =: _cdirrKMSKeyId,
               "AvailabilityZone" =: _cdirrAvailabilityZone,
               "PerformanceInsightsKMSKeyId" =:
                 _cdirrPerformanceInsightsKMSKeyId,
               "MultiAZ" =: _cdirrMultiAZ,
               "OptionGroupName" =: _cdirrOptionGroupName,
               "CopyTagsToSnapshot" =: _cdirrCopyTagsToSnapshot,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdirrTags),
               "Port" =: _cdirrPort,
               "EnableIAMDatabaseAuthentication" =:
                 _cdirrEnableIAMDatabaseAuthentication,
               "StorageType" =: _cdirrStorageType,
               "EnableCloudwatchLogsExports" =:
                 toQuery
                   (toQueryList "member" <$>
                      _cdirrEnableCloudwatchLogsExports),
               "DBInstanceIdentifier" =: _cdirrDBInstanceIdentifier,
               "SourceDBInstanceIdentifier" =:
                 _cdirrSourceDBInstanceIdentifier]

-- | /See:/ 'createDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
  { _cdirrrsDBInstance     :: !(Maybe DBInstance)
  , _cdirrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBInstanceReadReplicaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirrrsDBInstance' - Undocumented member.
--
-- * 'cdirrrsResponseStatus' - -- | The response status code.
createDBInstanceReadReplicaResponse
    :: Int -- ^ 'cdirrrsResponseStatus'
    -> CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplicaResponse pResponseStatus_ =
  CreateDBInstanceReadReplicaResponse'
    {_cdirrrsDBInstance = Nothing, _cdirrrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdirrrsDBInstance :: Lens' CreateDBInstanceReadReplicaResponse (Maybe DBInstance)
cdirrrsDBInstance = lens _cdirrrsDBInstance (\ s a -> s{_cdirrrsDBInstance = a})

-- | -- | The response status code.
cdirrrsResponseStatus :: Lens' CreateDBInstanceReadReplicaResponse Int
cdirrrsResponseStatus = lens _cdirrrsResponseStatus (\ s a -> s{_cdirrrsResponseStatus = a})

instance NFData CreateDBInstanceReadReplicaResponse
         where
