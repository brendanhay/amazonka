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
-- Module      : Network.AWS.RDS.CreateDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Aurora DB cluster.
--
--
-- You can use the @ReplicationSourceIdentifier@ parameter to create the DB cluster as a Read Replica of another DB cluster or Amazon RDS MySQL DB instance. For cross-region replication where the DB cluster identified by @ReplicationSourceIdentifier@ is encrypted, you must also specify the @PreSignedUrl@ parameter.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
module Network.AWS.RDS.CreateDBCluster
    (
    -- * Creating a Request
      createDBCluster
    , CreateDBCluster
    -- * Request Lenses
    , cdcEngineVersion
    , cdcDeletionProtection
    , cdcStorageEncrypted
    , cdcMasterUserPassword
    , cdcReplicationSourceIdentifier
    , cdcGlobalClusterIdentifier
    , cdcMasterUsername
    , cdcDBSubnetGroupName
    , cdcBacktrackWindow
    , cdcPreSignedURL
    , cdcPreferredMaintenanceWindow
    , cdcAvailabilityZones
    , cdcCharacterSetName
    , cdcKMSKeyId
    , cdcPreferredBackupWindow
    , cdcBackupRetentionPeriod
    , cdcVPCSecurityGroupIds
    , cdcDatabaseName
    , cdcDBClusterParameterGroupName
    , cdcEngineMode
    , cdcScalingConfiguration
    , cdcOptionGroupName
    , cdcCopyTagsToSnapshot
    , cdcTags
    , cdcPort
    , cdcEnableIAMDatabaseAuthentication
    , cdcEnableCloudwatchLogsExports
    , cdcDBClusterIdentifier
    , cdcEngine

    -- * Destructuring the Response
    , createDBClusterResponse
    , CreateDBClusterResponse
    -- * Response Lenses
    , cdcrsDBCluster
    , cdcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { _cdcEngineVersion                   :: !(Maybe Text)
  , _cdcDeletionProtection              :: !(Maybe Bool)
  , _cdcStorageEncrypted                :: !(Maybe Bool)
  , _cdcMasterUserPassword              :: !(Maybe Text)
  , _cdcReplicationSourceIdentifier     :: !(Maybe Text)
  , _cdcGlobalClusterIdentifier         :: !(Maybe Text)
  , _cdcMasterUsername                  :: !(Maybe Text)
  , _cdcDBSubnetGroupName               :: !(Maybe Text)
  , _cdcBacktrackWindow                 :: !(Maybe Integer)
  , _cdcPreSignedURL                    :: !(Maybe Text)
  , _cdcPreferredMaintenanceWindow      :: !(Maybe Text)
  , _cdcAvailabilityZones               :: !(Maybe [Text])
  , _cdcCharacterSetName                :: !(Maybe Text)
  , _cdcKMSKeyId                        :: !(Maybe Text)
  , _cdcPreferredBackupWindow           :: !(Maybe Text)
  , _cdcBackupRetentionPeriod           :: !(Maybe Int)
  , _cdcVPCSecurityGroupIds             :: !(Maybe [Text])
  , _cdcDatabaseName                    :: !(Maybe Text)
  , _cdcDBClusterParameterGroupName     :: !(Maybe Text)
  , _cdcEngineMode                      :: !(Maybe Text)
  , _cdcScalingConfiguration            :: !(Maybe ScalingConfiguration)
  , _cdcOptionGroupName                 :: !(Maybe Text)
  , _cdcCopyTagsToSnapshot              :: !(Maybe Bool)
  , _cdcTags                            :: !(Maybe [Tag])
  , _cdcPort                            :: !(Maybe Int)
  , _cdcEnableIAMDatabaseAuthentication :: !(Maybe Bool)
  , _cdcEnableCloudwatchLogsExports     :: !(Maybe [Text])
  , _cdcDBClusterIdentifier             :: !Text
  , _cdcEngine                          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcEngineVersion' - The version number of the database engine to use. __Aurora MySQL__  Example: @5.6.10a@ , @5.7.12@  __Aurora PostgreSQL__  Example: @9.6.3@
--
-- * 'cdcDeletionProtection' - Indicates if the DB cluster should have deletion protection enabled. The database can't be deleted when this value is set to true. The default is false.
--
-- * 'cdcStorageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- * 'cdcMasterUserPassword' - The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
--
-- * 'cdcReplicationSourceIdentifier' - The Amazon Resource Name (ARN) of the source DB instance or DB cluster if this DB cluster is created as a Read Replica.
--
-- * 'cdcGlobalClusterIdentifier' - The global cluster ID of an Aurora cluster that becomes the primary cluster in the new global database cluster.
--
-- * 'cdcMasterUsername' - The name of the master user for the DB cluster. Constraints:     * Must be 1 to 16 letters or numbers.     * First character must be a letter.     * Can't be a reserved word for the chosen database engine.
--
-- * 'cdcDBSubnetGroupName' - A DB subnet group to associate with this DB cluster. Constraints: Must match the name of an existing DBSubnetGroup. Must not be default. Example: @mySubnetgroup@
--
-- * 'cdcBacktrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0.  Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
-- * 'cdcPreSignedURL' - A URL that contains a Signature Version 4 signed request for the @CreateDBCluster@ action to be called in the source AWS Region where the DB cluster is replicated from. You only need to specify @PreSignedUrl@ when you are performing cross-region replication from an encrypted DB cluster. The pre-signed URL must be a valid request for the @CreateDBCluster@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster to be copied. The pre-signed URL request must contain the following parameter values:     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster in the destination AWS Region. This should refer to the same KMS key for both the @CreateDBCluster@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.     * @DestinationRegion@ - The name of the AWS Region that Aurora Read Replica will be created in.     * @ReplicationSourceIdentifier@ - The DB cluster identifier for the encrypted DB cluster to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster from the us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would look like Example: @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@ . To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- * 'cdcPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun. Constraints: Minimum 30-minute window.
--
-- * 'cdcAvailabilityZones' - A list of EC2 Availability Zones that instances in the DB cluster can be created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones> in the /Amazon Aurora User Guide/ .
--
-- * 'cdcCharacterSetName' - A value that indicates that the DB cluster should be associated with the specified CharacterSet.
--
-- * 'cdcKMSKeyId' - The AWS KMS key identifier for an encrypted DB cluster. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key. If an encryption key is not specified in @KmsKeyId@ :     * If @ReplicationSourceIdentifier@ identifies an encrypted source, then Amazon RDS will use the encryption key used to encrypt the source. Otherwise, Amazon RDS will use your default encryption key.      * If the @StorageEncrypted@ parameter is true and @ReplicationSourceIdentifier@ is not specified, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region. If you create a Read Replica of an encrypted DB cluster in another AWS Region, you must set @KmsKeyId@ to a KMS key ID that is valid in the destination AWS Region. This key is used to encrypt the Read Replica in that AWS Region.
--
-- * 'cdcPreferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'cdcBackupRetentionPeriod' - The number of days for which automated backups are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35
--
-- * 'cdcVPCSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB cluster.
--
-- * 'cdcDatabaseName' - The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon RDS will not create a database in the DB cluster you are creating.
--
-- * 'cdcDBClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, @default.aurora5.6@ is used.  Constraints:     * If supplied, must match the name of an existing DB cluster parameter group.
--
-- * 'cdcEngineMode' - The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , or @global@ .
--
-- * 'cdcScalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
--
-- * 'cdcOptionGroupName' - A value that indicates that the DB cluster should be associated with the specified option group. Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- * 'cdcCopyTagsToSnapshot' - True to copy all tags from the DB cluster to snapshots of the DB cluster, and otherwise false. The default is false.
--
-- * 'cdcTags' - Undocumented member.
--
-- * 'cdcPort' - The port number on which the instances in the DB cluster accept connections. Default: @3306@ if engine is set as aurora or @5432@ if set to aurora-postgresql.
--
-- * 'cdcEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. Default: @false@
--
-- * 'cdcEnableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
--
-- * 'cdcDBClusterIdentifier' - The DB cluster identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@
--
-- * 'cdcEngine' - The name of the database engine to be used for this DB cluster. Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
createDBCluster
    :: Text -- ^ 'cdcDBClusterIdentifier'
    -> Text -- ^ 'cdcEngine'
    -> CreateDBCluster
createDBCluster pDBClusterIdentifier_ pEngine_ =
  CreateDBCluster'
    { _cdcEngineVersion = Nothing
    , _cdcDeletionProtection = Nothing
    , _cdcStorageEncrypted = Nothing
    , _cdcMasterUserPassword = Nothing
    , _cdcReplicationSourceIdentifier = Nothing
    , _cdcGlobalClusterIdentifier = Nothing
    , _cdcMasterUsername = Nothing
    , _cdcDBSubnetGroupName = Nothing
    , _cdcBacktrackWindow = Nothing
    , _cdcPreSignedURL = Nothing
    , _cdcPreferredMaintenanceWindow = Nothing
    , _cdcAvailabilityZones = Nothing
    , _cdcCharacterSetName = Nothing
    , _cdcKMSKeyId = Nothing
    , _cdcPreferredBackupWindow = Nothing
    , _cdcBackupRetentionPeriod = Nothing
    , _cdcVPCSecurityGroupIds = Nothing
    , _cdcDatabaseName = Nothing
    , _cdcDBClusterParameterGroupName = Nothing
    , _cdcEngineMode = Nothing
    , _cdcScalingConfiguration = Nothing
    , _cdcOptionGroupName = Nothing
    , _cdcCopyTagsToSnapshot = Nothing
    , _cdcTags = Nothing
    , _cdcPort = Nothing
    , _cdcEnableIAMDatabaseAuthentication = Nothing
    , _cdcEnableCloudwatchLogsExports = Nothing
    , _cdcDBClusterIdentifier = pDBClusterIdentifier_
    , _cdcEngine = pEngine_
    }


-- | The version number of the database engine to use. __Aurora MySQL__  Example: @5.6.10a@ , @5.7.12@  __Aurora PostgreSQL__  Example: @9.6.3@
cdcEngineVersion :: Lens' CreateDBCluster (Maybe Text)
cdcEngineVersion = lens _cdcEngineVersion (\ s a -> s{_cdcEngineVersion = a})

-- | Indicates if the DB cluster should have deletion protection enabled. The database can't be deleted when this value is set to true. The default is false.
cdcDeletionProtection :: Lens' CreateDBCluster (Maybe Bool)
cdcDeletionProtection = lens _cdcDeletionProtection (\ s a -> s{_cdcDeletionProtection = a})

-- | Specifies whether the DB cluster is encrypted.
cdcStorageEncrypted :: Lens' CreateDBCluster (Maybe Bool)
cdcStorageEncrypted = lens _cdcStorageEncrypted (\ s a -> s{_cdcStorageEncrypted = a})

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
cdcMasterUserPassword :: Lens' CreateDBCluster (Maybe Text)
cdcMasterUserPassword = lens _cdcMasterUserPassword (\ s a -> s{_cdcMasterUserPassword = a})

-- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster if this DB cluster is created as a Read Replica.
cdcReplicationSourceIdentifier :: Lens' CreateDBCluster (Maybe Text)
cdcReplicationSourceIdentifier = lens _cdcReplicationSourceIdentifier (\ s a -> s{_cdcReplicationSourceIdentifier = a})

-- | The global cluster ID of an Aurora cluster that becomes the primary cluster in the new global database cluster.
cdcGlobalClusterIdentifier :: Lens' CreateDBCluster (Maybe Text)
cdcGlobalClusterIdentifier = lens _cdcGlobalClusterIdentifier (\ s a -> s{_cdcGlobalClusterIdentifier = a})

-- | The name of the master user for the DB cluster. Constraints:     * Must be 1 to 16 letters or numbers.     * First character must be a letter.     * Can't be a reserved word for the chosen database engine.
cdcMasterUsername :: Lens' CreateDBCluster (Maybe Text)
cdcMasterUsername = lens _cdcMasterUsername (\ s a -> s{_cdcMasterUsername = a})

-- | A DB subnet group to associate with this DB cluster. Constraints: Must match the name of an existing DBSubnetGroup. Must not be default. Example: @mySubnetgroup@
cdcDBSubnetGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcDBSubnetGroupName = lens _cdcDBSubnetGroupName (\ s a -> s{_cdcDBSubnetGroupName = a})

-- | The target backtrack window, in seconds. To disable backtracking, set this value to 0.  Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
cdcBacktrackWindow :: Lens' CreateDBCluster (Maybe Integer)
cdcBacktrackWindow = lens _cdcBacktrackWindow (\ s a -> s{_cdcBacktrackWindow = a})

-- | A URL that contains a Signature Version 4 signed request for the @CreateDBCluster@ action to be called in the source AWS Region where the DB cluster is replicated from. You only need to specify @PreSignedUrl@ when you are performing cross-region replication from an encrypted DB cluster. The pre-signed URL must be a valid request for the @CreateDBCluster@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster to be copied. The pre-signed URL request must contain the following parameter values:     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster in the destination AWS Region. This should refer to the same KMS key for both the @CreateDBCluster@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.     * @DestinationRegion@ - The name of the AWS Region that Aurora Read Replica will be created in.     * @ReplicationSourceIdentifier@ - The DB cluster identifier for the encrypted DB cluster to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster from the us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would look like Example: @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@ . To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
cdcPreSignedURL :: Lens' CreateDBCluster (Maybe Text)
cdcPreSignedURL = lens _cdcPreSignedURL (\ s a -> s{_cdcPreSignedURL = a})

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun. Constraints: Minimum 30-minute window.
cdcPreferredMaintenanceWindow :: Lens' CreateDBCluster (Maybe Text)
cdcPreferredMaintenanceWindow = lens _cdcPreferredMaintenanceWindow (\ s a -> s{_cdcPreferredMaintenanceWindow = a})

-- | A list of EC2 Availability Zones that instances in the DB cluster can be created in. For information on AWS Regions and Availability Zones, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones> in the /Amazon Aurora User Guide/ .
cdcAvailabilityZones :: Lens' CreateDBCluster [Text]
cdcAvailabilityZones = lens _cdcAvailabilityZones (\ s a -> s{_cdcAvailabilityZones = a}) . _Default . _Coerce

-- | A value that indicates that the DB cluster should be associated with the specified CharacterSet.
cdcCharacterSetName :: Lens' CreateDBCluster (Maybe Text)
cdcCharacterSetName = lens _cdcCharacterSetName (\ s a -> s{_cdcCharacterSetName = a})

-- | The AWS KMS key identifier for an encrypted DB cluster. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key. If an encryption key is not specified in @KmsKeyId@ :     * If @ReplicationSourceIdentifier@ identifies an encrypted source, then Amazon RDS will use the encryption key used to encrypt the source. Otherwise, Amazon RDS will use your default encryption key.      * If the @StorageEncrypted@ parameter is true and @ReplicationSourceIdentifier@ is not specified, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region. If you create a Read Replica of an encrypted DB cluster in another AWS Region, you must set @KmsKeyId@ to a KMS key ID that is valid in the destination AWS Region. This key is used to encrypt the Read Replica in that AWS Region.
cdcKMSKeyId :: Lens' CreateDBCluster (Maybe Text)
cdcKMSKeyId = lens _cdcKMSKeyId (\ s a -> s{_cdcKMSKeyId = a})

-- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window> in the /Amazon Aurora User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
cdcPreferredBackupWindow :: Lens' CreateDBCluster (Maybe Text)
cdcPreferredBackupWindow = lens _cdcPreferredBackupWindow (\ s a -> s{_cdcPreferredBackupWindow = a})

-- | The number of days for which automated backups are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35
cdcBackupRetentionPeriod :: Lens' CreateDBCluster (Maybe Int)
cdcBackupRetentionPeriod = lens _cdcBackupRetentionPeriod (\ s a -> s{_cdcBackupRetentionPeriod = a})

-- | A list of EC2 VPC security groups to associate with this DB cluster.
cdcVPCSecurityGroupIds :: Lens' CreateDBCluster [Text]
cdcVPCSecurityGroupIds = lens _cdcVPCSecurityGroupIds (\ s a -> s{_cdcVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon RDS will not create a database in the DB cluster you are creating.
cdcDatabaseName :: Lens' CreateDBCluster (Maybe Text)
cdcDatabaseName = lens _cdcDatabaseName (\ s a -> s{_cdcDatabaseName = a})

-- | The name of the DB cluster parameter group to associate with this DB cluster. If this argument is omitted, @default.aurora5.6@ is used.  Constraints:     * If supplied, must match the name of an existing DB cluster parameter group.
cdcDBClusterParameterGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcDBClusterParameterGroupName = lens _cdcDBClusterParameterGroupName (\ s a -> s{_cdcDBClusterParameterGroupName = a})

-- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , or @global@ .
cdcEngineMode :: Lens' CreateDBCluster (Maybe Text)
cdcEngineMode = lens _cdcEngineMode (\ s a -> s{_cdcEngineMode = a})

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties of the DB cluster.
cdcScalingConfiguration :: Lens' CreateDBCluster (Maybe ScalingConfiguration)
cdcScalingConfiguration = lens _cdcScalingConfiguration (\ s a -> s{_cdcScalingConfiguration = a})

-- | A value that indicates that the DB cluster should be associated with the specified option group. Permanent options can't be removed from an option group. The option group can't be removed from a DB cluster once it is associated with a DB cluster.
cdcOptionGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcOptionGroupName = lens _cdcOptionGroupName (\ s a -> s{_cdcOptionGroupName = a})

-- | True to copy all tags from the DB cluster to snapshots of the DB cluster, and otherwise false. The default is false.
cdcCopyTagsToSnapshot :: Lens' CreateDBCluster (Maybe Bool)
cdcCopyTagsToSnapshot = lens _cdcCopyTagsToSnapshot (\ s a -> s{_cdcCopyTagsToSnapshot = a})

-- | Undocumented member.
cdcTags :: Lens' CreateDBCluster [Tag]
cdcTags = lens _cdcTags (\ s a -> s{_cdcTags = a}) . _Default . _Coerce

-- | The port number on which the instances in the DB cluster accept connections. Default: @3306@ if engine is set as aurora or @5432@ if set to aurora-postgresql.
cdcPort :: Lens' CreateDBCluster (Maybe Int)
cdcPort = lens _cdcPort (\ s a -> s{_cdcPort = a})

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. Default: @false@
cdcEnableIAMDatabaseAuthentication :: Lens' CreateDBCluster (Maybe Bool)
cdcEnableIAMDatabaseAuthentication = lens _cdcEnableIAMDatabaseAuthentication (\ s a -> s{_cdcEnableIAMDatabaseAuthentication = a})

-- | The list of log types that need to be enabled for exporting to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs> in the /Amazon Aurora User Guide/ .
cdcEnableCloudwatchLogsExports :: Lens' CreateDBCluster [Text]
cdcEnableCloudwatchLogsExports = lens _cdcEnableCloudwatchLogsExports (\ s a -> s{_cdcEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The DB cluster identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@
cdcDBClusterIdentifier :: Lens' CreateDBCluster Text
cdcDBClusterIdentifier = lens _cdcDBClusterIdentifier (\ s a -> s{_cdcDBClusterIdentifier = a})

-- | The name of the database engine to be used for this DB cluster. Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@ (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
cdcEngine :: Lens' CreateDBCluster Text
cdcEngine = lens _cdcEngine (\ s a -> s{_cdcEngine = a})

instance AWSRequest CreateDBCluster where
        type Rs CreateDBCluster = CreateDBClusterResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CreateDBClusterResult"
              (\ s h x ->
                 CreateDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable CreateDBCluster where

instance NFData CreateDBCluster where

instance ToHeaders CreateDBCluster where
        toHeaders = const mempty

instance ToPath CreateDBCluster where
        toPath = const "/"

instance ToQuery CreateDBCluster where
        toQuery CreateDBCluster'{..}
          = mconcat
              ["Action" =: ("CreateDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _cdcEngineVersion,
               "DeletionProtection" =: _cdcDeletionProtection,
               "StorageEncrypted" =: _cdcStorageEncrypted,
               "MasterUserPassword" =: _cdcMasterUserPassword,
               "ReplicationSourceIdentifier" =:
                 _cdcReplicationSourceIdentifier,
               "GlobalClusterIdentifier" =:
                 _cdcGlobalClusterIdentifier,
               "MasterUsername" =: _cdcMasterUsername,
               "DBSubnetGroupName" =: _cdcDBSubnetGroupName,
               "BacktrackWindow" =: _cdcBacktrackWindow,
               "PreSignedUrl" =: _cdcPreSignedURL,
               "PreferredMaintenanceWindow" =:
                 _cdcPreferredMaintenanceWindow,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _cdcAvailabilityZones),
               "CharacterSetName" =: _cdcCharacterSetName,
               "KmsKeyId" =: _cdcKMSKeyId,
               "PreferredBackupWindow" =: _cdcPreferredBackupWindow,
               "BackupRetentionPeriod" =: _cdcBackupRetentionPeriod,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _cdcVPCSecurityGroupIds),
               "DatabaseName" =: _cdcDatabaseName,
               "DBClusterParameterGroupName" =:
                 _cdcDBClusterParameterGroupName,
               "EngineMode" =: _cdcEngineMode,
               "ScalingConfiguration" =: _cdcScalingConfiguration,
               "OptionGroupName" =: _cdcOptionGroupName,
               "CopyTagsToSnapshot" =: _cdcCopyTagsToSnapshot,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdcTags),
               "Port" =: _cdcPort,
               "EnableIAMDatabaseAuthentication" =:
                 _cdcEnableIAMDatabaseAuthentication,
               "EnableCloudwatchLogsExports" =:
                 toQuery
                   (toQueryList "member" <$>
                      _cdcEnableCloudwatchLogsExports),
               "DBClusterIdentifier" =: _cdcDBClusterIdentifier,
               "Engine" =: _cdcEngine]

-- | /See:/ 'createDBClusterResponse' smart constructor.
data CreateDBClusterResponse = CreateDBClusterResponse'
  { _cdcrsDBCluster      :: !(Maybe DBCluster)
  , _cdcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDBCluster' - Undocumented member.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDBClusterResponse
    :: Int -- ^ 'cdcrsResponseStatus'
    -> CreateDBClusterResponse
createDBClusterResponse pResponseStatus_ =
  CreateDBClusterResponse'
    {_cdcrsDBCluster = Nothing, _cdcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdcrsDBCluster :: Lens' CreateDBClusterResponse (Maybe DBCluster)
cdcrsDBCluster = lens _cdcrsDBCluster (\ s a -> s{_cdcrsDBCluster = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDBClusterResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\ s a -> s{_cdcrsResponseStatus = a})

instance NFData CreateDBClusterResponse where
