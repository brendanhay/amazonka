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
-- Module      : Network.AWS.RDS.RestoreDBClusterFromS3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Aurora DB cluster from data stored in an Amazon S3 bucket. Amazon RDS must be authorized to access the Amazon S3 bucket and the data must be created using the Percona XtraBackup utility as described in <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Migrate.MySQL.html#Aurora.Migrate.MySQL.S3 Migrating Data from MySQL by Using an Amazon S3 Bucket> .
--
--
module Network.AWS.RDS.RestoreDBClusterFromS3
    (
    -- * Creating a Request
      restoreDBClusterFromS3
    , RestoreDBClusterFromS3
    -- * Request Lenses
    , rdcfsEngineVersion
    , rdcfsStorageEncrypted
    , rdcfsDBSubnetGroupName
    , rdcfsBacktrackWindow
    , rdcfsPreferredMaintenanceWindow
    , rdcfsAvailabilityZones
    , rdcfsCharacterSetName
    , rdcfsKMSKeyId
    , rdcfsPreferredBackupWindow
    , rdcfsBackupRetentionPeriod
    , rdcfsVPCSecurityGroupIds
    , rdcfsDatabaseName
    , rdcfsDBClusterParameterGroupName
    , rdcfsS3Prefix
    , rdcfsOptionGroupName
    , rdcfsTags
    , rdcfsPort
    , rdcfsEnableIAMDatabaseAuthentication
    , rdcfsDBClusterIdentifier
    , rdcfsEngine
    , rdcfsMasterUsername
    , rdcfsMasterUserPassword
    , rdcfsSourceEngine
    , rdcfsSourceEngineVersion
    , rdcfsS3BucketName
    , rdcfsS3IngestionRoleARN

    -- * Destructuring the Response
    , restoreDBClusterFromS3Response
    , RestoreDBClusterFromS3Response
    -- * Response Lenses
    , rdcfsrsDBCluster
    , rdcfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreDBClusterFromS3' smart constructor.
data RestoreDBClusterFromS3 = RestoreDBClusterFromS3'
  { _rdcfsEngineVersion                   :: !(Maybe Text)
  , _rdcfsStorageEncrypted                :: !(Maybe Bool)
  , _rdcfsDBSubnetGroupName               :: !(Maybe Text)
  , _rdcfsBacktrackWindow                 :: !(Maybe Integer)
  , _rdcfsPreferredMaintenanceWindow      :: !(Maybe Text)
  , _rdcfsAvailabilityZones               :: !(Maybe [Text])
  , _rdcfsCharacterSetName                :: !(Maybe Text)
  , _rdcfsKMSKeyId                        :: !(Maybe Text)
  , _rdcfsPreferredBackupWindow           :: !(Maybe Text)
  , _rdcfsBackupRetentionPeriod           :: !(Maybe Int)
  , _rdcfsVPCSecurityGroupIds             :: !(Maybe [Text])
  , _rdcfsDatabaseName                    :: !(Maybe Text)
  , _rdcfsDBClusterParameterGroupName     :: !(Maybe Text)
  , _rdcfsS3Prefix                        :: !(Maybe Text)
  , _rdcfsOptionGroupName                 :: !(Maybe Text)
  , _rdcfsTags                            :: !(Maybe [Tag])
  , _rdcfsPort                            :: !(Maybe Int)
  , _rdcfsEnableIAMDatabaseAuthentication :: !(Maybe Bool)
  , _rdcfsDBClusterIdentifier             :: !Text
  , _rdcfsEngine                          :: !Text
  , _rdcfsMasterUsername                  :: !Text
  , _rdcfsMasterUserPassword              :: !Text
  , _rdcfsSourceEngine                    :: !Text
  , _rdcfsSourceEngineVersion             :: !Text
  , _rdcfsS3BucketName                    :: !Text
  , _rdcfsS3IngestionRoleARN              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterFromS3' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcfsEngineVersion' - The version number of the database engine to use. __Aurora MySQL__  Example: @5.6.10a@  __Aurora PostgreSQL__  Example: @9.6.3@
--
-- * 'rdcfsStorageEncrypted' - Specifies whether the restored DB cluster is encrypted.
--
-- * 'rdcfsDBSubnetGroupName' - A DB subnet group to associate with the restored DB cluster. Constraints: If supplied, must match the name of an existing DBSubnetGroup.  Example: @mySubnetgroup@
--
-- * 'rdcfsBacktrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0. Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
-- * 'rdcfsPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun. Constraints: Minimum 30-minute window.
--
-- * 'rdcfsAvailabilityZones' - A list of EC2 Availability Zones that instances in the restored DB cluster can be created in.
--
-- * 'rdcfsCharacterSetName' - A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
--
-- * 'rdcfsKMSKeyId' - The AWS KMS key identifier for an encrypted DB cluster. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key. If the @StorageEncrypted@ parameter is true, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'rdcfsPreferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'rdcfsBackupRetentionPeriod' - The number of days for which automated backups of the restored DB cluster are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35
--
-- * 'rdcfsVPCSecurityGroupIds' - A list of EC2 VPC security groups to associate with the restored DB cluster.
--
-- * 'rdcfsDatabaseName' - The database name for the restored DB cluster.
--
-- * 'rdcfsDBClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.  Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
-- * 'rdcfsS3Prefix' - The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
--
-- * 'rdcfsOptionGroupName' - A value that indicates that the restored DB cluster should be associated with the specified option group. Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
--
-- * 'rdcfsTags' - Undocumented member.
--
-- * 'rdcfsPort' - The port number on which the instances in the restored DB cluster accept connections. Default: @3306@
--
-- * 'rdcfsEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. Default: @false@
--
-- * 'rdcfsDBClusterIdentifier' - The name of the DB cluster to create from the source data in the Amazon S3 bucket. This parameter is isn't case-sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@
--
-- * 'rdcfsEngine' - The name of the database engine to be used for the restored DB cluster. Valid Values: @aurora@ , @aurora-postgresql@
--
-- * 'rdcfsMasterUsername' - The name of the master user for the restored DB cluster. Constraints:     * Must be 1 to 16 letters or numbers.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine.
--
-- * 'rdcfsMasterUserPassword' - The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
--
-- * 'rdcfsSourceEngine' - The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.  Valid values: @mysql@
--
-- * 'rdcfsSourceEngineVersion' - The version of the database that the backup files were created from. MySQL version 5.5 and 5.6 are supported.  Example: @5.6.22@
--
-- * 'rdcfsS3BucketName' - The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
--
-- * 'rdcfsS3IngestionRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
restoreDBClusterFromS3
    :: Text -- ^ 'rdcfsDBClusterIdentifier'
    -> Text -- ^ 'rdcfsEngine'
    -> Text -- ^ 'rdcfsMasterUsername'
    -> Text -- ^ 'rdcfsMasterUserPassword'
    -> Text -- ^ 'rdcfsSourceEngine'
    -> Text -- ^ 'rdcfsSourceEngineVersion'
    -> Text -- ^ 'rdcfsS3BucketName'
    -> Text -- ^ 'rdcfsS3IngestionRoleARN'
    -> RestoreDBClusterFromS3
restoreDBClusterFromS3 pDBClusterIdentifier_ pEngine_ pMasterUsername_ pMasterUserPassword_ pSourceEngine_ pSourceEngineVersion_ pS3BucketName_ pS3IngestionRoleARN_ =
  RestoreDBClusterFromS3'
    { _rdcfsEngineVersion = Nothing
    , _rdcfsStorageEncrypted = Nothing
    , _rdcfsDBSubnetGroupName = Nothing
    , _rdcfsBacktrackWindow = Nothing
    , _rdcfsPreferredMaintenanceWindow = Nothing
    , _rdcfsAvailabilityZones = Nothing
    , _rdcfsCharacterSetName = Nothing
    , _rdcfsKMSKeyId = Nothing
    , _rdcfsPreferredBackupWindow = Nothing
    , _rdcfsBackupRetentionPeriod = Nothing
    , _rdcfsVPCSecurityGroupIds = Nothing
    , _rdcfsDatabaseName = Nothing
    , _rdcfsDBClusterParameterGroupName = Nothing
    , _rdcfsS3Prefix = Nothing
    , _rdcfsOptionGroupName = Nothing
    , _rdcfsTags = Nothing
    , _rdcfsPort = Nothing
    , _rdcfsEnableIAMDatabaseAuthentication = Nothing
    , _rdcfsDBClusterIdentifier = pDBClusterIdentifier_
    , _rdcfsEngine = pEngine_
    , _rdcfsMasterUsername = pMasterUsername_
    , _rdcfsMasterUserPassword = pMasterUserPassword_
    , _rdcfsSourceEngine = pSourceEngine_
    , _rdcfsSourceEngineVersion = pSourceEngineVersion_
    , _rdcfsS3BucketName = pS3BucketName_
    , _rdcfsS3IngestionRoleARN = pS3IngestionRoleARN_
    }


-- | The version number of the database engine to use. __Aurora MySQL__  Example: @5.6.10a@  __Aurora PostgreSQL__  Example: @9.6.3@
rdcfsEngineVersion :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsEngineVersion = lens _rdcfsEngineVersion (\ s a -> s{_rdcfsEngineVersion = a})

-- | Specifies whether the restored DB cluster is encrypted.
rdcfsStorageEncrypted :: Lens' RestoreDBClusterFromS3 (Maybe Bool)
rdcfsStorageEncrypted = lens _rdcfsStorageEncrypted (\ s a -> s{_rdcfsStorageEncrypted = a})

-- | A DB subnet group to associate with the restored DB cluster. Constraints: If supplied, must match the name of an existing DBSubnetGroup.  Example: @mySubnetgroup@
rdcfsDBSubnetGroupName :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsDBSubnetGroupName = lens _rdcfsDBSubnetGroupName (\ s a -> s{_rdcfsDBSubnetGroupName = a})

-- | The target backtrack window, in seconds. To disable backtracking, set this value to 0. Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
rdcfsBacktrackWindow :: Lens' RestoreDBClusterFromS3 (Maybe Integer)
rdcfsBacktrackWindow = lens _rdcfsBacktrackWindow (\ s a -> s{_rdcfsBacktrackWindow = a})

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). Format: @ddd:hh24:mi-ddd:hh24:mi@  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun. Constraints: Minimum 30-minute window.
rdcfsPreferredMaintenanceWindow :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsPreferredMaintenanceWindow = lens _rdcfsPreferredMaintenanceWindow (\ s a -> s{_rdcfsPreferredMaintenanceWindow = a})

-- | A list of EC2 Availability Zones that instances in the restored DB cluster can be created in.
rdcfsAvailabilityZones :: Lens' RestoreDBClusterFromS3 [Text]
rdcfsAvailabilityZones = lens _rdcfsAvailabilityZones (\ s a -> s{_rdcfsAvailabilityZones = a}) . _Default . _Coerce

-- | A value that indicates that the restored DB cluster should be associated with the specified CharacterSet.
rdcfsCharacterSetName :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsCharacterSetName = lens _rdcfsCharacterSetName (\ s a -> s{_rdcfsCharacterSetName = a})

-- | The AWS KMS key identifier for an encrypted DB cluster. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KM encryption key. If the @StorageEncrypted@ parameter is true, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
rdcfsKMSKeyId :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsKMSKeyId = lens _rdcfsKMSKeyId (\ s a -> s{_rdcfsKMSKeyId = a})

-- | The daily time range during which automated backups are created if automated backups are enabled using the @BackupRetentionPeriod@ parameter.  The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Must be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
rdcfsPreferredBackupWindow :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsPreferredBackupWindow = lens _rdcfsPreferredBackupWindow (\ s a -> s{_rdcfsPreferredBackupWindow = a})

-- | The number of days for which automated backups of the restored DB cluster are retained. You must specify a minimum value of 1. Default: 1 Constraints:     * Must be a value from 1 to 35
rdcfsBackupRetentionPeriod :: Lens' RestoreDBClusterFromS3 (Maybe Int)
rdcfsBackupRetentionPeriod = lens _rdcfsBackupRetentionPeriod (\ s a -> s{_rdcfsBackupRetentionPeriod = a})

-- | A list of EC2 VPC security groups to associate with the restored DB cluster.
rdcfsVPCSecurityGroupIds :: Lens' RestoreDBClusterFromS3 [Text]
rdcfsVPCSecurityGroupIds = lens _rdcfsVPCSecurityGroupIds (\ s a -> s{_rdcfsVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The database name for the restored DB cluster.
rdcfsDatabaseName :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsDatabaseName = lens _rdcfsDatabaseName (\ s a -> s{_rdcfsDatabaseName = a})

-- | The name of the DB cluster parameter group to associate with the restored DB cluster. If this argument is omitted, @default.aurora5.6@ is used.  Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
rdcfsDBClusterParameterGroupName :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsDBClusterParameterGroupName = lens _rdcfsDBClusterParameterGroupName (\ s a -> s{_rdcfsDBClusterParameterGroupName = a})

-- | The prefix for all of the file names that contain the data used to create the Amazon Aurora DB cluster. If you do not specify a __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created by using all of the files in the Amazon S3 bucket.
rdcfsS3Prefix :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsS3Prefix = lens _rdcfsS3Prefix (\ s a -> s{_rdcfsS3Prefix = a})

-- | A value that indicates that the restored DB cluster should be associated with the specified option group. Permanent options can't be removed from an option group. An option group can't be removed from a DB cluster once it is associated with a DB cluster.
rdcfsOptionGroupName :: Lens' RestoreDBClusterFromS3 (Maybe Text)
rdcfsOptionGroupName = lens _rdcfsOptionGroupName (\ s a -> s{_rdcfsOptionGroupName = a})

-- | Undocumented member.
rdcfsTags :: Lens' RestoreDBClusterFromS3 [Tag]
rdcfsTags = lens _rdcfsTags (\ s a -> s{_rdcfsTags = a}) . _Default . _Coerce

-- | The port number on which the instances in the restored DB cluster accept connections. Default: @3306@
rdcfsPort :: Lens' RestoreDBClusterFromS3 (Maybe Int)
rdcfsPort = lens _rdcfsPort (\ s a -> s{_rdcfsPort = a})

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. Default: @false@
rdcfsEnableIAMDatabaseAuthentication :: Lens' RestoreDBClusterFromS3 (Maybe Bool)
rdcfsEnableIAMDatabaseAuthentication = lens _rdcfsEnableIAMDatabaseAuthentication (\ s a -> s{_rdcfsEnableIAMDatabaseAuthentication = a})

-- | The name of the DB cluster to create from the source data in the Amazon S3 bucket. This parameter is isn't case-sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@
rdcfsDBClusterIdentifier :: Lens' RestoreDBClusterFromS3 Text
rdcfsDBClusterIdentifier = lens _rdcfsDBClusterIdentifier (\ s a -> s{_rdcfsDBClusterIdentifier = a})

-- | The name of the database engine to be used for the restored DB cluster. Valid Values: @aurora@ , @aurora-postgresql@
rdcfsEngine :: Lens' RestoreDBClusterFromS3 Text
rdcfsEngine = lens _rdcfsEngine (\ s a -> s{_rdcfsEngine = a})

-- | The name of the master user for the restored DB cluster. Constraints:     * Must be 1 to 16 letters or numbers.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine.
rdcfsMasterUsername :: Lens' RestoreDBClusterFromS3 Text
rdcfsMasterUsername = lens _rdcfsMasterUsername (\ s a -> s{_rdcfsMasterUsername = a})

-- | The password for the master database user. This password can contain any printable ASCII character except "/", """, or "@". Constraints: Must contain from 8 to 41 characters.
rdcfsMasterUserPassword :: Lens' RestoreDBClusterFromS3 Text
rdcfsMasterUserPassword = lens _rdcfsMasterUserPassword (\ s a -> s{_rdcfsMasterUserPassword = a})

-- | The identifier for the database engine that was backed up to create the files stored in the Amazon S3 bucket.  Valid values: @mysql@
rdcfsSourceEngine :: Lens' RestoreDBClusterFromS3 Text
rdcfsSourceEngine = lens _rdcfsSourceEngine (\ s a -> s{_rdcfsSourceEngine = a})

-- | The version of the database that the backup files were created from. MySQL version 5.5 and 5.6 are supported.  Example: @5.6.22@
rdcfsSourceEngineVersion :: Lens' RestoreDBClusterFromS3 Text
rdcfsSourceEngineVersion = lens _rdcfsSourceEngineVersion (\ s a -> s{_rdcfsSourceEngineVersion = a})

-- | The name of the Amazon S3 bucket that contains the data used to create the Amazon Aurora DB cluster.
rdcfsS3BucketName :: Lens' RestoreDBClusterFromS3 Text
rdcfsS3BucketName = lens _rdcfsS3BucketName (\ s a -> s{_rdcfsS3BucketName = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon RDS to access the Amazon S3 bucket on your behalf.
rdcfsS3IngestionRoleARN :: Lens' RestoreDBClusterFromS3 Text
rdcfsS3IngestionRoleARN = lens _rdcfsS3IngestionRoleARN (\ s a -> s{_rdcfsS3IngestionRoleARN = a})

instance AWSRequest RestoreDBClusterFromS3 where
        type Rs RestoreDBClusterFromS3 =
             RestoreDBClusterFromS3Response
        request = postQuery rds
        response
          = receiveXMLWrapper "RestoreDBClusterFromS3Result"
              (\ s h x ->
                 RestoreDBClusterFromS3Response' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable RestoreDBClusterFromS3 where

instance NFData RestoreDBClusterFromS3 where

instance ToHeaders RestoreDBClusterFromS3 where
        toHeaders = const mempty

instance ToPath RestoreDBClusterFromS3 where
        toPath = const "/"

instance ToQuery RestoreDBClusterFromS3 where
        toQuery RestoreDBClusterFromS3'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBClusterFromS3" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _rdcfsEngineVersion,
               "StorageEncrypted" =: _rdcfsStorageEncrypted,
               "DBSubnetGroupName" =: _rdcfsDBSubnetGroupName,
               "BacktrackWindow" =: _rdcfsBacktrackWindow,
               "PreferredMaintenanceWindow" =:
                 _rdcfsPreferredMaintenanceWindow,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _rdcfsAvailabilityZones),
               "CharacterSetName" =: _rdcfsCharacterSetName,
               "KmsKeyId" =: _rdcfsKMSKeyId,
               "PreferredBackupWindow" =:
                 _rdcfsPreferredBackupWindow,
               "BackupRetentionPeriod" =:
                 _rdcfsBackupRetentionPeriod,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _rdcfsVPCSecurityGroupIds),
               "DatabaseName" =: _rdcfsDatabaseName,
               "DBClusterParameterGroupName" =:
                 _rdcfsDBClusterParameterGroupName,
               "S3Prefix" =: _rdcfsS3Prefix,
               "OptionGroupName" =: _rdcfsOptionGroupName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _rdcfsTags),
               "Port" =: _rdcfsPort,
               "EnableIAMDatabaseAuthentication" =:
                 _rdcfsEnableIAMDatabaseAuthentication,
               "DBClusterIdentifier" =: _rdcfsDBClusterIdentifier,
               "Engine" =: _rdcfsEngine,
               "MasterUsername" =: _rdcfsMasterUsername,
               "MasterUserPassword" =: _rdcfsMasterUserPassword,
               "SourceEngine" =: _rdcfsSourceEngine,
               "SourceEngineVersion" =: _rdcfsSourceEngineVersion,
               "S3BucketName" =: _rdcfsS3BucketName,
               "S3IngestionRoleArn" =: _rdcfsS3IngestionRoleARN]

-- | /See:/ 'restoreDBClusterFromS3Response' smart constructor.
data RestoreDBClusterFromS3Response = RestoreDBClusterFromS3Response'
  { _rdcfsrsDBCluster      :: !(Maybe DBCluster)
  , _rdcfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterFromS3Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcfsrsDBCluster' - Undocumented member.
--
-- * 'rdcfsrsResponseStatus' - -- | The response status code.
restoreDBClusterFromS3Response
    :: Int -- ^ 'rdcfsrsResponseStatus'
    -> RestoreDBClusterFromS3Response
restoreDBClusterFromS3Response pResponseStatus_ =
  RestoreDBClusterFromS3Response'
    {_rdcfsrsDBCluster = Nothing, _rdcfsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rdcfsrsDBCluster :: Lens' RestoreDBClusterFromS3Response (Maybe DBCluster)
rdcfsrsDBCluster = lens _rdcfsrsDBCluster (\ s a -> s{_rdcfsrsDBCluster = a})

-- | -- | The response status code.
rdcfsrsResponseStatus :: Lens' RestoreDBClusterFromS3Response Int
rdcfsrsResponseStatus = lens _rdcfsrsResponseStatus (\ s a -> s{_rdcfsrsResponseStatus = a})

instance NFData RestoreDBClusterFromS3Response where
