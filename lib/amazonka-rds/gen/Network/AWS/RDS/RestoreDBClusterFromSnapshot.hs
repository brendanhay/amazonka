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
-- Module      : Network.AWS.RDS.RestoreDBClusterFromSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster from a DB snapshot or DB cluster snapshot.
--
--
-- If a DB snapshot is specified, the target DB cluster is created from the source DB snapshot with a default configuration and default security group.
--
-- If a DB cluster snapshot is specified, the target DB cluster is created from the source DB cluster restore point with the same configuration as the original source DB cluster, except that the new DB cluster is created with the default security group.
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.RestoreDBClusterFromSnapshot
    (
    -- * Creating a Request
      restoreDBClusterFromSnapshot
    , RestoreDBClusterFromSnapshot
    -- * Request Lenses
    , rdbcfsEngineVersion
    , rdbcfsDBSubnetGroupName
    , rdbcfsBacktrackWindow
    , rdbcfsAvailabilityZones
    , rdbcfsKMSKeyId
    , rdbcfsVPCSecurityGroupIds
    , rdbcfsDatabaseName
    , rdbcfsOptionGroupName
    , rdbcfsTags
    , rdbcfsPort
    , rdbcfsEnableIAMDatabaseAuthentication
    , rdbcfsDBClusterIdentifier
    , rdbcfsSnapshotIdentifier
    , rdbcfsEngine

    -- * Destructuring the Response
    , restoreDBClusterFromSnapshotResponse
    , RestoreDBClusterFromSnapshotResponse
    -- * Response Lenses
    , rdbcfsrsDBCluster
    , rdbcfsrsResponseStatus
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
-- /See:/ 'restoreDBClusterFromSnapshot' smart constructor.
data RestoreDBClusterFromSnapshot = RestoreDBClusterFromSnapshot'
  { _rdbcfsEngineVersion                   :: !(Maybe Text)
  , _rdbcfsDBSubnetGroupName               :: !(Maybe Text)
  , _rdbcfsBacktrackWindow                 :: !(Maybe Integer)
  , _rdbcfsAvailabilityZones               :: !(Maybe [Text])
  , _rdbcfsKMSKeyId                        :: !(Maybe Text)
  , _rdbcfsVPCSecurityGroupIds             :: !(Maybe [Text])
  , _rdbcfsDatabaseName                    :: !(Maybe Text)
  , _rdbcfsOptionGroupName                 :: !(Maybe Text)
  , _rdbcfsTags                            :: !(Maybe [Tag])
  , _rdbcfsPort                            :: !(Maybe Int)
  , _rdbcfsEnableIAMDatabaseAuthentication :: !(Maybe Bool)
  , _rdbcfsDBClusterIdentifier             :: !Text
  , _rdbcfsSnapshotIdentifier              :: !Text
  , _rdbcfsEngine                          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdbcfsEngineVersion' - The version of the database engine to use for the new DB cluster.
--
-- * 'rdbcfsDBSubnetGroupName' - The name of the DB subnet group to use for the new DB cluster. Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetgroup@
--
-- * 'rdbcfsBacktrackWindow' - The target backtrack window, in seconds. To disable backtracking, set this value to 0. Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
--
-- * 'rdbcfsAvailabilityZones' - Provides the list of EC2 Availability Zones that instances in the restored DB cluster can be created in.
--
-- * 'rdbcfsKMSKeyId' - The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key. If you do not specify a value for the @KmsKeyId@ parameter, then the following will occur:     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the DB snapshot or DB cluster snapshot.     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is not encrypted, then the restored DB cluster is not encrypted.
--
-- * 'rdbcfsVPCSecurityGroupIds' - A list of VPC security groups that the new DB cluster will belong to.
--
-- * 'rdbcfsDatabaseName' - The database name for the restored DB cluster.
--
-- * 'rdbcfsOptionGroupName' - The name of the option group to use for the restored DB cluster.
--
-- * 'rdbcfsTags' - The tags to be assigned to the restored DB cluster.
--
-- * 'rdbcfsPort' - The port number on which the new DB cluster accepts connections. Constraints: Value must be @1150-65535@  Default: The same port as the original DB cluster.
--
-- * 'rdbcfsEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. Default: @false@
--
-- * 'rdbcfsDBClusterIdentifier' - The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
--
-- * 'rdbcfsSnapshotIdentifier' - The identifier for the DB snapshot or DB cluster snapshot to restore from. You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot. Constraints:     * Must match the identifier of an existing Snapshot.
--
-- * 'rdbcfsEngine' - The database engine to use for the new DB cluster. Default: The same as source Constraint: Must be compatible with the engine of the source
restoreDBClusterFromSnapshot
    :: Text -- ^ 'rdbcfsDBClusterIdentifier'
    -> Text -- ^ 'rdbcfsSnapshotIdentifier'
    -> Text -- ^ 'rdbcfsEngine'
    -> RestoreDBClusterFromSnapshot
restoreDBClusterFromSnapshot pDBClusterIdentifier_ pSnapshotIdentifier_ pEngine_ =
  RestoreDBClusterFromSnapshot'
    { _rdbcfsEngineVersion = Nothing
    , _rdbcfsDBSubnetGroupName = Nothing
    , _rdbcfsBacktrackWindow = Nothing
    , _rdbcfsAvailabilityZones = Nothing
    , _rdbcfsKMSKeyId = Nothing
    , _rdbcfsVPCSecurityGroupIds = Nothing
    , _rdbcfsDatabaseName = Nothing
    , _rdbcfsOptionGroupName = Nothing
    , _rdbcfsTags = Nothing
    , _rdbcfsPort = Nothing
    , _rdbcfsEnableIAMDatabaseAuthentication = Nothing
    , _rdbcfsDBClusterIdentifier = pDBClusterIdentifier_
    , _rdbcfsSnapshotIdentifier = pSnapshotIdentifier_
    , _rdbcfsEngine = pEngine_
    }


-- | The version of the database engine to use for the new DB cluster.
rdbcfsEngineVersion :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdbcfsEngineVersion = lens _rdbcfsEngineVersion (\ s a -> s{_rdbcfsEngineVersion = a})

-- | The name of the DB subnet group to use for the new DB cluster. Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetgroup@
rdbcfsDBSubnetGroupName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdbcfsDBSubnetGroupName = lens _rdbcfsDBSubnetGroupName (\ s a -> s{_rdbcfsDBSubnetGroupName = a})

-- | The target backtrack window, in seconds. To disable backtracking, set this value to 0. Default: 0 Constraints:     * If specified, this value must be set to a number from 0 to 259,200 (72 hours).
rdbcfsBacktrackWindow :: Lens' RestoreDBClusterFromSnapshot (Maybe Integer)
rdbcfsBacktrackWindow = lens _rdbcfsBacktrackWindow (\ s a -> s{_rdbcfsBacktrackWindow = a})

-- | Provides the list of EC2 Availability Zones that instances in the restored DB cluster can be created in.
rdbcfsAvailabilityZones :: Lens' RestoreDBClusterFromSnapshot [Text]
rdbcfsAvailabilityZones = lens _rdbcfsAvailabilityZones (\ s a -> s{_rdbcfsAvailabilityZones = a}) . _Default . _Coerce

-- | The AWS KMS key identifier to use when restoring an encrypted DB cluster from a DB snapshot or DB cluster snapshot. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are restoring a DB cluster with the same AWS account that owns the KMS encryption key used to encrypt the new DB cluster, then you can use the KMS key alias instead of the ARN for the KMS encryption key. If you do not specify a value for the @KmsKeyId@ parameter, then the following will occur:     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is encrypted, then the restored DB cluster is encrypted using the KMS key that was used to encrypt the DB snapshot or DB cluster snapshot.     * If the DB snapshot or DB cluster snapshot in @SnapshotIdentifier@ is not encrypted, then the restored DB cluster is not encrypted.
rdbcfsKMSKeyId :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdbcfsKMSKeyId = lens _rdbcfsKMSKeyId (\ s a -> s{_rdbcfsKMSKeyId = a})

-- | A list of VPC security groups that the new DB cluster will belong to.
rdbcfsVPCSecurityGroupIds :: Lens' RestoreDBClusterFromSnapshot [Text]
rdbcfsVPCSecurityGroupIds = lens _rdbcfsVPCSecurityGroupIds (\ s a -> s{_rdbcfsVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | The database name for the restored DB cluster.
rdbcfsDatabaseName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdbcfsDatabaseName = lens _rdbcfsDatabaseName (\ s a -> s{_rdbcfsDatabaseName = a})

-- | The name of the option group to use for the restored DB cluster.
rdbcfsOptionGroupName :: Lens' RestoreDBClusterFromSnapshot (Maybe Text)
rdbcfsOptionGroupName = lens _rdbcfsOptionGroupName (\ s a -> s{_rdbcfsOptionGroupName = a})

-- | The tags to be assigned to the restored DB cluster.
rdbcfsTags :: Lens' RestoreDBClusterFromSnapshot [Tag]
rdbcfsTags = lens _rdbcfsTags (\ s a -> s{_rdbcfsTags = a}) . _Default . _Coerce

-- | The port number on which the new DB cluster accepts connections. Constraints: Value must be @1150-65535@  Default: The same port as the original DB cluster.
rdbcfsPort :: Lens' RestoreDBClusterFromSnapshot (Maybe Int)
rdbcfsPort = lens _rdbcfsPort (\ s a -> s{_rdbcfsPort = a})

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. Default: @false@
rdbcfsEnableIAMDatabaseAuthentication :: Lens' RestoreDBClusterFromSnapshot (Maybe Bool)
rdbcfsEnableIAMDatabaseAuthentication = lens _rdbcfsEnableIAMDatabaseAuthentication (\ s a -> s{_rdbcfsEnableIAMDatabaseAuthentication = a})

-- | The name of the DB cluster to create from the DB snapshot or DB cluster snapshot. This parameter isn't case-sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
rdbcfsDBClusterIdentifier :: Lens' RestoreDBClusterFromSnapshot Text
rdbcfsDBClusterIdentifier = lens _rdbcfsDBClusterIdentifier (\ s a -> s{_rdbcfsDBClusterIdentifier = a})

-- | The identifier for the DB snapshot or DB cluster snapshot to restore from. You can use either the name or the Amazon Resource Name (ARN) to specify a DB cluster snapshot. However, you can use only the ARN to specify a DB snapshot. Constraints:     * Must match the identifier of an existing Snapshot.
rdbcfsSnapshotIdentifier :: Lens' RestoreDBClusterFromSnapshot Text
rdbcfsSnapshotIdentifier = lens _rdbcfsSnapshotIdentifier (\ s a -> s{_rdbcfsSnapshotIdentifier = a})

-- | The database engine to use for the new DB cluster. Default: The same as source Constraint: Must be compatible with the engine of the source
rdbcfsEngine :: Lens' RestoreDBClusterFromSnapshot Text
rdbcfsEngine = lens _rdbcfsEngine (\ s a -> s{_rdbcfsEngine = a})

instance AWSRequest RestoreDBClusterFromSnapshot
         where
        type Rs RestoreDBClusterFromSnapshot =
             RestoreDBClusterFromSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "RestoreDBClusterFromSnapshotResult"
              (\ s h x ->
                 RestoreDBClusterFromSnapshotResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable RestoreDBClusterFromSnapshot where

instance NFData RestoreDBClusterFromSnapshot where

instance ToHeaders RestoreDBClusterFromSnapshot where
        toHeaders = const mempty

instance ToPath RestoreDBClusterFromSnapshot where
        toPath = const "/"

instance ToQuery RestoreDBClusterFromSnapshot where
        toQuery RestoreDBClusterFromSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBClusterFromSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _rdbcfsEngineVersion,
               "DBSubnetGroupName" =: _rdbcfsDBSubnetGroupName,
               "BacktrackWindow" =: _rdbcfsBacktrackWindow,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _rdbcfsAvailabilityZones),
               "KmsKeyId" =: _rdbcfsKMSKeyId,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _rdbcfsVPCSecurityGroupIds),
               "DatabaseName" =: _rdbcfsDatabaseName,
               "OptionGroupName" =: _rdbcfsOptionGroupName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rdbcfsTags),
               "Port" =: _rdbcfsPort,
               "EnableIAMDatabaseAuthentication" =:
                 _rdbcfsEnableIAMDatabaseAuthentication,
               "DBClusterIdentifier" =: _rdbcfsDBClusterIdentifier,
               "SnapshotIdentifier" =: _rdbcfsSnapshotIdentifier,
               "Engine" =: _rdbcfsEngine]

-- | /See:/ 'restoreDBClusterFromSnapshotResponse' smart constructor.
data RestoreDBClusterFromSnapshotResponse = RestoreDBClusterFromSnapshotResponse'
  { _rdbcfsrsDBCluster      :: !(Maybe DBCluster)
  , _rdbcfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBClusterFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdbcfsrsDBCluster' - Undocumented member.
--
-- * 'rdbcfsrsResponseStatus' - -- | The response status code.
restoreDBClusterFromSnapshotResponse
    :: Int -- ^ 'rdbcfsrsResponseStatus'
    -> RestoreDBClusterFromSnapshotResponse
restoreDBClusterFromSnapshotResponse pResponseStatus_ =
  RestoreDBClusterFromSnapshotResponse'
    {_rdbcfsrsDBCluster = Nothing, _rdbcfsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rdbcfsrsDBCluster :: Lens' RestoreDBClusterFromSnapshotResponse (Maybe DBCluster)
rdbcfsrsDBCluster = lens _rdbcfsrsDBCluster (\ s a -> s{_rdbcfsrsDBCluster = a})

-- | -- | The response status code.
rdbcfsrsResponseStatus :: Lens' RestoreDBClusterFromSnapshotResponse Int
rdbcfsrsResponseStatus = lens _rdbcfsrsResponseStatus (\ s a -> s{_rdbcfsrsResponseStatus = a})

instance NFData RestoreDBClusterFromSnapshotResponse
         where
