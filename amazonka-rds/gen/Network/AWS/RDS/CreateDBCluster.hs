{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Aurora DB cluster. For more information on Amazon
-- Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html AWS API Reference> for CreateDBCluster.
module Network.AWS.RDS.CreateDBCluster
    (
    -- * Creating a Request
      CreateDBCluster
    , createDBCluster
    -- * Request Lenses
    , cdcEngineVersion
    , cdcDBClusterIdentifier
    , cdcMasterUserPassword
    , cdcMasterUsername
    , cdcDBSubnetGroupName
    , cdcEngine
    , cdcPreferredMaintenanceWindow
    , cdcCharacterSetName
    , cdcAvailabilityZones
    , cdcPreferredBackupWindow
    , cdcBackupRetentionPeriod
    , cdcDatabaseName
    , cdcVPCSecurityGroupIds
    , cdcDBClusterParameterGroupName
    , cdcOptionGroupName
    , cdcTags
    , cdcPort

    -- * Destructuring the Response
    , CreateDBClusterResponse
    , createDBClusterResponse
    -- * Response Lenses
    , cdcrsDBCluster
    , cdcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcEngineVersion'
--
-- * 'cdcDBClusterIdentifier'
--
-- * 'cdcMasterUserPassword'
--
-- * 'cdcMasterUsername'
--
-- * 'cdcDBSubnetGroupName'
--
-- * 'cdcEngine'
--
-- * 'cdcPreferredMaintenanceWindow'
--
-- * 'cdcCharacterSetName'
--
-- * 'cdcAvailabilityZones'
--
-- * 'cdcPreferredBackupWindow'
--
-- * 'cdcBackupRetentionPeriod'
--
-- * 'cdcDatabaseName'
--
-- * 'cdcVPCSecurityGroupIds'
--
-- * 'cdcDBClusterParameterGroupName'
--
-- * 'cdcOptionGroupName'
--
-- * 'cdcTags'
--
-- * 'cdcPort'
data CreateDBCluster = CreateDBCluster'
    { _cdcEngineVersion               :: !(Maybe Text)
    , _cdcDBClusterIdentifier         :: !(Maybe Text)
    , _cdcMasterUserPassword          :: !(Maybe Text)
    , _cdcMasterUsername              :: !(Maybe Text)
    , _cdcDBSubnetGroupName           :: !(Maybe Text)
    , _cdcEngine                      :: !(Maybe Text)
    , _cdcPreferredMaintenanceWindow  :: !(Maybe Text)
    , _cdcCharacterSetName            :: !(Maybe Text)
    , _cdcAvailabilityZones           :: !(Maybe [Text])
    , _cdcPreferredBackupWindow       :: !(Maybe Text)
    , _cdcBackupRetentionPeriod       :: !(Maybe Int)
    , _cdcDatabaseName                :: !(Maybe Text)
    , _cdcVPCSecurityGroupIds         :: !(Maybe [Text])
    , _cdcDBClusterParameterGroupName :: !(Maybe Text)
    , _cdcOptionGroupName             :: !(Maybe Text)
    , _cdcTags                        :: !(Maybe [Tag])
    , _cdcPort                        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBCluster' smart constructor.
createDBCluster :: CreateDBCluster
createDBCluster =
    CreateDBCluster'
    { _cdcEngineVersion = Nothing
    , _cdcDBClusterIdentifier = Nothing
    , _cdcMasterUserPassword = Nothing
    , _cdcMasterUsername = Nothing
    , _cdcDBSubnetGroupName = Nothing
    , _cdcEngine = Nothing
    , _cdcPreferredMaintenanceWindow = Nothing
    , _cdcCharacterSetName = Nothing
    , _cdcAvailabilityZones = Nothing
    , _cdcPreferredBackupWindow = Nothing
    , _cdcBackupRetentionPeriod = Nothing
    , _cdcDatabaseName = Nothing
    , _cdcVPCSecurityGroupIds = Nothing
    , _cdcDBClusterParameterGroupName = Nothing
    , _cdcOptionGroupName = Nothing
    , _cdcTags = Nothing
    , _cdcPort = Nothing
    }

-- | The version number of the database engine to use.
--
-- __Aurora__
--
-- Example: @5.6.0@
cdcEngineVersion :: Lens' CreateDBCluster (Maybe Text)
cdcEngineVersion = lens _cdcEngineVersion (\ s a -> s{_cdcEngineVersion = a});

-- | The DB cluster identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
cdcDBClusterIdentifier :: Lens' CreateDBCluster (Maybe Text)
cdcDBClusterIdentifier = lens _cdcDBClusterIdentifier (\ s a -> s{_cdcDBClusterIdentifier = a});

-- | The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
cdcMasterUserPassword :: Lens' CreateDBCluster (Maybe Text)
cdcMasterUserPassword = lens _cdcMasterUserPassword (\ s a -> s{_cdcMasterUserPassword = a});

-- | The name of the master user for the client DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot be a reserved word for the chosen database engine.
cdcMasterUsername :: Lens' CreateDBCluster (Maybe Text)
cdcMasterUsername = lens _cdcMasterUsername (\ s a -> s{_cdcMasterUsername = a});

-- | A DB subnet group to associate with this DB cluster.
cdcDBSubnetGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcDBSubnetGroupName = lens _cdcDBSubnetGroupName (\ s a -> s{_cdcDBSubnetGroupName = a});

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @MySQL@
cdcEngine :: Lens' CreateDBCluster (Maybe Text)
cdcEngine = lens _cdcEngine (\ s a -> s{_cdcEngine = a});

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region, occurring on a random day of the week. To see the time
-- blocks available, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
cdcPreferredMaintenanceWindow :: Lens' CreateDBCluster (Maybe Text)
cdcPreferredMaintenanceWindow = lens _cdcPreferredMaintenanceWindow (\ s a -> s{_cdcPreferredMaintenanceWindow = a});

-- | A value that indicates that the DB cluster should be associated with the
-- specified CharacterSet.
cdcCharacterSetName :: Lens' CreateDBCluster (Maybe Text)
cdcCharacterSetName = lens _cdcCharacterSetName (\ s a -> s{_cdcCharacterSetName = a});

-- | A list of EC2 Availability Zones that instances in the DB cluster can be
-- created in. For information on regions and Availability Zones, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
cdcAvailabilityZones :: Lens' CreateDBCluster [Text]
cdcAvailabilityZones = lens _cdcAvailabilityZones (\ s a -> s{_cdcAvailabilityZones = a}) . _Default . _Coerce;

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region. To see the time blocks available, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
-- -   Times should be in Universal Coordinated Time (UTC).
-- -   Must not conflict with the preferred maintenance window.
-- -   Must be at least 30 minutes.
cdcPreferredBackupWindow :: Lens' CreateDBCluster (Maybe Text)
cdcPreferredBackupWindow = lens _cdcPreferredBackupWindow (\ s a -> s{_cdcPreferredBackupWindow = a});

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to 0 disables automated backups.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
cdcBackupRetentionPeriod :: Lens' CreateDBCluster (Maybe Int)
cdcBackupRetentionPeriod = lens _cdcBackupRetentionPeriod (\ s a -> s{_cdcBackupRetentionPeriod = a});

-- | The name for your database of up to 8 alpha-numeric characters. If you
-- do not provide a name, Amazon RDS will not create a database in the DB
-- cluster you are creating.
cdcDatabaseName :: Lens' CreateDBCluster (Maybe Text)
cdcDatabaseName = lens _cdcDatabaseName (\ s a -> s{_cdcDatabaseName = a});

-- | A list of EC2 VPC security groups to associate with this DB cluster.
cdcVPCSecurityGroupIds :: Lens' CreateDBCluster [Text]
cdcVPCSecurityGroupIds = lens _cdcVPCSecurityGroupIds (\ s a -> s{_cdcVPCSecurityGroupIds = a}) . _Default . _Coerce;

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, @default.aurora5.6@ for the
-- specified engine will be used.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
cdcDBClusterParameterGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcDBClusterParameterGroupName = lens _cdcDBClusterParameterGroupName (\ s a -> s{_cdcDBClusterParameterGroupName = a});

-- | A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- Permanent options cannot be removed from an option group. The option
-- group cannot be removed from a DB cluster once it is associated with a
-- DB cluster.
cdcOptionGroupName :: Lens' CreateDBCluster (Maybe Text)
cdcOptionGroupName = lens _cdcOptionGroupName (\ s a -> s{_cdcOptionGroupName = a});

-- | Undocumented member.
cdcTags :: Lens' CreateDBCluster [Tag]
cdcTags = lens _cdcTags (\ s a -> s{_cdcTags = a}) . _Default . _Coerce;

-- | The port number on which the instances in the DB cluster accept
-- connections.
--
-- Default: @3306@
cdcPort :: Lens' CreateDBCluster (Maybe Int)
cdcPort = lens _cdcPort (\ s a -> s{_cdcPort = a});

instance AWSRequest CreateDBCluster where
        type Sv CreateDBCluster = RDS
        type Rs CreateDBCluster = CreateDBClusterResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateDBClusterResult"
              (\ s h x ->
                 CreateDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

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
               "DBClusterIdentifier" =: _cdcDBClusterIdentifier,
               "MasterUserPassword" =: _cdcMasterUserPassword,
               "MasterUsername" =: _cdcMasterUsername,
               "DBSubnetGroupName" =: _cdcDBSubnetGroupName,
               "Engine" =: _cdcEngine,
               "PreferredMaintenanceWindow" =:
                 _cdcPreferredMaintenanceWindow,
               "CharacterSetName" =: _cdcCharacterSetName,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _cdcAvailabilityZones),
               "PreferredBackupWindow" =: _cdcPreferredBackupWindow,
               "BackupRetentionPeriod" =: _cdcBackupRetentionPeriod,
               "DatabaseName" =: _cdcDatabaseName,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _cdcVPCSecurityGroupIds),
               "DBClusterParameterGroupName" =:
                 _cdcDBClusterParameterGroupName,
               "OptionGroupName" =: _cdcOptionGroupName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdcTags),
               "Port" =: _cdcPort]

-- | /See:/ 'createDBClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcrsDBCluster'
--
-- * 'cdcrsStatus'
data CreateDBClusterResponse = CreateDBClusterResponse'
    { _cdcrsDBCluster :: !(Maybe DBCluster)
    , _cdcrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBClusterResponse' smart constructor.
createDBClusterResponse :: Int -> CreateDBClusterResponse
createDBClusterResponse pStatus_ =
    CreateDBClusterResponse'
    { _cdcrsDBCluster = Nothing
    , _cdcrsStatus = pStatus_
    }

-- | Undocumented member.
cdcrsDBCluster :: Lens' CreateDBClusterResponse (Maybe DBCluster)
cdcrsDBCluster = lens _cdcrsDBCluster (\ s a -> s{_cdcrsDBCluster = a});

-- | Undocumented member.
cdcrsStatus :: Lens' CreateDBClusterResponse Int
cdcrsStatus = lens _cdcrsStatus (\ s a -> s{_cdcrsStatus = a});
