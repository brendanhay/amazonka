{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PromoteReadReplica
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Promotes a Read Replica DB instance to a standalone DB instance.
--
-- We recommend that you enable automated backups on your Read Replica
-- before promoting the Read Replica. This ensures that no backup is taken
-- during the promotion process. Once the instance is promoted to a primary
-- instance, backups are taken based on your backup settings.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_PromoteReadReplica.html>
module Network.AWS.RDS.PromoteReadReplica
    (
    -- * Request
      PromoteReadReplica
    -- ** Request constructor
    , promoteReadReplica
    -- ** Request lenses
    , prrPreferredBackupWindow
    , prrBackupRetentionPeriod
    , prrDBInstanceIdentifier

    -- * Response
    , PromoteReadReplicaResponse
    -- ** Response constructor
    , promoteReadReplicaResponse
    -- ** Response lenses
    , prrrsDBInstance
    , prrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'promoteReadReplica' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrPreferredBackupWindow'
--
-- * 'prrBackupRetentionPeriod'
--
-- * 'prrDBInstanceIdentifier'
data PromoteReadReplica = PromoteReadReplica'
    { _prrPreferredBackupWindow :: !(Maybe Text)
    , _prrBackupRetentionPeriod :: !(Maybe Int)
    , _prrDBInstanceIdentifier  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PromoteReadReplica' smart constructor.
promoteReadReplica :: Text -> PromoteReadReplica
promoteReadReplica pDBInstanceIdentifier_ =
    PromoteReadReplica'
    { _prrPreferredBackupWindow = Nothing
    , _prrBackupRetentionPeriod = Nothing
    , _prrDBInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter.
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region. See the Amazon RDS User Guide for the time blocks for
-- each region from which the default backup windows are assigned.
--
-- Constraints: Must be in the format @hh24:mi-hh24:mi@. Times should be
-- Universal Time Coordinated (UTC). Must not conflict with the preferred
-- maintenance window. Must be at least 30 minutes.
prrPreferredBackupWindow :: Lens' PromoteReadReplica (Maybe Text)
prrPreferredBackupWindow = lens _prrPreferredBackupWindow (\ s a -> s{_prrPreferredBackupWindow = a});

-- | The number of days to retain automated backups. Setting this parameter
-- to a positive number enables backups. Setting this parameter to 0
-- disables automated backups.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 8
prrBackupRetentionPeriod :: Lens' PromoteReadReplica (Maybe Int)
prrBackupRetentionPeriod = lens _prrBackupRetentionPeriod (\ s a -> s{_prrBackupRetentionPeriod = a});

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must be the identifier for an existing Read Replica DB instance
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: mydbinstance
prrDBInstanceIdentifier :: Lens' PromoteReadReplica Text
prrDBInstanceIdentifier = lens _prrDBInstanceIdentifier (\ s a -> s{_prrDBInstanceIdentifier = a});

instance AWSRequest PromoteReadReplica where
        type Sv PromoteReadReplica = RDS
        type Rs PromoteReadReplica =
             PromoteReadReplicaResponse
        request = post "PromoteReadReplica"
        response
          = receiveXMLWrapper "PromoteReadReplicaResult"
              (\ s h x ->
                 PromoteReadReplicaResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance ToHeaders PromoteReadReplica where
        toHeaders = const mempty

instance ToPath PromoteReadReplica where
        toPath = const "/"

instance ToQuery PromoteReadReplica where
        toQuery PromoteReadReplica'{..}
          = mconcat
              ["Action" =: ("PromoteReadReplica" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PreferredBackupWindow" =: _prrPreferredBackupWindow,
               "BackupRetentionPeriod" =: _prrBackupRetentionPeriod,
               "DBInstanceIdentifier" =: _prrDBInstanceIdentifier]

-- | /See:/ 'promoteReadReplicaResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrrsDBInstance'
--
-- * 'prrrsStatus'
data PromoteReadReplicaResponse = PromoteReadReplicaResponse'
    { _prrrsDBInstance :: !(Maybe DBInstance)
    , _prrrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PromoteReadReplicaResponse' smart constructor.
promoteReadReplicaResponse :: Int -> PromoteReadReplicaResponse
promoteReadReplicaResponse pStatus_ =
    PromoteReadReplicaResponse'
    { _prrrsDBInstance = Nothing
    , _prrrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
prrrsDBInstance :: Lens' PromoteReadReplicaResponse (Maybe DBInstance)
prrrsDBInstance = lens _prrrsDBInstance (\ s a -> s{_prrrsDBInstance = a});

-- | FIXME: Undocumented member.
prrrsStatus :: Lens' PromoteReadReplicaResponse Int
prrrsStatus = lens _prrrsStatus (\ s a -> s{_prrrsStatus = a});
