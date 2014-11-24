{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.PromoteReadReplica
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Promotes a read replica DB instance to a standalone DB instance.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_PromoteReadReplica.html>
module Network.AWS.RDS.PromoteReadReplica
    (
    -- * Request
      PromoteReadReplica
    -- ** Request constructor
    , promoteReadReplica
    -- ** Request lenses
    , prrBackupRetentionPeriod
    , prrDBInstanceIdentifier
    , prrPreferredBackupWindow

    -- * Response
    , PromoteReadReplicaResponse
    -- ** Response constructor
    , promoteReadReplicaResponse
    -- ** Response lenses
    , prrrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data PromoteReadReplica = PromoteReadReplica
    { _prrBackupRetentionPeriod :: Maybe Int
    , _prrDBInstanceIdentifier  :: Text
    , _prrPreferredBackupWindow :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'PromoteReadReplica' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'prrDBInstanceIdentifier' @::@ 'Text'
--
-- * 'prrPreferredBackupWindow' @::@ 'Maybe' 'Text'
--
promoteReadReplica :: Text -- ^ 'prrDBInstanceIdentifier'
                   -> PromoteReadReplica
promoteReadReplica p1 = PromoteReadReplica
    { _prrDBInstanceIdentifier  = p1
    , _prrBackupRetentionPeriod = Nothing
    , _prrPreferredBackupWindow = Nothing
    }

-- | The number of days to retain automated backups. Setting this parameter to
-- a positive number enables backups. Setting this parameter to 0 disables
-- automated backups. Default: 1 Constraints: Must be a value from 0 to 8.
prrBackupRetentionPeriod :: Lens' PromoteReadReplica (Maybe Int)
prrBackupRetentionPeriod =
    lens _prrBackupRetentionPeriod
        (\s a -> s { _prrBackupRetentionPeriod = a })

-- | The DB instance identifier. This value is stored as a lowercase string.
-- Constraints: Must be the identifier for an existing read replica DB
-- instance Must contain from 1 to 63 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: mydbinstance.
prrDBInstanceIdentifier :: Lens' PromoteReadReplica Text
prrDBInstanceIdentifier =
    lens _prrDBInstanceIdentifier (\s a -> s { _prrDBInstanceIdentifier = a })

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the 'BackupRetentionPeriod'
-- parameter. Default: A 30-minute window selected at random from an 8-hour
-- block of time per region. See the Amazon RDS User Guide for the time
-- blocks for each region from which the default backup windows are
-- assigned. Constraints: Must be in the format 'hh24:mi-hh24:mi'. Times
-- should be Universal Time Coordinated (UTC). Must not conflict with the
-- preferred maintenance window. Must be at least 30 minutes.
prrPreferredBackupWindow :: Lens' PromoteReadReplica (Maybe Text)
prrPreferredBackupWindow =
    lens _prrPreferredBackupWindow
        (\s a -> s { _prrPreferredBackupWindow = a })

newtype PromoteReadReplicaResponse = PromoteReadReplicaResponse
    { _prrrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show)

-- | 'PromoteReadReplicaResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrrDBInstance' @::@ 'Maybe' 'DBInstance'
--
promoteReadReplicaResponse :: PromoteReadReplicaResponse
promoteReadReplicaResponse = PromoteReadReplicaResponse
    { _prrrDBInstance = Nothing
    }

prrrDBInstance :: Lens' PromoteReadReplicaResponse (Maybe DBInstance)
prrrDBInstance = lens _prrrDBInstance (\s a -> s { _prrrDBInstance = a })

instance ToPath PromoteReadReplica where
    toPath = const "/"

instance ToQuery PromoteReadReplica where
    toQuery PromoteReadReplica{..} = mconcat
        [ "BackupRetentionPeriod" =? _prrBackupRetentionPeriod
        , "DBInstanceIdentifier"  =? _prrDBInstanceIdentifier
        , "PreferredBackupWindow" =? _prrPreferredBackupWindow
        ]

instance ToHeaders PromoteReadReplica

instance AWSRequest PromoteReadReplica where
    type Sv PromoteReadReplica = RDS
    type Rs PromoteReadReplica = PromoteReadReplicaResponse

    request  = post "PromoteReadReplica"
    response = xmlResponse

instance FromXML PromoteReadReplicaResponse where
    parseXML = withElement "PromoteReadReplicaResult" $ \x -> PromoteReadReplicaResponse
        <$> x .@? "DBInstance"
