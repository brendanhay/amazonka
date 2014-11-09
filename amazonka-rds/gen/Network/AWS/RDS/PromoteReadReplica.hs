{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.RDS.PromoteReadReplica
    (
    -- * Request
      PromoteReadReplicaMessage
    -- ** Request constructor
    , promoteReadReplicaMessage
    -- ** Request lenses
    , prrmBackupRetentionPeriod
    , prrmDBInstanceIdentifier
    , prrmPreferredBackupWindow

    -- * Response
    , PromoteReadReplicaResult
    -- ** Response constructor
    , promoteReadReplicaResult
    -- ** Response lenses
    , prrrDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data PromoteReadReplicaMessage = PromoteReadReplicaMessage
    { _prrmBackupRetentionPeriod :: Maybe Int
    , _prrmDBInstanceIdentifier  :: Text
    , _prrmPreferredBackupWindow :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PromoteReadReplicaMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrmBackupRetentionPeriod' @::@ 'Maybe' 'Int'
--
-- * 'prrmDBInstanceIdentifier' @::@ 'Text'
--
-- * 'prrmPreferredBackupWindow' @::@ 'Maybe' 'Text'
--
promoteReadReplicaMessage :: Text -- ^ 'prrmDBInstanceIdentifier'
                          -> PromoteReadReplicaMessage
promoteReadReplicaMessage p1 = PromoteReadReplicaMessage
    { _prrmDBInstanceIdentifier  = p1
    , _prrmBackupRetentionPeriod = Nothing
    , _prrmPreferredBackupWindow = Nothing
    }

-- | The number of days to retain automated backups. Setting this parameter to
-- a positive number enables backups. Setting this parameter to 0 disables
-- automated backups. Default: 1 Constraints: Must be a value from 0 to 8.
prrmBackupRetentionPeriod :: Lens' PromoteReadReplicaMessage (Maybe Int)
prrmBackupRetentionPeriod =
    lens _prrmBackupRetentionPeriod
        (\s a -> s { _prrmBackupRetentionPeriod = a })

-- | The DB instance identifier. This value is stored as a lowercase string.
-- Constraints: Must be the identifier for an existing read replica DB
-- instance Must contain from 1 to 63 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: mydbinstance.
prrmDBInstanceIdentifier :: Lens' PromoteReadReplicaMessage Text
prrmDBInstanceIdentifier =
    lens _prrmDBInstanceIdentifier
        (\s a -> s { _prrmDBInstanceIdentifier = a })

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the BackupRetentionPeriod parameter.
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region. See the Amazon RDS User Guide for the time blocks for
-- each region from which the default backup windows are assigned.
-- Constraints: Must be in the format hh24:mi-hh24:mi. Times should be
-- Universal Time Coordinated (UTC). Must not conflict with the preferred
-- maintenance window. Must be at least 30 minutes.
prrmPreferredBackupWindow :: Lens' PromoteReadReplicaMessage (Maybe Text)
prrmPreferredBackupWindow =
    lens _prrmPreferredBackupWindow
        (\s a -> s { _prrmPreferredBackupWindow = a })

instance ToPath PromoteReadReplicaMessage where
    toPath = const "/"

instance ToQuery PromoteReadReplicaMessage

newtype PromoteReadReplicaResult = PromoteReadReplicaResult
    { _prrrDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'PromoteReadReplicaResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrrDBInstance' @::@ 'Maybe' 'DBInstance'
--
promoteReadReplicaResult :: PromoteReadReplicaResult
promoteReadReplicaResult = PromoteReadReplicaResult
    { _prrrDBInstance = Nothing
    }

prrrDBInstance :: Lens' PromoteReadReplicaResult (Maybe DBInstance)
prrrDBInstance = lens _prrrDBInstance (\s a -> s { _prrrDBInstance = a })

instance AWSRequest PromoteReadReplicaMessage where
    type Sv PromoteReadReplicaMessage = RDS
    type Rs PromoteReadReplicaMessage = PromoteReadReplicaResult

    request  = post "PromoteReadReplica"
    response = const . xmlResponse $ \h x -> PromoteReadReplicaResult
newtype
