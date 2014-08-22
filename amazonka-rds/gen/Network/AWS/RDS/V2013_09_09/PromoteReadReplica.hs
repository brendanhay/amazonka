{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.PromoteReadReplica
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Promotes a read replica DB instance to a standalone DB instance.
-- https://rds.amazonaws.com/ ?Action=PromoteReadReplica
-- &DBInstanceIdentifier=simcoprod01 &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-08-23T08%3A02%3A09.574Z
-- &AWSAccessKeyId= &Signature= 2011-05-23T08:00:00Z mysql 50 1 false
-- general-public-license available 5.1.50 3306
-- simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com simcoprod01 in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00
-- us-east-1a 2011-05-23T06:06:43.110Z 10 db.m1.large master
-- f61a020f-8512-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.PromoteReadReplica where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PromoteReadReplica' request.
promoteReadReplica :: Text -- ^ '_prrmDBInstanceIdentifier'
                   -> PromoteReadReplica
promoteReadReplica p1 = PromoteReadReplica
    { _prrmDBInstanceIdentifier = p1
    , _prrmBackupRetentionPeriod = Nothing
    , _prrmPreferredBackupWindow = Nothing
    }

data PromoteReadReplica = PromoteReadReplica
    { _prrmDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier. This value is stored as a lowercase
      -- string. Constraints: Must be the identifier for an existing read
      -- replica DB instance Must contain from 1 to 63 alphanumeric
      -- characters or hyphens First character must be a letter Cannot end
      -- with a hyphen or contain two consecutive hyphens Example:
      -- mydbinstance.
    , _prrmBackupRetentionPeriod :: Maybe Integer
      -- ^ The number of days to retain automated backups. Setting this
      -- parameter to a positive number enables backups. Setting this
      -- parameter to 0 disables automated backups. Default: 1
      -- Constraints: Must be a value from 0 to 8.
    , _prrmPreferredBackupWindow :: Maybe Text
      -- ^ The daily time range during which automated backups are created
      -- if automated backups are enabled, using the BackupRetentionPeriod
      -- parameter. Default: A 30-minute window selected at random from an
      -- 8-hour block of time per region. See the Amazon RDS User Guide
      -- for the time blocks for each region from which the default backup
      -- windows are assigned. Constraints: Must be in the format
      -- hh24:mi-hh24:mi. Times should be Universal Time Coordinated
      -- (UTC). Must not conflict with the preferred maintenance window.
      -- Must be at least 30 minutes.
    } deriving (Show, Generic)

makeLenses ''PromoteReadReplica

instance ToQuery PromoteReadReplica where
    toQuery = genericQuery def

data PromoteReadReplicaResponse = PromoteReadReplicaResponse
    { _dbiwDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

makeLenses ''PromoteReadReplicaResponse

instance FromXML PromoteReadReplicaResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PromoteReadReplica where
    type Sv PromoteReadReplica = RDS
    type Rs PromoteReadReplica = PromoteReadReplicaResponse

    request = post "PromoteReadReplica"
    response _ = xmlResponse
