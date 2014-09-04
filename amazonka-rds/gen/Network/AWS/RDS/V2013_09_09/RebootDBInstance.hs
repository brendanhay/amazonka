{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RebootDBInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Rebooting a DB instance restarts the database engine service. A reboot also
-- applies to the DB instance any modifications to the associated DB parameter
-- group that were pending. Rebooting a DB instance results in a momentary
-- outage of the instance, during which the DB instance status is set to
-- rebooting. If the RDS instance is configured for MultiAZ, it is possible
-- that the reboot will be conducted through a failover. An Amazon RDS event
-- is created when the reboot is completed. If your DB instance is deployed in
-- multiple Availability Zones, you can force a failover from one AZ to the
-- other during the reboot. You might force a failover to test the
-- availability of your DB instance deployment or to restore operations to the
-- original AZ after a failover occurs. The time required to reboot is a
-- function of the specific database engine's crash recovery process. To
-- improve the reboot time, we recommend that you reduce database activities
-- as much as possible during the reboot process to reduce rollback activity
-- for in-transit transactions. https://rds.amazonaws.com/
-- ?Action=RebootDBInstance &DBInstanceIdentifier=simcoprod01
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A10%3A31.216Z &AWSAccessKeyId= &Signature=
-- 2011-05-23T06:07:38.831Z mysql 1 false general-public-license rebooting
-- 5.1.50 3306 simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com
-- simcoprod01 in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 us-east-1a 2011-05-23T06:06:43.110Z 10 db.m1.large
-- master 5d5df758-8503-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.RebootDBInstance
    (
    -- * Request
      RebootDBInstance
    -- ** Request constructor
    , rebootDBInstance
    -- ** Request lenses
    , rdbinDBInstanceIdentifier
    , rdbinForceFailover

    -- * Response
    , RebootDBInstanceResponse
    -- ** Response lenses
    , dbidsDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RebootDBInstance' request.
rebootDBInstance :: Text -- ^ 'rdbinDBInstanceIdentifier'
                 -> RebootDBInstance
rebootDBInstance p1 = RebootDBInstance
    { _rdbinDBInstanceIdentifier = p1
    , _rdbinForceFailover = Nothing
    }
{-# INLINE rebootDBInstance #-}

data RebootDBInstance = RebootDBInstance
    { _rdbinDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier. This parameter is stored as a
      -- lowercase string. Constraints: Must contain from 1 to 63
      -- alphanumeric characters or hyphens First character must be a
      -- letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , _rdbinForceFailover :: Maybe Bool
      -- ^ When true, the reboot will be conducted through a MultiAZ
      -- failover. Constraint: You cannot specify true if the instance is
      -- not configured for MultiAZ.
    } deriving (Show, Generic)

-- | The DB instance identifier. This parameter is stored as a lowercase string.
-- Constraints: Must contain from 1 to 63 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
rdbinDBInstanceIdentifier :: Lens' RebootDBInstance (Text)
rdbinDBInstanceIdentifier f x =
    f (_rdbinDBInstanceIdentifier x)
        <&> \y -> x { _rdbinDBInstanceIdentifier = y }
{-# INLINE rdbinDBInstanceIdentifier #-}

-- | When true, the reboot will be conducted through a MultiAZ failover.
-- Constraint: You cannot specify true if the instance is not configured for
-- MultiAZ.
rdbinForceFailover :: Lens' RebootDBInstance (Maybe Bool)
rdbinForceFailover f x =
    f (_rdbinForceFailover x)
        <&> \y -> x { _rdbinForceFailover = y }
{-# INLINE rdbinForceFailover #-}

instance ToQuery RebootDBInstance where
    toQuery = genericQuery def

data RebootDBInstanceResponse = RebootDBInstanceResponse
    { _dbidsDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
dbidsDBInstance :: Lens' RebootDBInstanceResponse (Maybe DBInstance)
dbidsDBInstance f x =
    f (_dbidsDBInstance x)
        <&> \y -> x { _dbidsDBInstance = y }
{-# INLINE dbidsDBInstance #-}

instance FromXML RebootDBInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RebootDBInstance where
    type Sv RebootDBInstance = RDS
    type Rs RebootDBInstance = RebootDBInstanceResponse

    request = post "RebootDBInstance"
    response _ = xmlResponse
