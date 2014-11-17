{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RebootDBInstance
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
-- for in-transit transactions.
module Network.AWS.RDS.RebootDBInstance
    (
    -- * Request
      RebootDBInstance
    -- ** Request constructor
    , rebootDBInstance
    -- ** Request lenses
    , rdbiDBInstanceIdentifier
    , rdbiForceFailover

    -- * Response
    , RebootDBInstanceResponse
    -- ** Response constructor
    , rebootDBInstanceResponse
    -- ** Response lenses
    , rdbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data RebootDBInstance = RebootDBInstance
    { _rdbiDBInstanceIdentifier :: Text
    , _rdbiForceFailover        :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebootDBInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbiDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbiForceFailover' @::@ 'Maybe' 'Bool'
--
rebootDBInstance :: Text -- ^ 'rdbiDBInstanceIdentifier'
                 -> RebootDBInstance
rebootDBInstance p1 = RebootDBInstance
    { _rdbiDBInstanceIdentifier = p1
    , _rdbiForceFailover        = Nothing
    }

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string. Constraints: Must contain from 1 to 63 alphanumeric characters or
-- hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens.
rdbiDBInstanceIdentifier :: Lens' RebootDBInstance Text
rdbiDBInstanceIdentifier =
    lens _rdbiDBInstanceIdentifier
        (\s a -> s { _rdbiDBInstanceIdentifier = a })

-- | When true, the reboot will be conducted through a MultiAZ failover.
-- Constraint: You cannot specify true if the instance is not configured for
-- MultiAZ.
rdbiForceFailover :: Lens' RebootDBInstance (Maybe Bool)
rdbiForceFailover =
    lens _rdbiForceFailover (\s a -> s { _rdbiForceFailover = a })

newtype RebootDBInstanceResponse = RebootDBInstanceResponse
    { _rdbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'RebootDBInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
rebootDBInstanceResponse :: RebootDBInstanceResponse
rebootDBInstanceResponse = RebootDBInstanceResponse
    { _rdbirDBInstance = Nothing
    }

rdbirDBInstance :: Lens' RebootDBInstanceResponse (Maybe DBInstance)
rdbirDBInstance = lens _rdbirDBInstance (\s a -> s { _rdbirDBInstance = a })

instance AWSRequest RebootDBInstance where
    type Sv RebootDBInstance = RDS
    type Rs RebootDBInstance = RebootDBInstanceResponse

    request  = post "RebootDBInstance"
    response = xmlResponse

instance FromXML RebootDBInstanceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebootDBInstanceResponse"

instance ToPath RebootDBInstance where
    toPath = const "/"

instance ToHeaders RebootDBInstance

instance ToQuery RebootDBInstance
