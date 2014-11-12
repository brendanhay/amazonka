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
      RebootDBInstanceMessage
    -- ** Request constructor
    , rebootDBInstance
    -- ** Request lenses
    , rdbimDBInstanceIdentifier
    , rdbimForceFailover

    -- * Response
    , RebootDBInstanceResult
    -- ** Response constructor
    , rebootDBInstanceResponse
    -- ** Response lenses
    , rdbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RebootDBInstanceMessage = RebootDBInstanceMessage
    { _rdbimDBInstanceIdentifier :: Text
    , _rdbimForceFailover        :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'RebootDBInstanceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbimDBInstanceIdentifier' @::@ 'Text'
--
-- * 'rdbimForceFailover' @::@ 'Maybe' 'Bool'
--
rebootDBInstance :: Text -- ^ 'rdbimDBInstanceIdentifier'
                 -> RebootDBInstanceMessage
rebootDBInstance p1 = RebootDBInstanceMessage
    { _rdbimDBInstanceIdentifier = p1
    , _rdbimForceFailover        = Nothing
    }

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string. Constraints: Must contain from 1 to 63 alphanumeric characters or
-- hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens.
rdbimDBInstanceIdentifier :: Lens' RebootDBInstanceMessage Text
rdbimDBInstanceIdentifier =
    lens _rdbimDBInstanceIdentifier
        (\s a -> s { _rdbimDBInstanceIdentifier = a })

-- | When true, the reboot will be conducted through a MultiAZ failover.
-- Constraint: You cannot specify true if the instance is not configured for
-- MultiAZ.
rdbimForceFailover :: Lens' RebootDBInstanceMessage (Maybe Bool)
rdbimForceFailover =
    lens _rdbimForceFailover (\s a -> s { _rdbimForceFailover = a })

instance ToQuery RebootDBInstanceMessage

instance ToPath RebootDBInstanceMessage where
    toPath = const "/"

newtype RebootDBInstanceResult = RebootDBInstanceResult
    { _rdbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'RebootDBInstanceResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
rebootDBInstanceResponse :: RebootDBInstanceResult
rebootDBInstanceResponse = RebootDBInstanceResult
    { _rdbirDBInstance = Nothing
    }

rdbirDBInstance :: Lens' RebootDBInstanceResult (Maybe DBInstance)
rdbirDBInstance = lens _rdbirDBInstance (\s a -> s { _rdbirDBInstance = a })

instance FromXML RebootDBInstanceResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RebootDBInstanceResult"

instance AWSRequest RebootDBInstanceMessage where
    type Sv RebootDBInstanceMessage = RDS
    type Rs RebootDBInstanceMessage = RebootDBInstanceResult

    request  = post "RebootDBInstance"
    response = xmlResponse $ \h x -> RebootDBInstanceResult
        <$> x %| "DBInstance"
