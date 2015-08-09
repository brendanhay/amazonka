{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RebootDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rebooting a DB instance restarts the database engine service. A reboot
-- also applies to the DB instance any modifications to the associated DB
-- parameter group that were pending. Rebooting a DB instance results in a
-- momentary outage of the instance, during which the DB instance status is
-- set to rebooting. If the RDS instance is configured for MultiAZ, it is
-- possible that the reboot will be conducted through a failover. An Amazon
-- RDS event is created when the reboot is completed.
--
-- If your DB instance is deployed in multiple Availability Zones, you can
-- force a failover from one AZ to the other during the reboot. You might
-- force a failover to test the availability of your DB instance deployment
-- or to restore operations to the original AZ after a failover occurs.
--
-- The time required to reboot is a function of the specific database
-- engine\'s crash recovery process. To improve the reboot time, we
-- recommend that you reduce database activities as much as possible during
-- the reboot process to reduce rollback activity for in-transit
-- transactions.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RebootDBInstance.html AWS API Reference> for RebootDBInstance.
module Network.AWS.RDS.RebootDBInstance
    (
    -- * Creating a Request
      RebootDBInstance
    , rebootDBInstance
    -- * Request Lenses
    , rdiForceFailover
    , rdiDBInstanceIdentifier

    -- * Destructuring the Response
    , RebootDBInstanceResponse
    , rebootDBInstanceResponse
    -- * Response Lenses
    , rdirsDBInstance
    , rdirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'rebootDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdiForceFailover'
--
-- * 'rdiDBInstanceIdentifier'
data RebootDBInstance = RebootDBInstance'
    { _rdiForceFailover        :: !(Maybe Bool)
    , _rdiDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootDBInstance' smart constructor.
rebootDBInstance :: Text -> RebootDBInstance
rebootDBInstance pDBInstanceIdentifier_ =
    RebootDBInstance'
    { _rdiForceFailover = Nothing
    , _rdiDBInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | When @true@, the reboot will be conducted through a MultiAZ failover.
--
-- Constraint: You cannot specify @true@ if the instance is not configured
-- for MultiAZ.
rdiForceFailover :: Lens' RebootDBInstance (Maybe Bool)
rdiForceFailover = lens _rdiForceFailover (\ s a -> s{_rdiForceFailover = a});

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rdiDBInstanceIdentifier :: Lens' RebootDBInstance Text
rdiDBInstanceIdentifier = lens _rdiDBInstanceIdentifier (\ s a -> s{_rdiDBInstanceIdentifier = a});

instance AWSRequest RebootDBInstance where
        type Sv RebootDBInstance = RDS
        type Rs RebootDBInstance = RebootDBInstanceResponse
        request = postQuery
        response
          = receiveXMLWrapper "RebootDBInstanceResult"
              (\ s h x ->
                 RebootDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance ToHeaders RebootDBInstance where
        toHeaders = const mempty

instance ToPath RebootDBInstance where
        toPath = const "/"

instance ToQuery RebootDBInstance where
        toQuery RebootDBInstance'{..}
          = mconcat
              ["Action" =: ("RebootDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ForceFailover" =: _rdiForceFailover,
               "DBInstanceIdentifier" =: _rdiDBInstanceIdentifier]

-- | /See:/ 'rebootDBInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdirsDBInstance'
--
-- * 'rdirsStatus'
data RebootDBInstanceResponse = RebootDBInstanceResponse'
    { _rdirsDBInstance :: !(Maybe DBInstance)
    , _rdirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootDBInstanceResponse' smart constructor.
rebootDBInstanceResponse :: Int -> RebootDBInstanceResponse
rebootDBInstanceResponse pStatus_ =
    RebootDBInstanceResponse'
    { _rdirsDBInstance = Nothing
    , _rdirsStatus = pStatus_
    }

-- | Undocumented member.
rdirsDBInstance :: Lens' RebootDBInstanceResponse (Maybe DBInstance)
rdirsDBInstance = lens _rdirsDBInstance (\ s a -> s{_rdirsDBInstance = a});

-- | Undocumented member.
rdirsStatus :: Lens' RebootDBInstanceResponse Int
rdirsStatus = lens _rdirsStatus (\ s a -> s{_rdirsStatus = a});
