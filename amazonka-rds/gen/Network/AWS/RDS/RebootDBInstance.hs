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
-- Module      : Network.AWS.RDS.RebootDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You might need to reboot your DB instance, usually for maintenance reasons. For example, if you make certain modifications, or if you change the DB parameter group associated with the DB instance, you must reboot the instance for the changes to take effect.
--
--
-- Rebooting a DB instance restarts the database engine service. Rebooting a DB instance results in a momentary outage, during which the DB instance status is set to rebooting.
--
-- For more information about rebooting, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_RebootInstance.html Rebooting a DB Instance> .
--
module Network.AWS.RDS.RebootDBInstance
    (
    -- * Creating a Request
      rebootDBInstance
    , RebootDBInstance
    -- * Request Lenses
    , rdiForceFailover
    , rdiDBInstanceIdentifier

    -- * Destructuring the Response
    , rebootDBInstanceResponse
    , RebootDBInstanceResponse
    -- * Response Lenses
    , rdirsDBInstance
    , rdirsResponseStatus
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
-- /See:/ 'rebootDBInstance' smart constructor.
data RebootDBInstance = RebootDBInstance'
  { _rdiForceFailover        :: !(Maybe Bool)
  , _rdiDBInstanceIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdiForceFailover' - When @true@ , the reboot is conducted through a MultiAZ failover.  Constraint: You can't specify @true@ if the instance is not configured for MultiAZ.
--
-- * 'rdiDBInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must match the identifier of an existing DBInstance.
rebootDBInstance
    :: Text -- ^ 'rdiDBInstanceIdentifier'
    -> RebootDBInstance
rebootDBInstance pDBInstanceIdentifier_ =
  RebootDBInstance'
    { _rdiForceFailover = Nothing
    , _rdiDBInstanceIdentifier = pDBInstanceIdentifier_
    }


-- | When @true@ , the reboot is conducted through a MultiAZ failover.  Constraint: You can't specify @true@ if the instance is not configured for MultiAZ.
rdiForceFailover :: Lens' RebootDBInstance (Maybe Bool)
rdiForceFailover = lens _rdiForceFailover (\ s a -> s{_rdiForceFailover = a})

-- | The DB instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must match the identifier of an existing DBInstance.
rdiDBInstanceIdentifier :: Lens' RebootDBInstance Text
rdiDBInstanceIdentifier = lens _rdiDBInstanceIdentifier (\ s a -> s{_rdiDBInstanceIdentifier = a})

instance AWSRequest RebootDBInstance where
        type Rs RebootDBInstance = RebootDBInstanceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "RebootDBInstanceResult"
              (\ s h x ->
                 RebootDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable RebootDBInstance where

instance NFData RebootDBInstance where

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
data RebootDBInstanceResponse = RebootDBInstanceResponse'
  { _rdirsDBInstance     :: !(Maybe DBInstance)
  , _rdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdirsDBInstance' - Undocumented member.
--
-- * 'rdirsResponseStatus' - -- | The response status code.
rebootDBInstanceResponse
    :: Int -- ^ 'rdirsResponseStatus'
    -> RebootDBInstanceResponse
rebootDBInstanceResponse pResponseStatus_ =
  RebootDBInstanceResponse'
    {_rdirsDBInstance = Nothing, _rdirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rdirsDBInstance :: Lens' RebootDBInstanceResponse (Maybe DBInstance)
rdirsDBInstance = lens _rdirsDBInstance (\ s a -> s{_rdirsDBInstance = a})

-- | -- | The response status code.
rdirsResponseStatus :: Lens' RebootDBInstanceResponse Int
rdirsResponseStatus = lens _rdirsResponseStatus (\ s a -> s{_rdirsResponseStatus = a})

instance NFData RebootDBInstanceResponse where
