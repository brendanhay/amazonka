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
-- Module      : Network.AWS.RDS.StopDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a DB instance. When you stop a DB instance, Amazon RDS retains the DB instance's metadata, including its endpoint, DB parameter group, and option group membership. Amazon RDS also retains the transaction logs so you can do a point-in-time restore if necessary. For more information, see Stopping and Starting a DB instance in the AWS RDS user guide.
--
--
module Network.AWS.RDS.StopDBInstance
    (
    -- * Creating a Request
      stopDBInstance
    , StopDBInstance
    -- * Request Lenses
    , sdiDBSnapshotIdentifier
    , sdiDBInstanceIdentifier

    -- * Destructuring the Response
    , stopDBInstanceResponse
    , StopDBInstanceResponse
    -- * Response Lenses
    , sdirsDBInstance
    , sdirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopDBInstance' smart constructor.
data StopDBInstance = StopDBInstance'
  { _sdiDBSnapshotIdentifier :: !(Maybe Text)
  , _sdiDBInstanceIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiDBSnapshotIdentifier' - The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped.
--
-- * 'sdiDBInstanceIdentifier' - The user-supplied instance identifier.
stopDBInstance
    :: Text -- ^ 'sdiDBInstanceIdentifier'
    -> StopDBInstance
stopDBInstance pDBInstanceIdentifier_ =
  StopDBInstance'
    { _sdiDBSnapshotIdentifier = Nothing
    , _sdiDBInstanceIdentifier = pDBInstanceIdentifier_
    }


-- | The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped.
sdiDBSnapshotIdentifier :: Lens' StopDBInstance (Maybe Text)
sdiDBSnapshotIdentifier = lens _sdiDBSnapshotIdentifier (\ s a -> s{_sdiDBSnapshotIdentifier = a})

-- | The user-supplied instance identifier.
sdiDBInstanceIdentifier :: Lens' StopDBInstance Text
sdiDBInstanceIdentifier = lens _sdiDBInstanceIdentifier (\ s a -> s{_sdiDBInstanceIdentifier = a})

instance AWSRequest StopDBInstance where
        type Rs StopDBInstance = StopDBInstanceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "StopDBInstanceResult"
              (\ s h x ->
                 StopDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable StopDBInstance where

instance NFData StopDBInstance where

instance ToHeaders StopDBInstance where
        toHeaders = const mempty

instance ToPath StopDBInstance where
        toPath = const "/"

instance ToQuery StopDBInstance where
        toQuery StopDBInstance'{..}
          = mconcat
              ["Action" =: ("StopDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSnapshotIdentifier" =: _sdiDBSnapshotIdentifier,
               "DBInstanceIdentifier" =: _sdiDBInstanceIdentifier]

-- | /See:/ 'stopDBInstanceResponse' smart constructor.
data StopDBInstanceResponse = StopDBInstanceResponse'
  { _sdirsDBInstance     :: !(Maybe DBInstance)
  , _sdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdirsDBInstance' - Undocumented member.
--
-- * 'sdirsResponseStatus' - -- | The response status code.
stopDBInstanceResponse
    :: Int -- ^ 'sdirsResponseStatus'
    -> StopDBInstanceResponse
stopDBInstanceResponse pResponseStatus_ =
  StopDBInstanceResponse'
    {_sdirsDBInstance = Nothing, _sdirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
sdirsDBInstance :: Lens' StopDBInstanceResponse (Maybe DBInstance)
sdirsDBInstance = lens _sdirsDBInstance (\ s a -> s{_sdirsDBInstance = a})

-- | -- | The response status code.
sdirsResponseStatus :: Lens' StopDBInstanceResponse Int
sdirsResponseStatus = lens _sdirsResponseStatus (\ s a -> s{_sdirsResponseStatus = a})

instance NFData StopDBInstanceResponse where
