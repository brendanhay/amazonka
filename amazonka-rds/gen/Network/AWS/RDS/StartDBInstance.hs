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
-- Module      : Network.AWS.RDS.StartDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a DB instance that was stopped using the AWS console, the stop-db-instance AWS CLI command, or the StopDBInstance action. For more information, see Stopping and Starting a DB instance in the AWS RDS user guide.
--
--
module Network.AWS.RDS.StartDBInstance
    (
    -- * Creating a Request
      startDBInstance
    , StartDBInstance
    -- * Request Lenses
    , sdbiDBInstanceIdentifier

    -- * Destructuring the Response
    , startDBInstanceResponse
    , StartDBInstanceResponse
    -- * Response Lenses
    , sdbirsDBInstance
    , sdbirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startDBInstance' smart constructor.
newtype StartDBInstance = StartDBInstance'
  { _sdbiDBInstanceIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdbiDBInstanceIdentifier' - The user-supplied instance identifier.
startDBInstance
    :: Text -- ^ 'sdbiDBInstanceIdentifier'
    -> StartDBInstance
startDBInstance pDBInstanceIdentifier_ =
  StartDBInstance' {_sdbiDBInstanceIdentifier = pDBInstanceIdentifier_}


-- | The user-supplied instance identifier.
sdbiDBInstanceIdentifier :: Lens' StartDBInstance Text
sdbiDBInstanceIdentifier = lens _sdbiDBInstanceIdentifier (\ s a -> s{_sdbiDBInstanceIdentifier = a})

instance AWSRequest StartDBInstance where
        type Rs StartDBInstance = StartDBInstanceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "StartDBInstanceResult"
              (\ s h x ->
                 StartDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable StartDBInstance where

instance NFData StartDBInstance where

instance ToHeaders StartDBInstance where
        toHeaders = const mempty

instance ToPath StartDBInstance where
        toPath = const "/"

instance ToQuery StartDBInstance where
        toQuery StartDBInstance'{..}
          = mconcat
              ["Action" =: ("StartDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBInstanceIdentifier" =: _sdbiDBInstanceIdentifier]

-- | /See:/ 'startDBInstanceResponse' smart constructor.
data StartDBInstanceResponse = StartDBInstanceResponse'
  { _sdbirsDBInstance     :: !(Maybe DBInstance)
  , _sdbirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdbirsDBInstance' - Undocumented member.
--
-- * 'sdbirsResponseStatus' - -- | The response status code.
startDBInstanceResponse
    :: Int -- ^ 'sdbirsResponseStatus'
    -> StartDBInstanceResponse
startDBInstanceResponse pResponseStatus_ =
  StartDBInstanceResponse'
    {_sdbirsDBInstance = Nothing, _sdbirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
sdbirsDBInstance :: Lens' StartDBInstanceResponse (Maybe DBInstance)
sdbirsDBInstance = lens _sdbirsDBInstance (\ s a -> s{_sdbirsDBInstance = a})

-- | -- | The response status code.
sdbirsResponseStatus :: Lens' StartDBInstanceResponse Int
sdbirsResponseStatus = lens _sdbirsResponseStatus (\ s a -> s{_sdbirsResponseStatus = a})

instance NFData StartDBInstanceResponse where
