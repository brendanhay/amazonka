{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.OpsWorks.RegisterRDSDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Registers an Amazon RDS instance with a stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterRDSDBInstance.html>
module Network.AWS.OpsWorks.RegisterRDSDBInstance
    (
    -- * Request
      RegisterRDSDBInstance
    -- ** Request constructor
    , registerRDSDBInstance
    -- ** Request lenses
    , rrdiStackId
    , rrdiRDSDBInstanceARN
    , rrdiDBUser
    , rrdiDBPassword

    -- * Response
    , RegisterRDSDBInstanceResponse
    -- ** Response constructor
    , registerRDSDBInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerRDSDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrdiStackId'
--
-- * 'rrdiRDSDBInstanceARN'
--
-- * 'rrdiDBUser'
--
-- * 'rrdiDBPassword'
data RegisterRDSDBInstance = RegisterRDSDBInstance'
    { _rrdiStackId          :: !Text
    , _rrdiRDSDBInstanceARN :: !Text
    , _rrdiDBUser           :: !Text
    , _rrdiDBPassword       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterRDSDBInstance' smart constructor.
registerRDSDBInstance :: Text -> Text -> Text -> Text -> RegisterRDSDBInstance
registerRDSDBInstance pStackId pRDSDBInstanceARN pDBUser pDBPassword =
    RegisterRDSDBInstance'
    { _rrdiStackId = pStackId
    , _rrdiRDSDBInstanceARN = pRDSDBInstanceARN
    , _rrdiDBUser = pDBUser
    , _rrdiDBPassword = pDBPassword
    }

-- | The stack ID.
rrdiStackId :: Lens' RegisterRDSDBInstance Text
rrdiStackId = lens _rrdiStackId (\ s a -> s{_rrdiStackId = a});

-- | The Amazon RDS instance\'s ARN.
rrdiRDSDBInstanceARN :: Lens' RegisterRDSDBInstance Text
rrdiRDSDBInstanceARN = lens _rrdiRDSDBInstanceARN (\ s a -> s{_rrdiRDSDBInstanceARN = a});

-- | The database\'s master user name.
rrdiDBUser :: Lens' RegisterRDSDBInstance Text
rrdiDBUser = lens _rrdiDBUser (\ s a -> s{_rrdiDBUser = a});

-- | The database password.
rrdiDBPassword :: Lens' RegisterRDSDBInstance Text
rrdiDBPassword = lens _rrdiDBPassword (\ s a -> s{_rrdiDBPassword = a});

instance AWSRequest RegisterRDSDBInstance where
        type Sv RegisterRDSDBInstance = OpsWorks
        type Rs RegisterRDSDBInstance =
             RegisterRDSDBInstanceResponse
        request = postJSON
        response = receiveNull RegisterRDSDBInstanceResponse'

instance ToHeaders RegisterRDSDBInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterRDSDBInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterRDSDBInstance where
        toJSON RegisterRDSDBInstance'{..}
          = object
              ["StackId" .= _rrdiStackId,
               "RdsDbInstanceArn" .= _rrdiRDSDBInstanceARN,
               "DbUser" .= _rrdiDBUser,
               "DbPassword" .= _rrdiDBPassword]

instance ToPath RegisterRDSDBInstance where
        toPath = const "/"

instance ToQuery RegisterRDSDBInstance where
        toQuery = const mempty

-- | /See:/ 'registerRDSDBInstanceResponse' smart constructor.
data RegisterRDSDBInstanceResponse =
    RegisterRDSDBInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterRDSDBInstanceResponse' smart constructor.
registerRDSDBInstanceResponse :: RegisterRDSDBInstanceResponse
registerRDSDBInstanceResponse = RegisterRDSDBInstanceResponse'
