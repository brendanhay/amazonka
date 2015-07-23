{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateRDSDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon RDS instance.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateRDSDBInstance.html>
module Network.AWS.OpsWorks.UpdateRDSDBInstance
    (
    -- * Request
      UpdateRDSDBInstance
    -- ** Request constructor
    , updateRDSDBInstance
    -- ** Request lenses
    , urdirqDBUser
    , urdirqDBPassword
    , urdirqRDSDBInstanceARN

    -- * Response
    , UpdateRDSDBInstanceResponse
    -- ** Response constructor
    , updateRDSDBInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateRDSDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urdirqDBUser'
--
-- * 'urdirqDBPassword'
--
-- * 'urdirqRDSDBInstanceARN'
data UpdateRDSDBInstance = UpdateRDSDBInstance'
    { _urdirqDBUser           :: !(Maybe Text)
    , _urdirqDBPassword       :: !(Maybe Text)
    , _urdirqRDSDBInstanceARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRDSDBInstance' smart constructor.
updateRDSDBInstance :: Text -> UpdateRDSDBInstance
updateRDSDBInstance pRDSDBInstanceARN_ =
    UpdateRDSDBInstance'
    { _urdirqDBUser = Nothing
    , _urdirqDBPassword = Nothing
    , _urdirqRDSDBInstanceARN = pRDSDBInstanceARN_
    }

-- | The master user name.
urdirqDBUser :: Lens' UpdateRDSDBInstance (Maybe Text)
urdirqDBUser = lens _urdirqDBUser (\ s a -> s{_urdirqDBUser = a});

-- | The database password.
urdirqDBPassword :: Lens' UpdateRDSDBInstance (Maybe Text)
urdirqDBPassword = lens _urdirqDBPassword (\ s a -> s{_urdirqDBPassword = a});

-- | The Amazon RDS instance\'s ARN.
urdirqRDSDBInstanceARN :: Lens' UpdateRDSDBInstance Text
urdirqRDSDBInstanceARN = lens _urdirqRDSDBInstanceARN (\ s a -> s{_urdirqRDSDBInstanceARN = a});

instance AWSRequest UpdateRDSDBInstance where
        type Sv UpdateRDSDBInstance = OpsWorks
        type Rs UpdateRDSDBInstance =
             UpdateRDSDBInstanceResponse
        request = postJSON
        response = receiveNull UpdateRDSDBInstanceResponse'

instance ToHeaders UpdateRDSDBInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateRDSDBInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRDSDBInstance where
        toJSON UpdateRDSDBInstance'{..}
          = object
              ["DbUser" .= _urdirqDBUser,
               "DbPassword" .= _urdirqDBPassword,
               "RdsDbInstanceArn" .= _urdirqRDSDBInstanceARN]

instance ToPath UpdateRDSDBInstance where
        toPath = const "/"

instance ToQuery UpdateRDSDBInstance where
        toQuery = const mempty

-- | /See:/ 'updateRDSDBInstanceResponse' smart constructor.
data UpdateRDSDBInstanceResponse =
    UpdateRDSDBInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRDSDBInstanceResponse' smart constructor.
updateRDSDBInstanceResponse :: UpdateRDSDBInstanceResponse
updateRDSDBInstanceResponse = UpdateRDSDBInstanceResponse'
