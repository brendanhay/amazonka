{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterRDSDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon RDS instance with a stack.
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
    , rrdirqStackId
    , rrdirqRDSDBInstanceARN
    , rrdirqDBUser
    , rrdirqDBPassword

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
-- * 'rrdirqStackId'
--
-- * 'rrdirqRDSDBInstanceARN'
--
-- * 'rrdirqDBUser'
--
-- * 'rrdirqDBPassword'
data RegisterRDSDBInstance = RegisterRDSDBInstance'
    { _rrdirqStackId          :: !Text
    , _rrdirqRDSDBInstanceARN :: !Text
    , _rrdirqDBUser           :: !Text
    , _rrdirqDBPassword       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterRDSDBInstance' smart constructor.
registerRDSDBInstance :: Text -> Text -> Text -> Text -> RegisterRDSDBInstance
registerRDSDBInstance pStackId pRDSDBInstanceARN pDBUser pDBPassword =
    RegisterRDSDBInstance'
    { _rrdirqStackId = pStackId
    , _rrdirqRDSDBInstanceARN = pRDSDBInstanceARN
    , _rrdirqDBUser = pDBUser
    , _rrdirqDBPassword = pDBPassword
    }

-- | The stack ID.
rrdirqStackId :: Lens' RegisterRDSDBInstance Text
rrdirqStackId = lens _rrdirqStackId (\ s a -> s{_rrdirqStackId = a});

-- | The Amazon RDS instance\'s ARN.
rrdirqRDSDBInstanceARN :: Lens' RegisterRDSDBInstance Text
rrdirqRDSDBInstanceARN = lens _rrdirqRDSDBInstanceARN (\ s a -> s{_rrdirqRDSDBInstanceARN = a});

-- | The database\'s master user name.
rrdirqDBUser :: Lens' RegisterRDSDBInstance Text
rrdirqDBUser = lens _rrdirqDBUser (\ s a -> s{_rrdirqDBUser = a});

-- | The database password.
rrdirqDBPassword :: Lens' RegisterRDSDBInstance Text
rrdirqDBPassword = lens _rrdirqDBPassword (\ s a -> s{_rrdirqDBPassword = a});

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
              ["StackId" .= _rrdirqStackId,
               "RdsDbInstanceArn" .= _rrdirqRDSDBInstanceARN,
               "DbUser" .= _rrdirqDBUser,
               "DbPassword" .= _rrdirqDBPassword]

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
