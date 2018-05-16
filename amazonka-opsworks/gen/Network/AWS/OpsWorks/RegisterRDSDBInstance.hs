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
-- Module      : Network.AWS.OpsWorks.RegisterRDSDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon RDS instance with a stack.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.RegisterRDSDBInstance
    (
    -- * Creating a Request
      registerRDSDBInstance
    , RegisterRDSDBInstance
    -- * Request Lenses
    , rrdiStackId
    , rrdiRDSDBInstanceARN
    , rrdiDBUser
    , rrdiDBPassword

    -- * Destructuring the Response
    , registerRDSDBInstanceResponse
    , RegisterRDSDBInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerRDSDBInstance' smart constructor.
data RegisterRDSDBInstance = RegisterRDSDBInstance'
  { _rrdiStackId          :: !Text
  , _rrdiRDSDBInstanceARN :: !Text
  , _rrdiDBUser           :: !Text
  , _rrdiDBPassword       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterRDSDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrdiStackId' - The stack ID.
--
-- * 'rrdiRDSDBInstanceARN' - The Amazon RDS instance's ARN.
--
-- * 'rrdiDBUser' - The database's master user name.
--
-- * 'rrdiDBPassword' - The database password.
registerRDSDBInstance
    :: Text -- ^ 'rrdiStackId'
    -> Text -- ^ 'rrdiRDSDBInstanceARN'
    -> Text -- ^ 'rrdiDBUser'
    -> Text -- ^ 'rrdiDBPassword'
    -> RegisterRDSDBInstance
registerRDSDBInstance pStackId_ pRDSDBInstanceARN_ pDBUser_ pDBPassword_ =
  RegisterRDSDBInstance'
    { _rrdiStackId = pStackId_
    , _rrdiRDSDBInstanceARN = pRDSDBInstanceARN_
    , _rrdiDBUser = pDBUser_
    , _rrdiDBPassword = pDBPassword_
    }


-- | The stack ID.
rrdiStackId :: Lens' RegisterRDSDBInstance Text
rrdiStackId = lens _rrdiStackId (\ s a -> s{_rrdiStackId = a})

-- | The Amazon RDS instance's ARN.
rrdiRDSDBInstanceARN :: Lens' RegisterRDSDBInstance Text
rrdiRDSDBInstanceARN = lens _rrdiRDSDBInstanceARN (\ s a -> s{_rrdiRDSDBInstanceARN = a})

-- | The database's master user name.
rrdiDBUser :: Lens' RegisterRDSDBInstance Text
rrdiDBUser = lens _rrdiDBUser (\ s a -> s{_rrdiDBUser = a})

-- | The database password.
rrdiDBPassword :: Lens' RegisterRDSDBInstance Text
rrdiDBPassword = lens _rrdiDBPassword (\ s a -> s{_rrdiDBPassword = a})

instance AWSRequest RegisterRDSDBInstance where
        type Rs RegisterRDSDBInstance =
             RegisterRDSDBInstanceResponse
        request = postJSON opsWorks
        response = receiveNull RegisterRDSDBInstanceResponse'

instance Hashable RegisterRDSDBInstance where

instance NFData RegisterRDSDBInstance where

instance ToHeaders RegisterRDSDBInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterRdsDbInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterRDSDBInstance where
        toJSON RegisterRDSDBInstance'{..}
          = object
              (catMaybes
                 [Just ("StackId" .= _rrdiStackId),
                  Just ("RdsDbInstanceArn" .= _rrdiRDSDBInstanceARN),
                  Just ("DbUser" .= _rrdiDBUser),
                  Just ("DbPassword" .= _rrdiDBPassword)])

instance ToPath RegisterRDSDBInstance where
        toPath = const "/"

instance ToQuery RegisterRDSDBInstance where
        toQuery = const mempty

-- | /See:/ 'registerRDSDBInstanceResponse' smart constructor.
data RegisterRDSDBInstanceResponse =
  RegisterRDSDBInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterRDSDBInstanceResponse' with the minimum fields required to make a request.
--
registerRDSDBInstanceResponse
    :: RegisterRDSDBInstanceResponse
registerRDSDBInstanceResponse = RegisterRDSDBInstanceResponse'


instance NFData RegisterRDSDBInstanceResponse where
