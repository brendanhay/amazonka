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
-- Module      : Network.AWS.OpsWorks.UpdateRDSDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon RDS instance.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateRDSDBInstance
    (
    -- * Creating a Request
      updateRDSDBInstance
    , UpdateRDSDBInstance
    -- * Request Lenses
    , urdiDBUser
    , urdiDBPassword
    , urdiRDSDBInstanceARN

    -- * Destructuring the Response
    , updateRDSDBInstanceResponse
    , UpdateRDSDBInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRDSDBInstance' smart constructor.
data UpdateRDSDBInstance = UpdateRDSDBInstance'
  { _urdiDBUser           :: !(Maybe Text)
  , _urdiDBPassword       :: !(Maybe Text)
  , _urdiRDSDBInstanceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRDSDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdiDBUser' - The master user name.
--
-- * 'urdiDBPassword' - The database password.
--
-- * 'urdiRDSDBInstanceARN' - The Amazon RDS instance's ARN.
updateRDSDBInstance
    :: Text -- ^ 'urdiRDSDBInstanceARN'
    -> UpdateRDSDBInstance
updateRDSDBInstance pRDSDBInstanceARN_ =
  UpdateRDSDBInstance'
    { _urdiDBUser = Nothing
    , _urdiDBPassword = Nothing
    , _urdiRDSDBInstanceARN = pRDSDBInstanceARN_
    }


-- | The master user name.
urdiDBUser :: Lens' UpdateRDSDBInstance (Maybe Text)
urdiDBUser = lens _urdiDBUser (\ s a -> s{_urdiDBUser = a})

-- | The database password.
urdiDBPassword :: Lens' UpdateRDSDBInstance (Maybe Text)
urdiDBPassword = lens _urdiDBPassword (\ s a -> s{_urdiDBPassword = a})

-- | The Amazon RDS instance's ARN.
urdiRDSDBInstanceARN :: Lens' UpdateRDSDBInstance Text
urdiRDSDBInstanceARN = lens _urdiRDSDBInstanceARN (\ s a -> s{_urdiRDSDBInstanceARN = a})

instance AWSRequest UpdateRDSDBInstance where
        type Rs UpdateRDSDBInstance =
             UpdateRDSDBInstanceResponse
        request = postJSON opsWorks
        response = receiveNull UpdateRDSDBInstanceResponse'

instance Hashable UpdateRDSDBInstance where

instance NFData UpdateRDSDBInstance where

instance ToHeaders UpdateRDSDBInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateRdsDbInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRDSDBInstance where
        toJSON UpdateRDSDBInstance'{..}
          = object
              (catMaybes
                 [("DbUser" .=) <$> _urdiDBUser,
                  ("DbPassword" .=) <$> _urdiDBPassword,
                  Just ("RdsDbInstanceArn" .= _urdiRDSDBInstanceARN)])

instance ToPath UpdateRDSDBInstance where
        toPath = const "/"

instance ToQuery UpdateRDSDBInstance where
        toQuery = const mempty

-- | /See:/ 'updateRDSDBInstanceResponse' smart constructor.
data UpdateRDSDBInstanceResponse =
  UpdateRDSDBInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRDSDBInstanceResponse' with the minimum fields required to make a request.
--
updateRDSDBInstanceResponse
    :: UpdateRDSDBInstanceResponse
updateRDSDBInstanceResponse = UpdateRDSDBInstanceResponse'


instance NFData UpdateRDSDBInstanceResponse where
