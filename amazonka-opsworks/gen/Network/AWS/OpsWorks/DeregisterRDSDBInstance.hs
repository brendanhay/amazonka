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
-- Module      : Network.AWS.OpsWorks.DeregisterRDSDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon RDS instance.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeregisterRDSDBInstance
    (
    -- * Creating a Request
      deregisterRDSDBInstance
    , DeregisterRDSDBInstance
    -- * Request Lenses
    , drdiRDSDBInstanceARN

    -- * Destructuring the Response
    , deregisterRDSDBInstanceResponse
    , DeregisterRDSDBInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterRDSDBInstance' smart constructor.
newtype DeregisterRDSDBInstance = DeregisterRDSDBInstance'
  { _drdiRDSDBInstanceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterRDSDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdiRDSDBInstanceARN' - The Amazon RDS instance's ARN.
deregisterRDSDBInstance
    :: Text -- ^ 'drdiRDSDBInstanceARN'
    -> DeregisterRDSDBInstance
deregisterRDSDBInstance pRDSDBInstanceARN_ =
  DeregisterRDSDBInstance' {_drdiRDSDBInstanceARN = pRDSDBInstanceARN_}


-- | The Amazon RDS instance's ARN.
drdiRDSDBInstanceARN :: Lens' DeregisterRDSDBInstance Text
drdiRDSDBInstanceARN = lens _drdiRDSDBInstanceARN (\ s a -> s{_drdiRDSDBInstanceARN = a})

instance AWSRequest DeregisterRDSDBInstance where
        type Rs DeregisterRDSDBInstance =
             DeregisterRDSDBInstanceResponse
        request = postJSON opsWorks
        response
          = receiveNull DeregisterRDSDBInstanceResponse'

instance Hashable DeregisterRDSDBInstance where

instance NFData DeregisterRDSDBInstance where

instance ToHeaders DeregisterRDSDBInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterRdsDbInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterRDSDBInstance where
        toJSON DeregisterRDSDBInstance'{..}
          = object
              (catMaybes
                 [Just ("RdsDbInstanceArn" .= _drdiRDSDBInstanceARN)])

instance ToPath DeregisterRDSDBInstance where
        toPath = const "/"

instance ToQuery DeregisterRDSDBInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterRDSDBInstanceResponse' smart constructor.
data DeregisterRDSDBInstanceResponse =
  DeregisterRDSDBInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterRDSDBInstanceResponse' with the minimum fields required to make a request.
--
deregisterRDSDBInstanceResponse
    :: DeregisterRDSDBInstanceResponse
deregisterRDSDBInstanceResponse = DeregisterRDSDBInstanceResponse'


instance NFData DeregisterRDSDBInstanceResponse where
