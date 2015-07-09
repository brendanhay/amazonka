{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterRDSDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deregisters an Amazon RDS instance.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterRDSDBInstance.html>
module Network.AWS.OpsWorks.DeregisterRDSDBInstance
    (
    -- * Request
      DeregisterRDSDBInstance
    -- ** Request constructor
    , deregisterRDSDBInstance
    -- ** Request lenses
    , drdiRDSDBInstanceARN

    -- * Response
    , DeregisterRDSDBInstanceResponse
    -- ** Response constructor
    , deregisterRDSDBInstanceResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterRDSDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdiRDSDBInstanceARN'
newtype DeregisterRDSDBInstance = DeregisterRDSDBInstance'
    { _drdiRDSDBInstanceARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterRDSDBInstance' smart constructor.
deregisterRDSDBInstance :: Text -> DeregisterRDSDBInstance
deregisterRDSDBInstance pRDSDBInstanceARN =
    DeregisterRDSDBInstance'
    { _drdiRDSDBInstanceARN = pRDSDBInstanceARN
    }

-- | The Amazon RDS instance\'s ARN.
drdiRDSDBInstanceARN :: Lens' DeregisterRDSDBInstance Text
drdiRDSDBInstanceARN = lens _drdiRDSDBInstanceARN (\ s a -> s{_drdiRDSDBInstanceARN = a});

instance AWSRequest DeregisterRDSDBInstance where
        type Sv DeregisterRDSDBInstance = OpsWorks
        type Rs DeregisterRDSDBInstance =
             DeregisterRDSDBInstanceResponse
        request = postJSON
        response
          = receiveNull DeregisterRDSDBInstanceResponse'

instance ToHeaders DeregisterRDSDBInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterRDSDBInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterRDSDBInstance where
        toJSON DeregisterRDSDBInstance'{..}
          = object
              ["RdsDbInstanceArn" .= _drdiRDSDBInstanceARN]

instance ToPath DeregisterRDSDBInstance where
        toPath = const "/"

instance ToQuery DeregisterRDSDBInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterRDSDBInstanceResponse' smart constructor.
data DeregisterRDSDBInstanceResponse =
    DeregisterRDSDBInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterRDSDBInstanceResponse' smart constructor.
deregisterRDSDBInstanceResponse :: DeregisterRDSDBInstanceResponse
deregisterRDSDBInstanceResponse = DeregisterRDSDBInstanceResponse'
