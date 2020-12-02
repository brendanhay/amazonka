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
-- Module      : Network.AWS.OpsWorks.DeregisterEcsCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Amazon ECS cluster from a stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html#workinglayers-ecscluster-delete Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html> .
--
module Network.AWS.OpsWorks.DeregisterEcsCluster
    (
    -- * Creating a Request
      deregisterEcsCluster
    , DeregisterEcsCluster
    -- * Request Lenses
    , decEcsClusterARN

    -- * Destructuring the Response
    , deregisterEcsClusterResponse
    , DeregisterEcsClusterResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterEcsCluster' smart constructor.
newtype DeregisterEcsCluster = DeregisterEcsCluster'
  { _decEcsClusterARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterEcsCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decEcsClusterARN' - The cluster's ARN.
deregisterEcsCluster
    :: Text -- ^ 'decEcsClusterARN'
    -> DeregisterEcsCluster
deregisterEcsCluster pEcsClusterARN_ =
  DeregisterEcsCluster' {_decEcsClusterARN = pEcsClusterARN_}


-- | The cluster's ARN.
decEcsClusterARN :: Lens' DeregisterEcsCluster Text
decEcsClusterARN = lens _decEcsClusterARN (\ s a -> s{_decEcsClusterARN = a})

instance AWSRequest DeregisterEcsCluster where
        type Rs DeregisterEcsCluster =
             DeregisterEcsClusterResponse
        request = postJSON opsWorks
        response = receiveNull DeregisterEcsClusterResponse'

instance Hashable DeregisterEcsCluster where

instance NFData DeregisterEcsCluster where

instance ToHeaders DeregisterEcsCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterEcsCluster" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterEcsCluster where
        toJSON DeregisterEcsCluster'{..}
          = object
              (catMaybes
                 [Just ("EcsClusterArn" .= _decEcsClusterARN)])

instance ToPath DeregisterEcsCluster where
        toPath = const "/"

instance ToQuery DeregisterEcsCluster where
        toQuery = const mempty

-- | /See:/ 'deregisterEcsClusterResponse' smart constructor.
data DeregisterEcsClusterResponse =
  DeregisterEcsClusterResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterEcsClusterResponse' with the minimum fields required to make a request.
--
deregisterEcsClusterResponse
    :: DeregisterEcsClusterResponse
deregisterEcsClusterResponse = DeregisterEcsClusterResponse'


instance NFData DeregisterEcsClusterResponse where
