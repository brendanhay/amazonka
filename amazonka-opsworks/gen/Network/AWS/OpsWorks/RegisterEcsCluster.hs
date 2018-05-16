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
-- Module      : Network.AWS.OpsWorks.RegisterEcsCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a specified Amazon ECS cluster with a stack. You can register only one cluster with a stack. A cluster can be registered with only one stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.RegisterEcsCluster
    (
    -- * Creating a Request
      registerEcsCluster
    , RegisterEcsCluster
    -- * Request Lenses
    , recEcsClusterARN
    , recStackId

    -- * Destructuring the Response
    , registerEcsClusterResponse
    , RegisterEcsClusterResponse
    -- * Response Lenses
    , recrsEcsClusterARN
    , recrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerEcsCluster' smart constructor.
data RegisterEcsCluster = RegisterEcsCluster'
  { _recEcsClusterARN :: !Text
  , _recStackId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterEcsCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'recEcsClusterARN' - The cluster's ARN.
--
-- * 'recStackId' - The stack ID.
registerEcsCluster
    :: Text -- ^ 'recEcsClusterARN'
    -> Text -- ^ 'recStackId'
    -> RegisterEcsCluster
registerEcsCluster pEcsClusterARN_ pStackId_ =
  RegisterEcsCluster'
    {_recEcsClusterARN = pEcsClusterARN_, _recStackId = pStackId_}


-- | The cluster's ARN.
recEcsClusterARN :: Lens' RegisterEcsCluster Text
recEcsClusterARN = lens _recEcsClusterARN (\ s a -> s{_recEcsClusterARN = a})

-- | The stack ID.
recStackId :: Lens' RegisterEcsCluster Text
recStackId = lens _recStackId (\ s a -> s{_recStackId = a})

instance AWSRequest RegisterEcsCluster where
        type Rs RegisterEcsCluster =
             RegisterEcsClusterResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 RegisterEcsClusterResponse' <$>
                   (x .?> "EcsClusterArn") <*> (pure (fromEnum s)))

instance Hashable RegisterEcsCluster where

instance NFData RegisterEcsCluster where

instance ToHeaders RegisterEcsCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterEcsCluster" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterEcsCluster where
        toJSON RegisterEcsCluster'{..}
          = object
              (catMaybes
                 [Just ("EcsClusterArn" .= _recEcsClusterARN),
                  Just ("StackId" .= _recStackId)])

instance ToPath RegisterEcsCluster where
        toPath = const "/"

instance ToQuery RegisterEcsCluster where
        toQuery = const mempty

-- | Contains the response to a @RegisterEcsCluster@ request.
--
--
--
-- /See:/ 'registerEcsClusterResponse' smart constructor.
data RegisterEcsClusterResponse = RegisterEcsClusterResponse'
  { _recrsEcsClusterARN  :: !(Maybe Text)
  , _recrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterEcsClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'recrsEcsClusterARN' - The cluster's ARN.
--
-- * 'recrsResponseStatus' - -- | The response status code.
registerEcsClusterResponse
    :: Int -- ^ 'recrsResponseStatus'
    -> RegisterEcsClusterResponse
registerEcsClusterResponse pResponseStatus_ =
  RegisterEcsClusterResponse'
    {_recrsEcsClusterARN = Nothing, _recrsResponseStatus = pResponseStatus_}


-- | The cluster's ARN.
recrsEcsClusterARN :: Lens' RegisterEcsClusterResponse (Maybe Text)
recrsEcsClusterARN = lens _recrsEcsClusterARN (\ s a -> s{_recrsEcsClusterARN = a})

-- | -- | The response status code.
recrsResponseStatus :: Lens' RegisterEcsClusterResponse Int
recrsResponseStatus = lens _recrsResponseStatus (\ s a -> s{_recrsResponseStatus = a})

instance NFData RegisterEcsClusterResponse where
