{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterEcsCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers a specified Amazon ECS cluster with a stack. You can register
-- only one cluster with a stack. A cluster can be registered with only one
-- stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterEcsCluster.html>
module Network.AWS.OpsWorks.RegisterEcsCluster
    (
    -- * Request
      RegisterEcsCluster
    -- ** Request constructor
    , registerEcsCluster
    -- ** Request lenses
    , recEcsClusterARN
    , recStackId

    -- * Response
    , RegisterEcsClusterResponse
    -- ** Response constructor
    , registerEcsClusterResponse
    -- ** Response lenses
    , recrsEcsClusterARN
    , recrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerEcsCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'recEcsClusterARN'
--
-- * 'recStackId'
data RegisterEcsCluster = RegisterEcsCluster'
    { _recEcsClusterARN :: !Text
    , _recStackId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterEcsCluster' smart constructor.
registerEcsCluster :: Text -> Text -> RegisterEcsCluster
registerEcsCluster pEcsClusterARN_ pStackId_ =
    RegisterEcsCluster'
    { _recEcsClusterARN = pEcsClusterARN_
    , _recStackId = pStackId_
    }

-- | The cluster\'s ARN.
recEcsClusterARN :: Lens' RegisterEcsCluster Text
recEcsClusterARN = lens _recEcsClusterARN (\ s a -> s{_recEcsClusterARN = a});

-- | The stack ID.
recStackId :: Lens' RegisterEcsCluster Text
recStackId = lens _recStackId (\ s a -> s{_recStackId = a});

instance AWSRequest RegisterEcsCluster where
        type Sv RegisterEcsCluster = OpsWorks
        type Rs RegisterEcsCluster =
             RegisterEcsClusterResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterEcsClusterResponse' <$>
                   (x .?> "EcsClusterArn") <*> (pure (fromEnum s)))

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
              ["EcsClusterArn" .= _recEcsClusterARN,
               "StackId" .= _recStackId]

instance ToPath RegisterEcsCluster where
        toPath = const "/"

instance ToQuery RegisterEcsCluster where
        toQuery = const mempty

-- | Contains the response to a @RegisterEcsCluster@ request.
--
-- /See:/ 'registerEcsClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'recrsEcsClusterARN'
--
-- * 'recrsStatus'
data RegisterEcsClusterResponse = RegisterEcsClusterResponse'
    { _recrsEcsClusterARN :: !(Maybe Text)
    , _recrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterEcsClusterResponse' smart constructor.
registerEcsClusterResponse :: Int -> RegisterEcsClusterResponse
registerEcsClusterResponse pStatus_ =
    RegisterEcsClusterResponse'
    { _recrsEcsClusterARN = Nothing
    , _recrsStatus = pStatus_
    }

-- | The cluster\'s ARN.
recrsEcsClusterARN :: Lens' RegisterEcsClusterResponse (Maybe Text)
recrsEcsClusterARN = lens _recrsEcsClusterARN (\ s a -> s{_recrsEcsClusterARN = a});

-- | FIXME: Undocumented member.
recrsStatus :: Lens' RegisterEcsClusterResponse Int
recrsStatus = lens _recrsStatus (\ s a -> s{_recrsStatus = a});
