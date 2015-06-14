{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.RegisterContainerInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action is only used by the Amazon EC2 Container Service agent, and
-- it is not intended for use outside of the agent.
--
-- Registers an Amazon EC2 instance into the specified cluster. This
-- instance will become available to place containers on.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RegisterContainerInstance.html>
module Network.AWS.ECS.RegisterContainerInstance
    (
    -- * Request
      RegisterContainerInstance
    -- ** Request constructor
    , registerContainerInstance
    -- ** Request lenses
    , rciInstanceIdentityDocumentSignature
    , rciCluster
    , rciInstanceIdentityDocument
    , rciVersionInfo
    , rciTotalResources

    -- * Response
    , RegisterContainerInstanceResponse
    -- ** Response constructor
    , registerContainerInstanceResponse
    -- ** Response lenses
    , rcirContainerInstance
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'registerContainerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rciInstanceIdentityDocumentSignature'
--
-- * 'rciCluster'
--
-- * 'rciInstanceIdentityDocument'
--
-- * 'rciVersionInfo'
--
-- * 'rciTotalResources'
data RegisterContainerInstance = RegisterContainerInstance'{_rciInstanceIdentityDocumentSignature :: Maybe Text, _rciCluster :: Maybe Text, _rciInstanceIdentityDocument :: Maybe Text, _rciVersionInfo :: Maybe VersionInfo, _rciTotalResources :: Maybe [Resource]} deriving (Eq, Read, Show)

-- | 'RegisterContainerInstance' smart constructor.
registerContainerInstance :: RegisterContainerInstance
registerContainerInstance = RegisterContainerInstance'{_rciInstanceIdentityDocumentSignature = Nothing, _rciCluster = Nothing, _rciInstanceIdentityDocument = Nothing, _rciVersionInfo = Nothing, _rciTotalResources = Nothing};

-- | FIXME: Undocumented member.
rciInstanceIdentityDocumentSignature :: Lens' RegisterContainerInstance (Maybe Text)
rciInstanceIdentityDocumentSignature = lens _rciInstanceIdentityDocumentSignature (\ s a -> s{_rciInstanceIdentityDocumentSignature = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to register your container instance with. If you do not specify
-- a cluster, the default cluster is assumed..
rciCluster :: Lens' RegisterContainerInstance (Maybe Text)
rciCluster = lens _rciCluster (\ s a -> s{_rciCluster = a});

-- | FIXME: Undocumented member.
rciInstanceIdentityDocument :: Lens' RegisterContainerInstance (Maybe Text)
rciInstanceIdentityDocument = lens _rciInstanceIdentityDocument (\ s a -> s{_rciInstanceIdentityDocument = a});

-- | FIXME: Undocumented member.
rciVersionInfo :: Lens' RegisterContainerInstance (Maybe VersionInfo)
rciVersionInfo = lens _rciVersionInfo (\ s a -> s{_rciVersionInfo = a});

-- | FIXME: Undocumented member.
rciTotalResources :: Lens' RegisterContainerInstance (Maybe [Resource])
rciTotalResources = lens _rciTotalResources (\ s a -> s{_rciTotalResources = a});

instance AWSRequest RegisterContainerInstance where
        type Sv RegisterContainerInstance = ECS
        type Rs RegisterContainerInstance =
             RegisterContainerInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterContainerInstanceResponse' <$>
                   x .?> "containerInstance")

instance ToHeaders RegisterContainerInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.RegisterContainerInstance"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterContainerInstance where
        toJSON RegisterContainerInstance'{..}
          = object
              ["instanceIdentityDocumentSignature" .=
                 _rciInstanceIdentityDocumentSignature,
               "cluster" .= _rciCluster,
               "instanceIdentityDocument" .=
                 _rciInstanceIdentityDocument,
               "versionInfo" .= _rciVersionInfo,
               "totalResources" .= _rciTotalResources]

instance ToPath RegisterContainerInstance where
        toPath = const "/"

instance ToQuery RegisterContainerInstance where
        toQuery = const mempty

-- | /See:/ 'registerContainerInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcirContainerInstance'
newtype RegisterContainerInstanceResponse = RegisterContainerInstanceResponse'{_rcirContainerInstance :: Maybe ContainerInstance} deriving (Eq, Read, Show)

-- | 'RegisterContainerInstanceResponse' smart constructor.
registerContainerInstanceResponse :: RegisterContainerInstanceResponse
registerContainerInstanceResponse = RegisterContainerInstanceResponse'{_rcirContainerInstance = Nothing};

-- | FIXME: Undocumented member.
rcirContainerInstance :: Lens' RegisterContainerInstanceResponse (Maybe ContainerInstance)
rcirContainerInstance = lens _rcirContainerInstance (\ s a -> s{_rcirContainerInstance = a});
