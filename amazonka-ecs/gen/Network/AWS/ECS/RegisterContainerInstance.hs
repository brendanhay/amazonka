{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RegisterContainerInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon EC2 Container Service agent, and
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
    , rcirqInstanceIdentityDocumentSignature
    , rcirqCluster
    , rcirqInstanceIdentityDocument
    , rcirqContainerInstanceARN
    , rcirqVersionInfo
    , rcirqTotalResources

    -- * Response
    , RegisterContainerInstanceResponse
    -- ** Response constructor
    , registerContainerInstanceResponse
    -- ** Response lenses
    , rcirsContainerInstance
    , rcirsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerContainerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcirqInstanceIdentityDocumentSignature'
--
-- * 'rcirqCluster'
--
-- * 'rcirqInstanceIdentityDocument'
--
-- * 'rcirqContainerInstanceARN'
--
-- * 'rcirqVersionInfo'
--
-- * 'rcirqTotalResources'
data RegisterContainerInstance = RegisterContainerInstance'
    { _rcirqInstanceIdentityDocumentSignature :: !(Maybe Text)
    , _rcirqCluster                           :: !(Maybe Text)
    , _rcirqInstanceIdentityDocument          :: !(Maybe Text)
    , _rcirqContainerInstanceARN              :: !(Maybe Text)
    , _rcirqVersionInfo                       :: !(Maybe VersionInfo)
    , _rcirqTotalResources                    :: !(Maybe [Resource])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterContainerInstance' smart constructor.
registerContainerInstance :: RegisterContainerInstance
registerContainerInstance =
    RegisterContainerInstance'
    { _rcirqInstanceIdentityDocumentSignature = Nothing
    , _rcirqCluster = Nothing
    , _rcirqInstanceIdentityDocument = Nothing
    , _rcirqContainerInstanceARN = Nothing
    , _rcirqVersionInfo = Nothing
    , _rcirqTotalResources = Nothing
    }

-- | The instance identity document signature for the Amazon EC2 instance to
-- register. This signature can be found by running the following command
-- from the instance:
-- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/signature\/@
rcirqInstanceIdentityDocumentSignature :: Lens' RegisterContainerInstance (Maybe Text)
rcirqInstanceIdentityDocumentSignature = lens _rcirqInstanceIdentityDocumentSignature (\ s a -> s{_rcirqInstanceIdentityDocumentSignature = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to register your container instance with. If you do not specify
-- a cluster, the default cluster is assumed..
rcirqCluster :: Lens' RegisterContainerInstance (Maybe Text)
rcirqCluster = lens _rcirqCluster (\ s a -> s{_rcirqCluster = a});

-- | The instance identity document for the Amazon EC2 instance to register.
-- This document can be found by running the following command from the
-- instance:
-- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/document\/@
rcirqInstanceIdentityDocument :: Lens' RegisterContainerInstance (Maybe Text)
rcirqInstanceIdentityDocument = lens _rcirqInstanceIdentityDocument (\ s a -> s{_rcirqInstanceIdentityDocument = a});

-- | The Amazon Resource Name (ARN) of the container instance (if it was
-- previously registered).
rcirqContainerInstanceARN :: Lens' RegisterContainerInstance (Maybe Text)
rcirqContainerInstanceARN = lens _rcirqContainerInstanceARN (\ s a -> s{_rcirqContainerInstanceARN = a});

-- | The version information for the Amazon ECS container agent and Docker
-- daemon running on the container instance.
rcirqVersionInfo :: Lens' RegisterContainerInstance (Maybe VersionInfo)
rcirqVersionInfo = lens _rcirqVersionInfo (\ s a -> s{_rcirqVersionInfo = a});

-- | The resources available on the instance.
rcirqTotalResources :: Lens' RegisterContainerInstance [Resource]
rcirqTotalResources = lens _rcirqTotalResources (\ s a -> s{_rcirqTotalResources = a}) . _Default;

instance AWSRequest RegisterContainerInstance where
        type Sv RegisterContainerInstance = ECS
        type Rs RegisterContainerInstance =
             RegisterContainerInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterContainerInstanceResponse' <$>
                   (x .?> "containerInstance") <*> (pure (fromEnum s)))

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
                 _rcirqInstanceIdentityDocumentSignature,
               "cluster" .= _rcirqCluster,
               "instanceIdentityDocument" .=
                 _rcirqInstanceIdentityDocument,
               "containerInstanceArn" .= _rcirqContainerInstanceARN,
               "versionInfo" .= _rcirqVersionInfo,
               "totalResources" .= _rcirqTotalResources]

instance ToPath RegisterContainerInstance where
        toPath = const "/"

instance ToQuery RegisterContainerInstance where
        toQuery = const mempty

-- | /See:/ 'registerContainerInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcirsContainerInstance'
--
-- * 'rcirsStatus'
data RegisterContainerInstanceResponse = RegisterContainerInstanceResponse'
    { _rcirsContainerInstance :: !(Maybe ContainerInstance)
    , _rcirsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterContainerInstanceResponse' smart constructor.
registerContainerInstanceResponse :: Int -> RegisterContainerInstanceResponse
registerContainerInstanceResponse pStatus =
    RegisterContainerInstanceResponse'
    { _rcirsContainerInstance = Nothing
    , _rcirsStatus = pStatus
    }

-- | FIXME: Undocumented member.
rcirsContainerInstance :: Lens' RegisterContainerInstanceResponse (Maybe ContainerInstance)
rcirsContainerInstance = lens _rcirsContainerInstance (\ s a -> s{_rcirsContainerInstance = a});

-- | FIXME: Undocumented member.
rcirsStatus :: Lens' RegisterContainerInstanceResponse Int
rcirsStatus = lens _rcirsStatus (\ s a -> s{_rcirsStatus = a});
