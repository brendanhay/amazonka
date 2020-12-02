{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RegisterContainerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an EC2 instance into the specified cluster. This instance becomes available to place containers on.
module Network.AWS.ECS.RegisterContainerInstance
  ( -- * Creating a Request
    registerContainerInstance,
    RegisterContainerInstance,

    -- * Request Lenses
    rciPlatformDevices,
    rciInstanceIdentityDocumentSignature,
    rciCluster,
    rciInstanceIdentityDocument,
    rciContainerInstanceARN,
    rciVersionInfo,
    rciAttributes,
    rciTotalResources,
    rciTags,

    -- * Destructuring the Response
    registerContainerInstanceResponse,
    RegisterContainerInstanceResponse,

    -- * Response Lenses
    rcirsContainerInstance,
    rcirsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerContainerInstance' smart constructor.
data RegisterContainerInstance = RegisterContainerInstance'
  { _rciPlatformDevices ::
      !(Maybe [PlatformDevice]),
    _rciInstanceIdentityDocumentSignature ::
      !(Maybe Text),
    _rciCluster :: !(Maybe Text),
    _rciInstanceIdentityDocument ::
      !(Maybe Text),
    _rciContainerInstanceARN ::
      !(Maybe Text),
    _rciVersionInfo :: !(Maybe VersionInfo),
    _rciAttributes :: !(Maybe [Attribute]),
    _rciTotalResources ::
      !(Maybe [Resource]),
    _rciTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterContainerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rciPlatformDevices' - The devices that are available on the container instance. The only supported device type is a GPU.
--
-- * 'rciInstanceIdentityDocumentSignature' - The instance identity document signature for the EC2 instance to register. This signature can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/signature/@
--
-- * 'rciCluster' - The short name or full Amazon Resource Name (ARN) of the cluster with which to register your container instance. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'rciInstanceIdentityDocument' - The instance identity document for the EC2 instance to register. This document can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/document/@
--
-- * 'rciContainerInstanceARN' - The ARN of the container instance (if it was previously registered).
--
-- * 'rciVersionInfo' - The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- * 'rciAttributes' - The container instance attributes that this container instance supports.
--
-- * 'rciTotalResources' - The resources available on the instance.
--
-- * 'rciTags' - The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
registerContainerInstance ::
  RegisterContainerInstance
registerContainerInstance =
  RegisterContainerInstance'
    { _rciPlatformDevices = Nothing,
      _rciInstanceIdentityDocumentSignature = Nothing,
      _rciCluster = Nothing,
      _rciInstanceIdentityDocument = Nothing,
      _rciContainerInstanceARN = Nothing,
      _rciVersionInfo = Nothing,
      _rciAttributes = Nothing,
      _rciTotalResources = Nothing,
      _rciTags = Nothing
    }

-- | The devices that are available on the container instance. The only supported device type is a GPU.
rciPlatformDevices :: Lens' RegisterContainerInstance [PlatformDevice]
rciPlatformDevices = lens _rciPlatformDevices (\s a -> s {_rciPlatformDevices = a}) . _Default . _Coerce

-- | The instance identity document signature for the EC2 instance to register. This signature can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/signature/@
rciInstanceIdentityDocumentSignature :: Lens' RegisterContainerInstance (Maybe Text)
rciInstanceIdentityDocumentSignature = lens _rciInstanceIdentityDocumentSignature (\s a -> s {_rciInstanceIdentityDocumentSignature = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster with which to register your container instance. If you do not specify a cluster, the default cluster is assumed.
rciCluster :: Lens' RegisterContainerInstance (Maybe Text)
rciCluster = lens _rciCluster (\s a -> s {_rciCluster = a})

-- | The instance identity document for the EC2 instance to register. This document can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/document/@
rciInstanceIdentityDocument :: Lens' RegisterContainerInstance (Maybe Text)
rciInstanceIdentityDocument = lens _rciInstanceIdentityDocument (\s a -> s {_rciInstanceIdentityDocument = a})

-- | The ARN of the container instance (if it was previously registered).
rciContainerInstanceARN :: Lens' RegisterContainerInstance (Maybe Text)
rciContainerInstanceARN = lens _rciContainerInstanceARN (\s a -> s {_rciContainerInstanceARN = a})

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
rciVersionInfo :: Lens' RegisterContainerInstance (Maybe VersionInfo)
rciVersionInfo = lens _rciVersionInfo (\s a -> s {_rciVersionInfo = a})

-- | The container instance attributes that this container instance supports.
rciAttributes :: Lens' RegisterContainerInstance [Attribute]
rciAttributes = lens _rciAttributes (\s a -> s {_rciAttributes = a}) . _Default . _Coerce

-- | The resources available on the instance.
rciTotalResources :: Lens' RegisterContainerInstance [Resource]
rciTotalResources = lens _rciTotalResources (\s a -> s {_rciTotalResources = a}) . _Default . _Coerce

-- | The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
rciTags :: Lens' RegisterContainerInstance [Tag]
rciTags = lens _rciTags (\s a -> s {_rciTags = a}) . _Default . _Coerce

instance AWSRequest RegisterContainerInstance where
  type
    Rs RegisterContainerInstance =
      RegisterContainerInstanceResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          RegisterContainerInstanceResponse'
            <$> (x .?> "containerInstance") <*> (pure (fromEnum s))
      )

instance Hashable RegisterContainerInstance

instance NFData RegisterContainerInstance

instance ToHeaders RegisterContainerInstance where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.RegisterContainerInstance" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterContainerInstance where
  toJSON RegisterContainerInstance' {..} =
    object
      ( catMaybes
          [ ("platformDevices" .=) <$> _rciPlatformDevices,
            ("instanceIdentityDocumentSignature" .=)
              <$> _rciInstanceIdentityDocumentSignature,
            ("cluster" .=) <$> _rciCluster,
            ("instanceIdentityDocument" .=) <$> _rciInstanceIdentityDocument,
            ("containerInstanceArn" .=) <$> _rciContainerInstanceARN,
            ("versionInfo" .=) <$> _rciVersionInfo,
            ("attributes" .=) <$> _rciAttributes,
            ("totalResources" .=) <$> _rciTotalResources,
            ("tags" .=) <$> _rciTags
          ]
      )

instance ToPath RegisterContainerInstance where
  toPath = const "/"

instance ToQuery RegisterContainerInstance where
  toQuery = const mempty

-- | /See:/ 'registerContainerInstanceResponse' smart constructor.
data RegisterContainerInstanceResponse = RegisterContainerInstanceResponse'
  { _rcirsContainerInstance ::
      !( Maybe
           ContainerInstance
       ),
    _rcirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterContainerInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcirsContainerInstance' - The container instance that was registered.
--
-- * 'rcirsResponseStatus' - -- | The response status code.
registerContainerInstanceResponse ::
  -- | 'rcirsResponseStatus'
  Int ->
  RegisterContainerInstanceResponse
registerContainerInstanceResponse pResponseStatus_ =
  RegisterContainerInstanceResponse'
    { _rcirsContainerInstance =
        Nothing,
      _rcirsResponseStatus = pResponseStatus_
    }

-- | The container instance that was registered.
rcirsContainerInstance :: Lens' RegisterContainerInstanceResponse (Maybe ContainerInstance)
rcirsContainerInstance = lens _rcirsContainerInstance (\s a -> s {_rcirsContainerInstance = a})

-- | -- | The response status code.
rcirsResponseStatus :: Lens' RegisterContainerInstanceResponse Int
rcirsResponseStatus = lens _rcirsResponseStatus (\s a -> s {_rcirsResponseStatus = a})

instance NFData RegisterContainerInstanceResponse
