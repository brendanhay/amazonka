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
-- Module      : Network.AWS.MediaStore.CreateContainer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a storage container to hold objects. A container is similar to a bucket in the Amazon S3 service.
--
--
module Network.AWS.MediaStore.CreateContainer
    (
    -- * Creating a Request
      createContainer
    , CreateContainer
    -- * Request Lenses
    , ccContainerName

    -- * Destructuring the Response
    , createContainerResponse
    , CreateContainerResponse
    -- * Response Lenses
    , ccrsResponseStatus
    , ccrsContainer
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContainer' smart constructor.
newtype CreateContainer = CreateContainer'
  { _ccContainerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccContainerName' - The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
createContainer
    :: Text -- ^ 'ccContainerName'
    -> CreateContainer
createContainer pContainerName_ =
  CreateContainer' {_ccContainerName = pContainerName_}


-- | The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
ccContainerName :: Lens' CreateContainer Text
ccContainerName = lens _ccContainerName (\ s a -> s{_ccContainerName = a})

instance AWSRequest CreateContainer where
        type Rs CreateContainer = CreateContainerResponse
        request = postJSON mediaStore
        response
          = receiveJSON
              (\ s h x ->
                 CreateContainerResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Container"))

instance Hashable CreateContainer where

instance NFData CreateContainer where

instance ToHeaders CreateContainer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.CreateContainer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateContainer where
        toJSON CreateContainer'{..}
          = object
              (catMaybes
                 [Just ("ContainerName" .= _ccContainerName)])

instance ToPath CreateContainer where
        toPath = const "/"

instance ToQuery CreateContainer where
        toQuery = const mempty

-- | /See:/ 'createContainerResponse' smart constructor.
data CreateContainerResponse = CreateContainerResponse'
  { _ccrsResponseStatus :: !Int
  , _ccrsContainer      :: !Container
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateContainerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsResponseStatus' - -- | The response status code.
--
-- * 'ccrsContainer' - ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies  ContainerName: The container name as specified in the request. CreationTime: Unix time stamp. Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ . The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
createContainerResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> Container -- ^ 'ccrsContainer'
    -> CreateContainerResponse
createContainerResponse pResponseStatus_ pContainer_ =
  CreateContainerResponse'
    {_ccrsResponseStatus = pResponseStatus_, _ccrsContainer = pContainer_}


-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateContainerResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

-- | ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies  ContainerName: The container name as specified in the request. CreationTime: Unix time stamp. Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ . The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
ccrsContainer :: Lens' CreateContainerResponse Container
ccrsContainer = lens _ccrsContainer (\ s a -> s{_ccrsContainer = a})

instance NFData CreateContainerResponse where
