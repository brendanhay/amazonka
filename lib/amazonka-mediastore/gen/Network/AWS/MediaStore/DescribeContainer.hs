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
-- Module      : Network.AWS.MediaStore.DescribeContainer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The @DescribeContainer@ request returns a single @Container@ object based on @ContainerName@ . To return all @Container@ objects that are associated with a specified AWS account, use 'ListContainers' .
--
--
module Network.AWS.MediaStore.DescribeContainer
    (
    -- * Creating a Request
      describeContainer
    , DescribeContainer
    -- * Request Lenses
    , dContainerName

    -- * Destructuring the Response
    , describeContainerResponse
    , DescribeContainerResponse
    -- * Response Lenses
    , drsContainer
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeContainer' smart constructor.
newtype DescribeContainer = DescribeContainer'
  { _dContainerName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContainer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dContainerName' - The name of the container to query.
describeContainer
    :: DescribeContainer
describeContainer = DescribeContainer' {_dContainerName = Nothing}


-- | The name of the container to query.
dContainerName :: Lens' DescribeContainer (Maybe Text)
dContainerName = lens _dContainerName (\ s a -> s{_dContainerName = a})

instance AWSRequest DescribeContainer where
        type Rs DescribeContainer = DescribeContainerResponse
        request = postJSON mediaStore
        response
          = receiveJSON
              (\ s h x ->
                 DescribeContainerResponse' <$>
                   (x .?> "Container") <*> (pure (fromEnum s)))

instance Hashable DescribeContainer where

instance NFData DescribeContainer where

instance ToHeaders DescribeContainer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.DescribeContainer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeContainer where
        toJSON DescribeContainer'{..}
          = object
              (catMaybes
                 [("ContainerName" .=) <$> _dContainerName])

instance ToPath DescribeContainer where
        toPath = const "/"

instance ToQuery DescribeContainer where
        toQuery = const mempty

-- | /See:/ 'describeContainerResponse' smart constructor.
data DescribeContainerResponse = DescribeContainerResponse'
  { _drsContainer      :: !(Maybe Container)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContainerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsContainer' - The name of the queried container.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeContainerResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeContainerResponse
describeContainerResponse pResponseStatus_ =
  DescribeContainerResponse'
    {_drsContainer = Nothing, _drsResponseStatus = pResponseStatus_}


-- | The name of the queried container.
drsContainer :: Lens' DescribeContainerResponse (Maybe Container)
drsContainer = lens _drsContainer (\ s a -> s{_drsContainer = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeContainerResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeContainerResponse where
