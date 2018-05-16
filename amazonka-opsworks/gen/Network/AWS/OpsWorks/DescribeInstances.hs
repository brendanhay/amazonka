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
-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a set of instances.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeInstances
    (
    -- * Creating a Request
      describeInstances
    , DescribeInstances
    -- * Request Lenses
    , diInstanceIds
    , diStackId
    , diLayerId

    -- * Destructuring the Response
    , describeInstancesResponse
    , DescribeInstancesResponse
    -- * Response Lenses
    , dirsInstances
    , dirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { _diInstanceIds :: !(Maybe [Text])
  , _diStackId     :: !(Maybe Text)
  , _diLayerId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diInstanceIds' - An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
--
-- * 'diStackId' - A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
--
-- * 'diLayerId' - A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
describeInstances
    :: DescribeInstances
describeInstances =
  DescribeInstances'
    {_diInstanceIds = Nothing, _diStackId = Nothing, _diLayerId = Nothing}


-- | An array of instance IDs to be described. If you use this parameter, @DescribeInstances@ returns a description of the specified instances. Otherwise, it returns a description of every instance.
diInstanceIds :: Lens' DescribeInstances [Text]
diInstanceIds = lens _diInstanceIds (\ s a -> s{_diInstanceIds = a}) . _Default . _Coerce

-- | A stack ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified stack.
diStackId :: Lens' DescribeInstances (Maybe Text)
diStackId = lens _diStackId (\ s a -> s{_diStackId = a})

-- | A layer ID. If you use this parameter, @DescribeInstances@ returns descriptions of the instances associated with the specified layer.
diLayerId :: Lens' DescribeInstances (Maybe Text)
diLayerId = lens _diLayerId (\ s a -> s{_diLayerId = a})

instance AWSRequest DescribeInstances where
        type Rs DescribeInstances = DescribeInstancesResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstancesResponse' <$>
                   (x .?> "Instances" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeInstances where

instance NFData DescribeInstances where

instance ToHeaders DescribeInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInstances where
        toJSON DescribeInstances'{..}
          = object
              (catMaybes
                 [("InstanceIds" .=) <$> _diInstanceIds,
                  ("StackId" .=) <$> _diStackId,
                  ("LayerId" .=) <$> _diLayerId])

instance ToPath DescribeInstances where
        toPath = const "/"

instance ToQuery DescribeInstances where
        toQuery = const mempty

-- | Contains the response to a @DescribeInstances@ request.
--
--
--
-- /See:/ 'describeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { _dirsInstances      :: !(Maybe [Instance])
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsInstances' - An array of @Instance@ objects that describe the instances.
--
-- * 'dirsResponseStatus' - -- | The response status code.
describeInstancesResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DescribeInstancesResponse
describeInstancesResponse pResponseStatus_ =
  DescribeInstancesResponse'
    {_dirsInstances = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | An array of @Instance@ objects that describe the instances.
dirsInstances :: Lens' DescribeInstancesResponse [Instance]
dirsInstances = lens _dirsInstances (\ s a -> s{_dirsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DescribeInstancesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DescribeInstancesResponse where
