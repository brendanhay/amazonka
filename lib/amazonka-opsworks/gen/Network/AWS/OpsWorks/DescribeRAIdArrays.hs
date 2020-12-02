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
-- Module      : Network.AWS.OpsWorks.DescribeRAIdArrays
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an instance's RAID arrays.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeRAIdArrays
    (
    -- * Creating a Request
      describeRAIdArrays
    , DescribeRAIdArrays
    -- * Request Lenses
    , draiaInstanceId
    , draiaRAIdArrayIds
    , draiaStackId

    -- * Destructuring the Response
    , describeRAIdArraysResponse
    , DescribeRAIdArraysResponse
    -- * Response Lenses
    , draiarsRAIdArrays
    , draiarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRAIdArrays' smart constructor.
data DescribeRAIdArrays = DescribeRAIdArrays'
  { _draiaInstanceId   :: !(Maybe Text)
  , _draiaRAIdArrayIds :: !(Maybe [Text])
  , _draiaStackId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRAIdArrays' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'draiaInstanceId' - The instance ID. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the RAID arrays associated with the specified instance.
--
-- * 'draiaRAIdArrayIds' - An array of RAID array IDs. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the specified arrays. Otherwise, it returns a description of every array.
--
-- * 'draiaStackId' - The stack ID.
describeRAIdArrays
    :: DescribeRAIdArrays
describeRAIdArrays =
  DescribeRAIdArrays'
    { _draiaInstanceId = Nothing
    , _draiaRAIdArrayIds = Nothing
    , _draiaStackId = Nothing
    }


-- | The instance ID. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the RAID arrays associated with the specified instance.
draiaInstanceId :: Lens' DescribeRAIdArrays (Maybe Text)
draiaInstanceId = lens _draiaInstanceId (\ s a -> s{_draiaInstanceId = a})

-- | An array of RAID array IDs. If you use this parameter, @DescribeRaidArrays@ returns descriptions of the specified arrays. Otherwise, it returns a description of every array.
draiaRAIdArrayIds :: Lens' DescribeRAIdArrays [Text]
draiaRAIdArrayIds = lens _draiaRAIdArrayIds (\ s a -> s{_draiaRAIdArrayIds = a}) . _Default . _Coerce

-- | The stack ID.
draiaStackId :: Lens' DescribeRAIdArrays (Maybe Text)
draiaStackId = lens _draiaStackId (\ s a -> s{_draiaStackId = a})

instance AWSRequest DescribeRAIdArrays where
        type Rs DescribeRAIdArrays =
             DescribeRAIdArraysResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRAIdArraysResponse' <$>
                   (x .?> "RaidArrays" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeRAIdArrays where

instance NFData DescribeRAIdArrays where

instance ToHeaders DescribeRAIdArrays where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeRaidArrays" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRAIdArrays where
        toJSON DescribeRAIdArrays'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _draiaInstanceId,
                  ("RaidArrayIds" .=) <$> _draiaRAIdArrayIds,
                  ("StackId" .=) <$> _draiaStackId])

instance ToPath DescribeRAIdArrays where
        toPath = const "/"

instance ToQuery DescribeRAIdArrays where
        toQuery = const mempty

-- | Contains the response to a @DescribeRaidArrays@ request.
--
--
--
-- /See:/ 'describeRAIdArraysResponse' smart constructor.
data DescribeRAIdArraysResponse = DescribeRAIdArraysResponse'
  { _draiarsRAIdArrays     :: !(Maybe [RAIdArray])
  , _draiarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRAIdArraysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'draiarsRAIdArrays' - A @RaidArrays@ object that describes the specified RAID arrays.
--
-- * 'draiarsResponseStatus' - -- | The response status code.
describeRAIdArraysResponse
    :: Int -- ^ 'draiarsResponseStatus'
    -> DescribeRAIdArraysResponse
describeRAIdArraysResponse pResponseStatus_ =
  DescribeRAIdArraysResponse'
    {_draiarsRAIdArrays = Nothing, _draiarsResponseStatus = pResponseStatus_}


-- | A @RaidArrays@ object that describes the specified RAID arrays.
draiarsRAIdArrays :: Lens' DescribeRAIdArraysResponse [RAIdArray]
draiarsRAIdArrays = lens _draiarsRAIdArrays (\ s a -> s{_draiarsRAIdArrays = a}) . _Default . _Coerce

-- | -- | The response status code.
draiarsResponseStatus :: Lens' DescribeRAIdArraysResponse Int
draiarsResponseStatus = lens _draiarsResponseStatus (\ s a -> s{_draiarsResponseStatus = a})

instance NFData DescribeRAIdArraysResponse where
