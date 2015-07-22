{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeRAIdArrays
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describe an instance\'s RAID arrays.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeRAIdArrays.html>
module Network.AWS.OpsWorks.DescribeRAIdArrays
    (
    -- * Request
      DescribeRAIdArrays
    -- ** Request constructor
    , describeRAIdArrays
    -- ** Request lenses
    , draiarqInstanceId
    , draiarqRAIdArrayIds
    , draiarqStackId

    -- * Response
    , DescribeRAIdArraysResponse
    -- ** Response constructor
    , describeRAIdArraysResponse
    -- ** Response lenses
    , draiarsRAIdArrays
    , draiarsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRAIdArrays' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'draiarqInstanceId'
--
-- * 'draiarqRAIdArrayIds'
--
-- * 'draiarqStackId'
data DescribeRAIdArrays = DescribeRAIdArrays'
    { _draiarqInstanceId   :: !(Maybe Text)
    , _draiarqRAIdArrayIds :: !(Maybe [Text])
    , _draiarqStackId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRAIdArrays' smart constructor.
describeRAIdArrays :: DescribeRAIdArrays
describeRAIdArrays =
    DescribeRAIdArrays'
    { _draiarqInstanceId = Nothing
    , _draiarqRAIdArrayIds = Nothing
    , _draiarqStackId = Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeRaidArrays@ returns
-- descriptions of the RAID arrays associated with the specified instance.
draiarqInstanceId :: Lens' DescribeRAIdArrays (Maybe Text)
draiarqInstanceId = lens _draiarqInstanceId (\ s a -> s{_draiarqInstanceId = a});

-- | An array of RAID array IDs. If you use this parameter,
-- @DescribeRaidArrays@ returns descriptions of the specified arrays.
-- Otherwise, it returns a description of every array.
draiarqRAIdArrayIds :: Lens' DescribeRAIdArrays [Text]
draiarqRAIdArrayIds = lens _draiarqRAIdArrayIds (\ s a -> s{_draiarqRAIdArrayIds = a}) . _Default;

-- | The stack ID.
draiarqStackId :: Lens' DescribeRAIdArrays (Maybe Text)
draiarqStackId = lens _draiarqStackId (\ s a -> s{_draiarqStackId = a});

instance AWSRequest DescribeRAIdArrays where
        type Sv DescribeRAIdArrays = OpsWorks
        type Rs DescribeRAIdArrays =
             DescribeRAIdArraysResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRAIdArraysResponse' <$>
                   (x .?> "RaidArrays" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeRAIdArrays where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeRAIdArrays" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRAIdArrays where
        toJSON DescribeRAIdArrays'{..}
          = object
              ["InstanceId" .= _draiarqInstanceId,
               "RaidArrayIds" .= _draiarqRAIdArrayIds,
               "StackId" .= _draiarqStackId]

instance ToPath DescribeRAIdArrays where
        toPath = const "/"

instance ToQuery DescribeRAIdArrays where
        toQuery = const mempty

-- | Contains the response to a @DescribeRaidArrays@ request.
--
-- /See:/ 'describeRAIdArraysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'draiarsRAIdArrays'
--
-- * 'draiarsStatus'
data DescribeRAIdArraysResponse = DescribeRAIdArraysResponse'
    { _draiarsRAIdArrays :: !(Maybe [RAIdArray])
    , _draiarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRAIdArraysResponse' smart constructor.
describeRAIdArraysResponse :: Int -> DescribeRAIdArraysResponse
describeRAIdArraysResponse pStatus =
    DescribeRAIdArraysResponse'
    { _draiarsRAIdArrays = Nothing
    , _draiarsStatus = pStatus
    }

-- | A @RaidArrays@ object that describes the specified RAID arrays.
draiarsRAIdArrays :: Lens' DescribeRAIdArraysResponse [RAIdArray]
draiarsRAIdArrays = lens _draiarsRAIdArrays (\ s a -> s{_draiarsRAIdArrays = a}) . _Default;

-- | FIXME: Undocumented member.
draiarsStatus :: Lens' DescribeRAIdArraysResponse Int
draiarsStatus = lens _draiarsStatus (\ s a -> s{_draiarsStatus = a});
