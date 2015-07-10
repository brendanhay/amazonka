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
    , draiaInstanceId
    , draiaRAIdArrayIds
    , draiaStackId

    -- * Response
    , DescribeRAIdArraysResponse
    -- ** Response constructor
    , describeRAIdArraysResponse
    -- ** Response lenses
    , draiarRAIdArrays
    , draiarStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRAIdArrays' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'draiaInstanceId'
--
-- * 'draiaRAIdArrayIds'
--
-- * 'draiaStackId'
data DescribeRAIdArrays = DescribeRAIdArrays'
    { _draiaInstanceId   :: !(Maybe Text)
    , _draiaRAIdArrayIds :: !(Maybe [Text])
    , _draiaStackId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRAIdArrays' smart constructor.
describeRAIdArrays :: DescribeRAIdArrays
describeRAIdArrays =
    DescribeRAIdArrays'
    { _draiaInstanceId = Nothing
    , _draiaRAIdArrayIds = Nothing
    , _draiaStackId = Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeRaidArrays@ returns
-- descriptions of the RAID arrays associated with the specified instance.
draiaInstanceId :: Lens' DescribeRAIdArrays (Maybe Text)
draiaInstanceId = lens _draiaInstanceId (\ s a -> s{_draiaInstanceId = a});

-- | An array of RAID array IDs. If you use this parameter,
-- @DescribeRaidArrays@ returns descriptions of the specified arrays.
-- Otherwise, it returns a description of every array.
draiaRAIdArrayIds :: Lens' DescribeRAIdArrays [Text]
draiaRAIdArrayIds = lens _draiaRAIdArrayIds (\ s a -> s{_draiaRAIdArrayIds = a}) . _Default;

-- | The stack ID.
draiaStackId :: Lens' DescribeRAIdArrays (Maybe Text)
draiaStackId = lens _draiaStackId (\ s a -> s{_draiaStackId = a});

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
              ["InstanceId" .= _draiaInstanceId,
               "RaidArrayIds" .= _draiaRAIdArrayIds,
               "StackId" .= _draiaStackId]

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
-- * 'draiarRAIdArrays'
--
-- * 'draiarStatus'
data DescribeRAIdArraysResponse = DescribeRAIdArraysResponse'
    { _draiarRAIdArrays :: !(Maybe [RAIdArray])
    , _draiarStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRAIdArraysResponse' smart constructor.
describeRAIdArraysResponse :: Int -> DescribeRAIdArraysResponse
describeRAIdArraysResponse pStatus =
    DescribeRAIdArraysResponse'
    { _draiarRAIdArrays = Nothing
    , _draiarStatus = pStatus
    }

-- | A @RaidArrays@ object that describes the specified RAID arrays.
draiarRAIdArrays :: Lens' DescribeRAIdArraysResponse [RAIdArray]
draiarRAIdArrays = lens _draiarRAIdArrays (\ s a -> s{_draiarRAIdArrays = a}) . _Default;

-- | FIXME: Undocumented member.
draiarStatus :: Lens' DescribeRAIdArraysResponse Int
draiarStatus = lens _draiarStatus (\ s a -> s{_draiarStatus = a});
