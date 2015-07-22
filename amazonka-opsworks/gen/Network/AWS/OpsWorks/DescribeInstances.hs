{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a set of instances.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeInstances.html>
module Network.AWS.OpsWorks.DescribeInstances
    (
    -- * Request
      DescribeInstances
    -- ** Request constructor
    , describeInstances
    -- ** Request lenses
    , dirqInstanceIds
    , dirqStackId
    , dirqLayerId

    -- * Response
    , DescribeInstancesResponse
    -- ** Response constructor
    , describeInstancesResponse
    -- ** Response lenses
    , dirsInstances
    , dirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirqInstanceIds'
--
-- * 'dirqStackId'
--
-- * 'dirqLayerId'
data DescribeInstances = DescribeInstances'
    { _dirqInstanceIds :: !(Maybe [Text])
    , _dirqStackId     :: !(Maybe Text)
    , _dirqLayerId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstances' smart constructor.
describeInstances :: DescribeInstances
describeInstances =
    DescribeInstances'
    { _dirqInstanceIds = Nothing
    , _dirqStackId = Nothing
    , _dirqLayerId = Nothing
    }

-- | An array of instance IDs to be described. If you use this parameter,
-- @DescribeInstances@ returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
dirqInstanceIds :: Lens' DescribeInstances [Text]
dirqInstanceIds = lens _dirqInstanceIds (\ s a -> s{_dirqInstanceIds = a}) . _Default;

-- | A stack ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified stack.
dirqStackId :: Lens' DescribeInstances (Maybe Text)
dirqStackId = lens _dirqStackId (\ s a -> s{_dirqStackId = a});

-- | A layer ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified layer.
dirqLayerId :: Lens' DescribeInstances (Maybe Text)
dirqLayerId = lens _dirqLayerId (\ s a -> s{_dirqLayerId = a});

instance AWSRequest DescribeInstances where
        type Sv DescribeInstances = OpsWorks
        type Rs DescribeInstances = DescribeInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstancesResponse' <$>
                   (x .?> "Instances" .!@ mempty) <*>
                     (pure (fromEnum s)))

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
              ["InstanceIds" .= _dirqInstanceIds,
               "StackId" .= _dirqStackId, "LayerId" .= _dirqLayerId]

instance ToPath DescribeInstances where
        toPath = const "/"

instance ToQuery DescribeInstances where
        toQuery = const mempty

-- | Contains the response to a @DescribeInstances@ request.
--
-- /See:/ 'describeInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirsInstances'
--
-- * 'dirsStatus'
data DescribeInstancesResponse = DescribeInstancesResponse'
    { _dirsInstances :: !(Maybe [Instance])
    , _dirsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstancesResponse' smart constructor.
describeInstancesResponse :: Int -> DescribeInstancesResponse
describeInstancesResponse pStatus =
    DescribeInstancesResponse'
    { _dirsInstances = Nothing
    , _dirsStatus = pStatus
    }

-- | An array of @Instance@ objects that describe the instances.
dirsInstances :: Lens' DescribeInstancesResponse [Instance]
dirsInstances = lens _dirsInstances (\ s a -> s{_dirsInstances = a}) . _Default;

-- | FIXME: Undocumented member.
dirsStatus :: Lens' DescribeInstancesResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});
