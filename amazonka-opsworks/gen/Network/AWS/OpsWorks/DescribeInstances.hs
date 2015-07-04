{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a description of a set of instances.
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
    , diInstanceIds
    , diStackId
    , diLayerId

    -- * Response
    , DescribeInstancesResponse
    -- ** Response constructor
    , describeInstancesResponse
    -- ** Response lenses
    , dirInstances
    , dirStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diInstanceIds'
--
-- * 'diStackId'
--
-- * 'diLayerId'
data DescribeInstances = DescribeInstances'
    { _diInstanceIds :: !(Maybe [Text])
    , _diStackId     :: !(Maybe Text)
    , _diLayerId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstances' smart constructor.
describeInstances :: DescribeInstances
describeInstances =
    DescribeInstances'
    { _diInstanceIds = Nothing
    , _diStackId = Nothing
    , _diLayerId = Nothing
    }

-- | An array of instance IDs to be described. If you use this parameter,
-- @DescribeInstances@ returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
diInstanceIds :: Lens' DescribeInstances [Text]
diInstanceIds = lens _diInstanceIds (\ s a -> s{_diInstanceIds = a}) . _Default;

-- | A stack ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified stack.
diStackId :: Lens' DescribeInstances (Maybe Text)
diStackId = lens _diStackId (\ s a -> s{_diStackId = a});

-- | A layer ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified layer.
diLayerId :: Lens' DescribeInstances (Maybe Text)
diLayerId = lens _diLayerId (\ s a -> s{_diLayerId = a});

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
              ["InstanceIds" .= _diInstanceIds,
               "StackId" .= _diStackId, "LayerId" .= _diLayerId]

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
-- * 'dirInstances'
--
-- * 'dirStatus'
data DescribeInstancesResponse = DescribeInstancesResponse'
    { _dirInstances :: !(Maybe [Instance])
    , _dirStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeInstancesResponse' smart constructor.
describeInstancesResponse :: Int -> DescribeInstancesResponse
describeInstancesResponse pStatus =
    DescribeInstancesResponse'
    { _dirInstances = Nothing
    , _dirStatus = pStatus
    }

-- | An array of @Instance@ objects that describe the instances.
dirInstances :: Lens' DescribeInstancesResponse [Instance]
dirInstances = lens _dirInstances (\ s a -> s{_dirInstances = a}) . _Default;

-- | FIXME: Undocumented member.
dirStatus :: Lens' DescribeInstancesResponse Int
dirStatus = lens _dirStatus (\ s a -> s{_dirStatus = a});
