{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes time-based auto scaling configurations for specified
-- instances.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeTimeBasedAutoScaling.html>
module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
    (
    -- * Request
      DescribeTimeBasedAutoScaling
    -- ** Request constructor
    , describeTimeBasedAutoScaling
    -- ** Request lenses
    , dtbasInstanceIds

    -- * Response
    , DescribeTimeBasedAutoScalingResponse
    -- ** Response constructor
    , describeTimeBasedAutoScalingResponse
    -- ** Response lenses
    , dtbasrTimeBasedAutoScalingConfigurations
    , dtbasrStatusCode
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTimeBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtbasInstanceIds'
newtype DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling'{_dtbasInstanceIds :: [Text]} deriving (Eq, Read, Show)

-- | 'DescribeTimeBasedAutoScaling' smart constructor.
describeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling
describeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling'{_dtbasInstanceIds = mempty};

-- | An array of instance IDs.
dtbasInstanceIds :: Lens' DescribeTimeBasedAutoScaling [Text]
dtbasInstanceIds = lens _dtbasInstanceIds (\ s a -> s{_dtbasInstanceIds = a});

instance AWSRequest DescribeTimeBasedAutoScaling
         where
        type Sv DescribeTimeBasedAutoScaling = OpsWorks
        type Rs DescribeTimeBasedAutoScaling =
             DescribeTimeBasedAutoScalingResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTimeBasedAutoScalingResponse' <$>
                   (x .?> "TimeBasedAutoScalingConfigurations" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeTimeBasedAutoScaling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeTimeBasedAutoScaling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTimeBasedAutoScaling where
        toJSON DescribeTimeBasedAutoScaling'{..}
          = object ["InstanceIds" .= _dtbasInstanceIds]

instance ToPath DescribeTimeBasedAutoScaling where
        toPath = const "/"

instance ToQuery DescribeTimeBasedAutoScaling where
        toQuery = const mempty

-- | Contains the response to a @DescribeTimeBasedAutoScaling@ request.
--
-- /See:/ 'describeTimeBasedAutoScalingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtbasrTimeBasedAutoScalingConfigurations'
--
-- * 'dtbasrStatusCode'
data DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse'{_dtbasrTimeBasedAutoScalingConfigurations :: Maybe [TimeBasedAutoScalingConfiguration], _dtbasrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeTimeBasedAutoScalingResponse' smart constructor.
describeTimeBasedAutoScalingResponse :: Int -> DescribeTimeBasedAutoScalingResponse
describeTimeBasedAutoScalingResponse pStatusCode = DescribeTimeBasedAutoScalingResponse'{_dtbasrTimeBasedAutoScalingConfigurations = Nothing, _dtbasrStatusCode = pStatusCode};

-- | An array of @TimeBasedAutoScalingConfiguration@ objects that describe
-- the configuration for the specified instances.
dtbasrTimeBasedAutoScalingConfigurations :: Lens' DescribeTimeBasedAutoScalingResponse [TimeBasedAutoScalingConfiguration]
dtbasrTimeBasedAutoScalingConfigurations = lens _dtbasrTimeBasedAutoScalingConfigurations (\ s a -> s{_dtbasrTimeBasedAutoScalingConfigurations = a}) . _Default;

-- | FIXME: Undocumented member.
dtbasrStatusCode :: Lens' DescribeTimeBasedAutoScalingResponse Int
dtbasrStatusCode = lens _dtbasrStatusCode (\ s a -> s{_dtbasrStatusCode = a});
