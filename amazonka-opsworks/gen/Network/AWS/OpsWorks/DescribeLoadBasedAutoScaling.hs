{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
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

-- | Describes load-based auto scaling configurations for specified layers.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeLoadBasedAutoScaling.html>
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
    (
    -- * Request
      DescribeLoadBasedAutoScaling
    -- ** Request constructor
    , describeLoadBasedAutoScaling
    -- ** Request lenses
    , dlbasLayerIds

    -- * Response
    , DescribeLoadBasedAutoScalingResponse
    -- ** Response constructor
    , describeLoadBasedAutoScalingResponse
    -- ** Response lenses
    , dlbasrLoadBasedAutoScalingConfigurations
    , dlbasrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbasLayerIds'
newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
    { _dlbasLayerIds :: [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeLoadBasedAutoScaling' smart constructor.
describeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling
describeLoadBasedAutoScaling =
    DescribeLoadBasedAutoScaling'
    { _dlbasLayerIds = mempty
    }

-- | An array of layer IDs.
dlbasLayerIds :: Lens' DescribeLoadBasedAutoScaling [Text]
dlbasLayerIds = lens _dlbasLayerIds (\ s a -> s{_dlbasLayerIds = a});

instance AWSRequest DescribeLoadBasedAutoScaling
         where
        type Sv DescribeLoadBasedAutoScaling = OpsWorks
        type Rs DescribeLoadBasedAutoScaling =
             DescribeLoadBasedAutoScalingResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLoadBasedAutoScalingResponse' <$>
                   (x .?> "LoadBasedAutoScalingConfigurations" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeLoadBasedAutoScaling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeLoadBasedAutoScaling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLoadBasedAutoScaling where
        toJSON DescribeLoadBasedAutoScaling'{..}
          = object ["LayerIds" .= _dlbasLayerIds]

instance ToPath DescribeLoadBasedAutoScaling where
        toPath = const "/"

instance ToQuery DescribeLoadBasedAutoScaling where
        toQuery = const mempty

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
-- /See:/ 'describeLoadBasedAutoScalingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbasrLoadBasedAutoScalingConfigurations'
--
-- * 'dlbasrStatus'
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
    { _dlbasrLoadBasedAutoScalingConfigurations :: !(Maybe [LoadBasedAutoScalingConfiguration])
    , _dlbasrStatus                             :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeLoadBasedAutoScalingResponse' smart constructor.
describeLoadBasedAutoScalingResponse :: Int -> DescribeLoadBasedAutoScalingResponse
describeLoadBasedAutoScalingResponse pStatus =
    DescribeLoadBasedAutoScalingResponse'
    { _dlbasrLoadBasedAutoScalingConfigurations = Nothing
    , _dlbasrStatus = pStatus
    }

-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe
-- each layer\'s configuration.
dlbasrLoadBasedAutoScalingConfigurations :: Lens' DescribeLoadBasedAutoScalingResponse [LoadBasedAutoScalingConfiguration]
dlbasrLoadBasedAutoScalingConfigurations = lens _dlbasrLoadBasedAutoScalingConfigurations (\ s a -> s{_dlbasrLoadBasedAutoScalingConfigurations = a}) . _Default;

-- | FIXME: Undocumented member.
dlbasrStatus :: Lens' DescribeLoadBasedAutoScalingResponse Int
dlbasrStatus = lens _dlbasrStatus (\ s a -> s{_dlbasrStatus = a});
