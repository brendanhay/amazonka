{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
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

-- | Enables monitoring of the specified metrics for the specified Auto
-- Scaling group.
--
-- You can only enable metrics collection if @InstanceMonitoring@ in the
-- launch configuration for the group is set to @True@.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnableMetricsCollection.html>
module Network.AWS.AutoScaling.EnableMetricsCollection
    (
    -- * Request
      EnableMetricsCollection
    -- ** Request constructor
    , enableMetricsCollection
    -- ** Request lenses
    , emcMetrics
    , emcAutoScalingGroupName
    , emcGranularity

    -- * Response
    , EnableMetricsCollectionResponse
    -- ** Response constructor
    , enableMetricsCollectionResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'enableMetricsCollection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emcMetrics'
--
-- * 'emcAutoScalingGroupName'
--
-- * 'emcGranularity'
data EnableMetricsCollection = EnableMetricsCollection'{_emcMetrics :: Maybe [Text], _emcAutoScalingGroupName :: Text, _emcGranularity :: Text} deriving (Eq, Read, Show)

-- | 'EnableMetricsCollection' smart constructor.
enableMetricsCollection :: Text -> Text -> EnableMetricsCollection
enableMetricsCollection pAutoScalingGroupName pGranularity = EnableMetricsCollection'{_emcMetrics = Nothing, _emcAutoScalingGroupName = pAutoScalingGroupName, _emcGranularity = pGranularity};

-- | One or more of the following metrics:
--
-- -   GroupMinSize
--
-- -   GroupMaxSize
--
-- -   GroupDesiredCapacity
--
-- -   GroupInServiceInstances
--
-- -   GroupPendingInstances
--
-- -   GroupStandbyInstances
--
-- -   GroupTerminatingInstances
--
-- -   GroupTotalInstances
--
-- If you omit this parameter, all metrics are enabled.
--
-- The @GroupStandbyInstances@ metric is not returned by default. You must
-- explicitly request it when calling EnableMetricsCollection.
emcMetrics :: Lens' EnableMetricsCollection (Maybe [Text])
emcMetrics = lens _emcMetrics (\ s a -> s{_emcMetrics = a});

-- | The name or ARN of the Auto Scaling group.
emcAutoScalingGroupName :: Lens' EnableMetricsCollection Text
emcAutoScalingGroupName = lens _emcAutoScalingGroupName (\ s a -> s{_emcAutoScalingGroupName = a});

-- | The granularity to associate with the metrics to collect. Currently, the
-- only valid value is \"1Minute\".
emcGranularity :: Lens' EnableMetricsCollection Text
emcGranularity = lens _emcGranularity (\ s a -> s{_emcGranularity = a});

instance AWSRequest EnableMetricsCollection where
        type Sv EnableMetricsCollection = AutoScaling
        type Rs EnableMetricsCollection =
             EnableMetricsCollectionResponse
        request = post
        response
          = receiveNull EnableMetricsCollectionResponse'

instance ToHeaders EnableMetricsCollection where
        toHeaders = const mempty

instance ToPath EnableMetricsCollection where
        toPath = const "/"

instance ToQuery EnableMetricsCollection where
        toQuery EnableMetricsCollection'{..}
          = mconcat
              ["Action" =:
                 ("EnableMetricsCollection" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Metrics" =: "member" =: _emcMetrics,
               "AutoScalingGroupName" =: _emcAutoScalingGroupName,
               "Granularity" =: _emcGranularity]

-- | /See:/ 'enableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse' deriving (Eq, Read, Show)

-- | 'EnableMetricsCollectionResponse' smart constructor.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse';
