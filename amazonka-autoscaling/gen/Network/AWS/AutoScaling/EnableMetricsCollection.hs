{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables monitoring of the specified metrics for the specified Auto
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
    , emcrqMetrics
    , emcrqAutoScalingGroupName
    , emcrqGranularity

    -- * Response
    , EnableMetricsCollectionResponse
    -- ** Response constructor
    , enableMetricsCollectionResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableMetricsCollection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emcrqMetrics'
--
-- * 'emcrqAutoScalingGroupName'
--
-- * 'emcrqGranularity'
data EnableMetricsCollection = EnableMetricsCollection'
    { _emcrqMetrics              :: !(Maybe [Text])
    , _emcrqAutoScalingGroupName :: !Text
    , _emcrqGranularity          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMetricsCollection' smart constructor.
enableMetricsCollection :: Text -> Text -> EnableMetricsCollection
enableMetricsCollection pAutoScalingGroupName pGranularity =
    EnableMetricsCollection'
    { _emcrqMetrics = Nothing
    , _emcrqAutoScalingGroupName = pAutoScalingGroupName
    , _emcrqGranularity = pGranularity
    }

-- | One or more metrics. If you omit this parameter, all metrics are
-- enabled.
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
-- Note that the @GroupStandbyInstances@ metric is not enabled by default.
-- You must explicitly request this metric.
emcrqMetrics :: Lens' EnableMetricsCollection [Text]
emcrqMetrics = lens _emcrqMetrics (\ s a -> s{_emcrqMetrics = a}) . _Default;

-- | The name or ARN of the Auto Scaling group.
emcrqAutoScalingGroupName :: Lens' EnableMetricsCollection Text
emcrqAutoScalingGroupName = lens _emcrqAutoScalingGroupName (\ s a -> s{_emcrqAutoScalingGroupName = a});

-- | The granularity to associate with the metrics to collect. The only valid
-- value is @1Minute@.
emcrqGranularity :: Lens' EnableMetricsCollection Text
emcrqGranularity = lens _emcrqGranularity (\ s a -> s{_emcrqGranularity = a});

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
               "Metrics" =:
                 toQuery (toQueryList "member" <$> _emcrqMetrics),
               "AutoScalingGroupName" =: _emcrqAutoScalingGroupName,
               "Granularity" =: _emcrqGranularity]

-- | /See:/ 'enableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse =
    EnableMetricsCollectionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMetricsCollectionResponse' smart constructor.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse'
