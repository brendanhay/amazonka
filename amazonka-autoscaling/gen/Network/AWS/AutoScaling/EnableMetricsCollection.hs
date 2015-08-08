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
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnableMetricsCollection.html AWS API Reference> for EnableMetricsCollection.
module Network.AWS.AutoScaling.EnableMetricsCollection
    (
    -- * Creating a Request
      EnableMetricsCollection
    , enableMetricsCollection
    -- * Request Lenses
    , emcMetrics
    , emcAutoScalingGroupName
    , emcGranularity

    -- * Destructuring the Response
    , EnableMetricsCollectionResponse
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
-- * 'emcMetrics'
--
-- * 'emcAutoScalingGroupName'
--
-- * 'emcGranularity'
data EnableMetricsCollection = EnableMetricsCollection'
    { _emcMetrics              :: !(Maybe [Text])
    , _emcAutoScalingGroupName :: !Text
    , _emcGranularity          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMetricsCollection' smart constructor.
enableMetricsCollection :: Text -> Text -> EnableMetricsCollection
enableMetricsCollection pAutoScalingGroupName_ pGranularity_ =
    EnableMetricsCollection'
    { _emcMetrics = Nothing
    , _emcAutoScalingGroupName = pAutoScalingGroupName_
    , _emcGranularity = pGranularity_
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
emcMetrics :: Lens' EnableMetricsCollection [Text]
emcMetrics = lens _emcMetrics (\ s a -> s{_emcMetrics = a}) . _Default . _Coerce;

-- | The name or ARN of the Auto Scaling group.
emcAutoScalingGroupName :: Lens' EnableMetricsCollection Text
emcAutoScalingGroupName = lens _emcAutoScalingGroupName (\ s a -> s{_emcAutoScalingGroupName = a});

-- | The granularity to associate with the metrics to collect. The only valid
-- value is @1Minute@.
emcGranularity :: Lens' EnableMetricsCollection Text
emcGranularity = lens _emcGranularity (\ s a -> s{_emcGranularity = a});

instance AWSRequest EnableMetricsCollection where
        type Sv EnableMetricsCollection = AutoScaling
        type Rs EnableMetricsCollection =
             EnableMetricsCollectionResponse
        request = postQuery
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
                 toQuery (toQueryList "member" <$> _emcMetrics),
               "AutoScalingGroupName" =: _emcAutoScalingGroupName,
               "Granularity" =: _emcGranularity]

-- | /See:/ 'enableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse =
    EnableMetricsCollectionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMetricsCollectionResponse' smart constructor.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse'
