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
-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables group metrics for the specified Auto Scaling group. For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-instance-monitoring.html Monitoring Your Auto Scaling Groups and Instances> in the /Auto Scaling User Guide/ .
--
--
module Network.AWS.AutoScaling.EnableMetricsCollection
    (
    -- * Creating a Request
      enableMetricsCollection
    , EnableMetricsCollection
    -- * Request Lenses
    , emcMetrics
    , emcAutoScalingGroupName
    , emcGranularity

    -- * Destructuring the Response
    , enableMetricsCollectionResponse
    , EnableMetricsCollectionResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableMetricsCollection' smart constructor.
data EnableMetricsCollection = EnableMetricsCollection'
  { _emcMetrics              :: !(Maybe [Text])
  , _emcAutoScalingGroupName :: !Text
  , _emcGranularity          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableMetricsCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emcMetrics' - One or more of the following metrics. If you omit this parameter, all metrics are enabled.     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@
--
-- * 'emcAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'emcGranularity' - The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
enableMetricsCollection
    :: Text -- ^ 'emcAutoScalingGroupName'
    -> Text -- ^ 'emcGranularity'
    -> EnableMetricsCollection
enableMetricsCollection pAutoScalingGroupName_ pGranularity_ =
  EnableMetricsCollection'
    { _emcMetrics = Nothing
    , _emcAutoScalingGroupName = pAutoScalingGroupName_
    , _emcGranularity = pGranularity_
    }


-- | One or more of the following metrics. If you omit this parameter, all metrics are enabled.     * @GroupMinSize@      * @GroupMaxSize@      * @GroupDesiredCapacity@      * @GroupInServiceInstances@      * @GroupPendingInstances@      * @GroupStandbyInstances@      * @GroupTerminatingInstances@      * @GroupTotalInstances@
emcMetrics :: Lens' EnableMetricsCollection [Text]
emcMetrics = lens _emcMetrics (\ s a -> s{_emcMetrics = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
emcAutoScalingGroupName :: Lens' EnableMetricsCollection Text
emcAutoScalingGroupName = lens _emcAutoScalingGroupName (\ s a -> s{_emcAutoScalingGroupName = a})

-- | The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
emcGranularity :: Lens' EnableMetricsCollection Text
emcGranularity = lens _emcGranularity (\ s a -> s{_emcGranularity = a})

instance AWSRequest EnableMetricsCollection where
        type Rs EnableMetricsCollection =
             EnableMetricsCollectionResponse
        request = postQuery autoScaling
        response
          = receiveNull EnableMetricsCollectionResponse'

instance Hashable EnableMetricsCollection where

instance NFData EnableMetricsCollection where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableMetricsCollectionResponse' with the minimum fields required to make a request.
--
enableMetricsCollectionResponse
    :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse'


instance NFData EnableMetricsCollectionResponse where
