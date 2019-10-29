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
-- Module      : Network.AWS.CloudWatch.PutAnomalyDetector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an anomaly detection model for a CloudWatch metric. You can use the model to display a band of expected normal values when the metric is graphed.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Anomaly_Detection.html CloudWatch Anomaly Detection> .
--
module Network.AWS.CloudWatch.PutAnomalyDetector
    (
    -- * Creating a Request
      putAnomalyDetector
    , PutAnomalyDetector
    -- * Request Lenses
    , padConfiguration
    , padDimensions
    , padNamespace
    , padMetricName
    , padStat

    -- * Destructuring the Response
    , putAnomalyDetectorResponse
    , PutAnomalyDetectorResponse
    -- * Response Lenses
    , padrsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAnomalyDetector' smart constructor.
data PutAnomalyDetector = PutAnomalyDetector'
  { _padConfiguration :: !(Maybe AnomalyDetectorConfiguration)
  , _padDimensions    :: !(Maybe [Dimension])
  , _padNamespace     :: !Text
  , _padMetricName    :: !Text
  , _padStat          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAnomalyDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'padConfiguration' - The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges. The configuration can also include the time zone to use for the metric. You can in
--
-- * 'padDimensions' - The metric dimensions to create the anomaly detection model for.
--
-- * 'padNamespace' - The namespace of the metric to create the anomaly detection model for.
--
-- * 'padMetricName' - The name of the metric to create the anomaly detection model for.
--
-- * 'padStat' - The statistic to use for the metric and the anomaly detection model.
putAnomalyDetector
    :: Text -- ^ 'padNamespace'
    -> Text -- ^ 'padMetricName'
    -> Text -- ^ 'padStat'
    -> PutAnomalyDetector
putAnomalyDetector pNamespace_ pMetricName_ pStat_ =
  PutAnomalyDetector'
    { _padConfiguration = Nothing
    , _padDimensions = Nothing
    , _padNamespace = pNamespace_
    , _padMetricName = pMetricName_
    , _padStat = pStat_
    }


-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude when training and updating the model. You can specify as many as 10 time ranges. The configuration can also include the time zone to use for the metric. You can in
padConfiguration :: Lens' PutAnomalyDetector (Maybe AnomalyDetectorConfiguration)
padConfiguration = lens _padConfiguration (\ s a -> s{_padConfiguration = a})

-- | The metric dimensions to create the anomaly detection model for.
padDimensions :: Lens' PutAnomalyDetector [Dimension]
padDimensions = lens _padDimensions (\ s a -> s{_padDimensions = a}) . _Default . _Coerce

-- | The namespace of the metric to create the anomaly detection model for.
padNamespace :: Lens' PutAnomalyDetector Text
padNamespace = lens _padNamespace (\ s a -> s{_padNamespace = a})

-- | The name of the metric to create the anomaly detection model for.
padMetricName :: Lens' PutAnomalyDetector Text
padMetricName = lens _padMetricName (\ s a -> s{_padMetricName = a})

-- | The statistic to use for the metric and the anomaly detection model.
padStat :: Lens' PutAnomalyDetector Text
padStat = lens _padStat (\ s a -> s{_padStat = a})

instance AWSRequest PutAnomalyDetector where
        type Rs PutAnomalyDetector =
             PutAnomalyDetectorResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "PutAnomalyDetectorResult"
              (\ s h x ->
                 PutAnomalyDetectorResponse' <$> (pure (fromEnum s)))

instance Hashable PutAnomalyDetector where

instance NFData PutAnomalyDetector where

instance ToHeaders PutAnomalyDetector where
        toHeaders = const mempty

instance ToPath PutAnomalyDetector where
        toPath = const "/"

instance ToQuery PutAnomalyDetector where
        toQuery PutAnomalyDetector'{..}
          = mconcat
              ["Action" =: ("PutAnomalyDetector" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "Configuration" =: _padConfiguration,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _padDimensions),
               "Namespace" =: _padNamespace,
               "MetricName" =: _padMetricName, "Stat" =: _padStat]

-- | /See:/ 'putAnomalyDetectorResponse' smart constructor.
newtype PutAnomalyDetectorResponse = PutAnomalyDetectorResponse'
  { _padrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAnomalyDetectorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'padrsResponseStatus' - -- | The response status code.
putAnomalyDetectorResponse
    :: Int -- ^ 'padrsResponseStatus'
    -> PutAnomalyDetectorResponse
putAnomalyDetectorResponse pResponseStatus_ =
  PutAnomalyDetectorResponse' {_padrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
padrsResponseStatus :: Lens' PutAnomalyDetectorResponse Int
padrsResponseStatus = lens _padrsResponseStatus (\ s a -> s{_padrsResponseStatus = a})

instance NFData PutAnomalyDetectorResponse where
