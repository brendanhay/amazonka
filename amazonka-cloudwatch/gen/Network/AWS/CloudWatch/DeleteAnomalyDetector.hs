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
-- Module      : Network.AWS.CloudWatch.DeleteAnomalyDetector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified anomaly detection model from your account.
--
--
module Network.AWS.CloudWatch.DeleteAnomalyDetector
    (
    -- * Creating a Request
      deleteAnomalyDetector
    , DeleteAnomalyDetector
    -- * Request Lenses
    , dadDimensions
    , dadNamespace
    , dadMetricName
    , dadStat

    -- * Destructuring the Response
    , deleteAnomalyDetectorResponse
    , DeleteAnomalyDetectorResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAnomalyDetector' smart constructor.
data DeleteAnomalyDetector = DeleteAnomalyDetector'
  { _dadDimensions :: !(Maybe [Dimension])
  , _dadNamespace  :: !Text
  , _dadMetricName :: !Text
  , _dadStat       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAnomalyDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dadDimensions' - The metric dimensions associated with the anomaly detection model to delete.
--
-- * 'dadNamespace' - The namespace associated with the anomaly detection model to delete.
--
-- * 'dadMetricName' - The metric name associated with the anomaly detection model to delete.
--
-- * 'dadStat' - The statistic associated with the anomaly detection model to delete.
deleteAnomalyDetector
    :: Text -- ^ 'dadNamespace'
    -> Text -- ^ 'dadMetricName'
    -> Text -- ^ 'dadStat'
    -> DeleteAnomalyDetector
deleteAnomalyDetector pNamespace_ pMetricName_ pStat_ =
  DeleteAnomalyDetector'
    { _dadDimensions = Nothing
    , _dadNamespace = pNamespace_
    , _dadMetricName = pMetricName_
    , _dadStat = pStat_
    }


-- | The metric dimensions associated with the anomaly detection model to delete.
dadDimensions :: Lens' DeleteAnomalyDetector [Dimension]
dadDimensions = lens _dadDimensions (\ s a -> s{_dadDimensions = a}) . _Default . _Coerce

-- | The namespace associated with the anomaly detection model to delete.
dadNamespace :: Lens' DeleteAnomalyDetector Text
dadNamespace = lens _dadNamespace (\ s a -> s{_dadNamespace = a})

-- | The metric name associated with the anomaly detection model to delete.
dadMetricName :: Lens' DeleteAnomalyDetector Text
dadMetricName = lens _dadMetricName (\ s a -> s{_dadMetricName = a})

-- | The statistic associated with the anomaly detection model to delete.
dadStat :: Lens' DeleteAnomalyDetector Text
dadStat = lens _dadStat (\ s a -> s{_dadStat = a})

instance AWSRequest DeleteAnomalyDetector where
        type Rs DeleteAnomalyDetector =
             DeleteAnomalyDetectorResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "DeleteAnomalyDetectorResult"
              (\ s h x ->
                 DeleteAnomalyDetectorResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteAnomalyDetector where

instance NFData DeleteAnomalyDetector where

instance ToHeaders DeleteAnomalyDetector where
        toHeaders = const mempty

instance ToPath DeleteAnomalyDetector where
        toPath = const "/"

instance ToQuery DeleteAnomalyDetector where
        toQuery DeleteAnomalyDetector'{..}
          = mconcat
              ["Action" =: ("DeleteAnomalyDetector" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _dadDimensions),
               "Namespace" =: _dadNamespace,
               "MetricName" =: _dadMetricName, "Stat" =: _dadStat]

-- | /See:/ 'deleteAnomalyDetectorResponse' smart constructor.
newtype DeleteAnomalyDetectorResponse = DeleteAnomalyDetectorResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAnomalyDetectorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteAnomalyDetectorResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteAnomalyDetectorResponse
deleteAnomalyDetectorResponse pResponseStatus_ =
  DeleteAnomalyDetectorResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteAnomalyDetectorResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteAnomalyDetectorResponse where
