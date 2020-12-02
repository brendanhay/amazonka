{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAnomalyDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the anomaly detection models that you have created in your account. You can list all models in your account or filter the results to only the models that are related to a certain namespace, metric name, or metric dimension.
module Network.AWS.CloudWatch.DescribeAnomalyDetectors
  ( -- * Creating a Request
    describeAnomalyDetectors,
    DescribeAnomalyDetectors,

    -- * Request Lenses
    dMetricName,
    dNamespace,
    dNextToken,
    dDimensions,
    dMaxResults,

    -- * Destructuring the Response
    describeAnomalyDetectorsResponse,
    DescribeAnomalyDetectorsResponse,

    -- * Response Lenses
    dadrsAnomalyDetectors,
    dadrsNextToken,
    dadrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAnomalyDetectors' smart constructor.
data DescribeAnomalyDetectors = DescribeAnomalyDetectors'
  { _dMetricName ::
      !(Maybe Text),
    _dNamespace :: !(Maybe Text),
    _dNextToken :: !(Maybe Text),
    _dDimensions :: !(Maybe [Dimension]),
    _dMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAnomalyDetectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMetricName' - Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
--
-- * 'dNamespace' - Limits the results to only the anomaly detection models that are associated with the specified namespace.
--
-- * 'dNextToken' - Use the token returned by the previous operation to request the next page of results.
--
-- * 'dDimensions' - Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
--
-- * 'dMaxResults' - The maximum number of results to return in one operation. The maximum value that you can specify is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeAnomalyDetectors ::
  DescribeAnomalyDetectors
describeAnomalyDetectors =
  DescribeAnomalyDetectors'
    { _dMetricName = Nothing,
      _dNamespace = Nothing,
      _dNextToken = Nothing,
      _dDimensions = Nothing,
      _dMaxResults = Nothing
    }

-- | Limits the results to only the anomaly detection models that are associated with the specified metric name. If there are multiple metrics with this name in different namespaces that have anomaly detection models, they're all returned.
dMetricName :: Lens' DescribeAnomalyDetectors (Maybe Text)
dMetricName = lens _dMetricName (\s a -> s {_dMetricName = a})

-- | Limits the results to only the anomaly detection models that are associated with the specified namespace.
dNamespace :: Lens' DescribeAnomalyDetectors (Maybe Text)
dNamespace = lens _dNamespace (\s a -> s {_dNamespace = a})

-- | Use the token returned by the previous operation to request the next page of results.
dNextToken :: Lens' DescribeAnomalyDetectors (Maybe Text)
dNextToken = lens _dNextToken (\s a -> s {_dNextToken = a})

-- | Limits the results to only the anomaly detection models that are associated with the specified metric dimensions. If there are multiple metrics that have these dimensions and have anomaly detection models associated, they're all returned.
dDimensions :: Lens' DescribeAnomalyDetectors [Dimension]
dDimensions = lens _dDimensions (\s a -> s {_dDimensions = a}) . _Default . _Coerce

-- | The maximum number of results to return in one operation. The maximum value that you can specify is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dMaxResults :: Lens' DescribeAnomalyDetectors (Maybe Natural)
dMaxResults = lens _dMaxResults (\s a -> s {_dMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeAnomalyDetectors where
  type Rs DescribeAnomalyDetectors = DescribeAnomalyDetectorsResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "DescribeAnomalyDetectorsResult"
      ( \s h x ->
          DescribeAnomalyDetectorsResponse'
            <$> ( x .@? "AnomalyDetectors" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (x .@? "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAnomalyDetectors

instance NFData DescribeAnomalyDetectors

instance ToHeaders DescribeAnomalyDetectors where
  toHeaders = const mempty

instance ToPath DescribeAnomalyDetectors where
  toPath = const "/"

instance ToQuery DescribeAnomalyDetectors where
  toQuery DescribeAnomalyDetectors' {..} =
    mconcat
      [ "Action" =: ("DescribeAnomalyDetectors" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "MetricName" =: _dMetricName,
        "Namespace" =: _dNamespace,
        "NextToken" =: _dNextToken,
        "Dimensions" =: toQuery (toQueryList "member" <$> _dDimensions),
        "MaxResults" =: _dMaxResults
      ]

-- | /See:/ 'describeAnomalyDetectorsResponse' smart constructor.
data DescribeAnomalyDetectorsResponse = DescribeAnomalyDetectorsResponse'
  { _dadrsAnomalyDetectors ::
      !( Maybe
           [AnomalyDetector]
       ),
    _dadrsNextToken ::
      !(Maybe Text),
    _dadrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAnomalyDetectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dadrsAnomalyDetectors' - The list of anomaly detection models returned by the operation.
--
-- * 'dadrsNextToken' - A token that you can use in a subsequent operation to retrieve the next set of results.
--
-- * 'dadrsResponseStatus' - -- | The response status code.
describeAnomalyDetectorsResponse ::
  -- | 'dadrsResponseStatus'
  Int ->
  DescribeAnomalyDetectorsResponse
describeAnomalyDetectorsResponse pResponseStatus_ =
  DescribeAnomalyDetectorsResponse'
    { _dadrsAnomalyDetectors =
        Nothing,
      _dadrsNextToken = Nothing,
      _dadrsResponseStatus = pResponseStatus_
    }

-- | The list of anomaly detection models returned by the operation.
dadrsAnomalyDetectors :: Lens' DescribeAnomalyDetectorsResponse [AnomalyDetector]
dadrsAnomalyDetectors = lens _dadrsAnomalyDetectors (\s a -> s {_dadrsAnomalyDetectors = a}) . _Default . _Coerce

-- | A token that you can use in a subsequent operation to retrieve the next set of results.
dadrsNextToken :: Lens' DescribeAnomalyDetectorsResponse (Maybe Text)
dadrsNextToken = lens _dadrsNextToken (\s a -> s {_dadrsNextToken = a})

-- | -- | The response status code.
dadrsResponseStatus :: Lens' DescribeAnomalyDetectorsResponse Int
dadrsResponseStatus = lens _dadrsResponseStatus (\s a -> s {_dadrsResponseStatus = a})

instance NFData DescribeAnomalyDetectorsResponse
