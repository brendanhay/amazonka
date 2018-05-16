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
-- Module      : Network.AWS.CloudWatchLogs.PutMetricFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a metric filter and associates it with the specified log group. Metric filters allow you to configure rules to extract metric data from log events ingested through 'PutLogEvents' .
--
--
-- The maximum number of metric filters that can be associated with a log group is 100.
--
module Network.AWS.CloudWatchLogs.PutMetricFilter
    (
    -- * Creating a Request
      putMetricFilter
    , PutMetricFilter
    -- * Request Lenses
    , pmfLogGroupName
    , pmfFilterName
    , pmfFilterPattern
    , pmfMetricTransformations

    -- * Destructuring the Response
    , putMetricFilterResponse
    , PutMetricFilterResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putMetricFilter' smart constructor.
data PutMetricFilter = PutMetricFilter'
  { _pmfLogGroupName          :: !Text
  , _pmfFilterName            :: !Text
  , _pmfFilterPattern         :: !Text
  , _pmfMetricTransformations :: !(List1 MetricTransformation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMetricFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmfLogGroupName' - The name of the log group.
--
-- * 'pmfFilterName' - A name for the metric filter.
--
-- * 'pmfFilterPattern' - A filter pattern for extracting metric data out of ingested log events.
--
-- * 'pmfMetricTransformations' - A collection of information that defines how metric data gets emitted.
putMetricFilter
    :: Text -- ^ 'pmfLogGroupName'
    -> Text -- ^ 'pmfFilterName'
    -> Text -- ^ 'pmfFilterPattern'
    -> NonEmpty MetricTransformation -- ^ 'pmfMetricTransformations'
    -> PutMetricFilter
putMetricFilter pLogGroupName_ pFilterName_ pFilterPattern_ pMetricTransformations_ =
  PutMetricFilter'
    { _pmfLogGroupName = pLogGroupName_
    , _pmfFilterName = pFilterName_
    , _pmfFilterPattern = pFilterPattern_
    , _pmfMetricTransformations = _List1 # pMetricTransformations_
    }


-- | The name of the log group.
pmfLogGroupName :: Lens' PutMetricFilter Text
pmfLogGroupName = lens _pmfLogGroupName (\ s a -> s{_pmfLogGroupName = a})

-- | A name for the metric filter.
pmfFilterName :: Lens' PutMetricFilter Text
pmfFilterName = lens _pmfFilterName (\ s a -> s{_pmfFilterName = a})

-- | A filter pattern for extracting metric data out of ingested log events.
pmfFilterPattern :: Lens' PutMetricFilter Text
pmfFilterPattern = lens _pmfFilterPattern (\ s a -> s{_pmfFilterPattern = a})

-- | A collection of information that defines how metric data gets emitted.
pmfMetricTransformations :: Lens' PutMetricFilter (NonEmpty MetricTransformation)
pmfMetricTransformations = lens _pmfMetricTransformations (\ s a -> s{_pmfMetricTransformations = a}) . _List1

instance AWSRequest PutMetricFilter where
        type Rs PutMetricFilter = PutMetricFilterResponse
        request = postJSON cloudWatchLogs
        response = receiveNull PutMetricFilterResponse'

instance Hashable PutMetricFilter where

instance NFData PutMetricFilter where

instance ToHeaders PutMetricFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutMetricFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutMetricFilter where
        toJSON PutMetricFilter'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _pmfLogGroupName),
                  Just ("filterName" .= _pmfFilterName),
                  Just ("filterPattern" .= _pmfFilterPattern),
                  Just
                    ("metricTransformations" .=
                       _pmfMetricTransformations)])

instance ToPath PutMetricFilter where
        toPath = const "/"

instance ToQuery PutMetricFilter where
        toQuery = const mempty

-- | /See:/ 'putMetricFilterResponse' smart constructor.
data PutMetricFilterResponse =
  PutMetricFilterResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMetricFilterResponse' with the minimum fields required to make a request.
--
putMetricFilterResponse
    :: PutMetricFilterResponse
putMetricFilterResponse = PutMetricFilterResponse'


instance NFData PutMetricFilterResponse where
