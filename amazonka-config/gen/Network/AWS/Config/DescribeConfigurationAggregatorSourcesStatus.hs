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
-- Module      : Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for sources within an aggregator. The status includes information about the last time AWS Config aggregated data from source accounts or AWS Config failed to aggregate data from source accounts with the related error code or message.
--
--
module Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
    (
    -- * Creating a Request
      describeConfigurationAggregatorSourcesStatus
    , DescribeConfigurationAggregatorSourcesStatus
    -- * Request Lenses
    , dcassNextToken
    , dcassLimit
    , dcassUpdateStatus
    , dcassConfigurationAggregatorName

    -- * Destructuring the Response
    , describeConfigurationAggregatorSourcesStatusResponse
    , DescribeConfigurationAggregatorSourcesStatusResponse
    -- * Response Lenses
    , dcassrsAggregatedSourceStatusList
    , dcassrsNextToken
    , dcassrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConfigurationAggregatorSourcesStatus' smart constructor.
data DescribeConfigurationAggregatorSourcesStatus = DescribeConfigurationAggregatorSourcesStatus'
  { _dcassNextToken :: !(Maybe Text)
  , _dcassLimit :: !(Maybe Nat)
  , _dcassUpdateStatus :: !(Maybe (List1 AggregatedSourceStatusType))
  , _dcassConfigurationAggregatorName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationAggregatorSourcesStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcassNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dcassLimit' - The maximum number of AggregatorSourceStatus returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- * 'dcassUpdateStatus' - Filters the status type.     * Valid value FAILED indicates errors while moving data.     * Valid value SUCCEEDED indicates the data was successfully moved.     * Valid value OUTDATED indicates the data is not the most recent.
--
-- * 'dcassConfigurationAggregatorName' - The name of the configuration aggregator.
describeConfigurationAggregatorSourcesStatus
    :: Text -- ^ 'dcassConfigurationAggregatorName'
    -> DescribeConfigurationAggregatorSourcesStatus
describeConfigurationAggregatorSourcesStatus pConfigurationAggregatorName_ =
  DescribeConfigurationAggregatorSourcesStatus'
    { _dcassNextToken = Nothing
    , _dcassLimit = Nothing
    , _dcassUpdateStatus = Nothing
    , _dcassConfigurationAggregatorName = pConfigurationAggregatorName_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dcassNextToken :: Lens' DescribeConfigurationAggregatorSourcesStatus (Maybe Text)
dcassNextToken = lens _dcassNextToken (\ s a -> s{_dcassNextToken = a})

-- | The maximum number of AggregatorSourceStatus returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
dcassLimit :: Lens' DescribeConfigurationAggregatorSourcesStatus (Maybe Natural)
dcassLimit = lens _dcassLimit (\ s a -> s{_dcassLimit = a}) . mapping _Nat

-- | Filters the status type.     * Valid value FAILED indicates errors while moving data.     * Valid value SUCCEEDED indicates the data was successfully moved.     * Valid value OUTDATED indicates the data is not the most recent.
dcassUpdateStatus :: Lens' DescribeConfigurationAggregatorSourcesStatus (Maybe (NonEmpty AggregatedSourceStatusType))
dcassUpdateStatus = lens _dcassUpdateStatus (\ s a -> s{_dcassUpdateStatus = a}) . mapping _List1

-- | The name of the configuration aggregator.
dcassConfigurationAggregatorName :: Lens' DescribeConfigurationAggregatorSourcesStatus Text
dcassConfigurationAggregatorName = lens _dcassConfigurationAggregatorName (\ s a -> s{_dcassConfigurationAggregatorName = a})

instance AWSRequest
           DescribeConfigurationAggregatorSourcesStatus
         where
        type Rs DescribeConfigurationAggregatorSourcesStatus
             =
             DescribeConfigurationAggregatorSourcesStatusResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationAggregatorSourcesStatusResponse'
                   <$>
                   (x .?> "AggregatedSourceStatusList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeConfigurationAggregatorSourcesStatus
         where

instance NFData
           DescribeConfigurationAggregatorSourcesStatus
         where

instance ToHeaders
           DescribeConfigurationAggregatorSourcesStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigurationAggregatorSourcesStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeConfigurationAggregatorSourcesStatus
         where
        toJSON
          DescribeConfigurationAggregatorSourcesStatus'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dcassNextToken,
                  ("Limit" .=) <$> _dcassLimit,
                  ("UpdateStatus" .=) <$> _dcassUpdateStatus,
                  Just
                    ("ConfigurationAggregatorName" .=
                       _dcassConfigurationAggregatorName)])

instance ToPath
           DescribeConfigurationAggregatorSourcesStatus
         where
        toPath = const "/"

instance ToQuery
           DescribeConfigurationAggregatorSourcesStatus
         where
        toQuery = const mempty

-- | /See:/ 'describeConfigurationAggregatorSourcesStatusResponse' smart constructor.
data DescribeConfigurationAggregatorSourcesStatusResponse = DescribeConfigurationAggregatorSourcesStatusResponse'
  { _dcassrsAggregatedSourceStatusList :: !(Maybe [AggregatedSourceStatus])
  , _dcassrsNextToken                  :: !(Maybe Text)
  , _dcassrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationAggregatorSourcesStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcassrsAggregatedSourceStatusList' - Retuns an AggregatedSourceStatus object.
--
-- * 'dcassrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dcassrsResponseStatus' - -- | The response status code.
describeConfigurationAggregatorSourcesStatusResponse
    :: Int -- ^ 'dcassrsResponseStatus'
    -> DescribeConfigurationAggregatorSourcesStatusResponse
describeConfigurationAggregatorSourcesStatusResponse pResponseStatus_ =
  DescribeConfigurationAggregatorSourcesStatusResponse'
    { _dcassrsAggregatedSourceStatusList = Nothing
    , _dcassrsNextToken = Nothing
    , _dcassrsResponseStatus = pResponseStatus_
    }


-- | Retuns an AggregatedSourceStatus object.
dcassrsAggregatedSourceStatusList :: Lens' DescribeConfigurationAggregatorSourcesStatusResponse [AggregatedSourceStatus]
dcassrsAggregatedSourceStatusList = lens _dcassrsAggregatedSourceStatusList (\ s a -> s{_dcassrsAggregatedSourceStatusList = a}) . _Default . _Coerce

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dcassrsNextToken :: Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Maybe Text)
dcassrsNextToken = lens _dcassrsNextToken (\ s a -> s{_dcassrsNextToken = a})

-- | -- | The response status code.
dcassrsResponseStatus :: Lens' DescribeConfigurationAggregatorSourcesStatusResponse Int
dcassrsResponseStatus = lens _dcassrsResponseStatus (\ s a -> s{_dcassrsResponseStatus = a})

instance NFData
           DescribeConfigurationAggregatorSourcesStatusResponse
         where
