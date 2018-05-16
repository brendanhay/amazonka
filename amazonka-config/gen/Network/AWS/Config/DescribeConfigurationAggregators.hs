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
-- Module      : Network.AWS.Config.DescribeConfigurationAggregators
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more configuration aggregators. If the configuration aggregator is not specified, this action returns the details for all the configuration aggregators associated with the account.
--
--
module Network.AWS.Config.DescribeConfigurationAggregators
    (
    -- * Creating a Request
      describeConfigurationAggregators
    , DescribeConfigurationAggregators
    -- * Request Lenses
    , dcaNextToken
    , dcaLimit
    , dcaConfigurationAggregatorNames

    -- * Destructuring the Response
    , describeConfigurationAggregatorsResponse
    , DescribeConfigurationAggregatorsResponse
    -- * Response Lenses
    , dcarsNextToken
    , dcarsConfigurationAggregators
    , dcarsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConfigurationAggregators' smart constructor.
data DescribeConfigurationAggregators = DescribeConfigurationAggregators'
  { _dcaNextToken                    :: !(Maybe Text)
  , _dcaLimit                        :: !(Maybe Nat)
  , _dcaConfigurationAggregatorNames :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationAggregators' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dcaLimit' - The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- * 'dcaConfigurationAggregatorNames' - The name of the configuration aggregators.
describeConfigurationAggregators
    :: DescribeConfigurationAggregators
describeConfigurationAggregators =
  DescribeConfigurationAggregators'
    { _dcaNextToken = Nothing
    , _dcaLimit = Nothing
    , _dcaConfigurationAggregatorNames = Nothing
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dcaNextToken :: Lens' DescribeConfigurationAggregators (Maybe Text)
dcaNextToken = lens _dcaNextToken (\ s a -> s{_dcaNextToken = a})

-- | The maximum number of configuration aggregators returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
dcaLimit :: Lens' DescribeConfigurationAggregators (Maybe Natural)
dcaLimit = lens _dcaLimit (\ s a -> s{_dcaLimit = a}) . mapping _Nat

-- | The name of the configuration aggregators.
dcaConfigurationAggregatorNames :: Lens' DescribeConfigurationAggregators [Text]
dcaConfigurationAggregatorNames = lens _dcaConfigurationAggregatorNames (\ s a -> s{_dcaConfigurationAggregatorNames = a}) . _Default . _Coerce

instance AWSRequest DescribeConfigurationAggregators
         where
        type Rs DescribeConfigurationAggregators =
             DescribeConfigurationAggregatorsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationAggregatorsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ConfigurationAggregators" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigurationAggregators
         where

instance NFData DescribeConfigurationAggregators
         where

instance ToHeaders DescribeConfigurationAggregators
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigurationAggregators"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigurationAggregators
         where
        toJSON DescribeConfigurationAggregators'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dcaNextToken,
                  ("Limit" .=) <$> _dcaLimit,
                  ("ConfigurationAggregatorNames" .=) <$>
                    _dcaConfigurationAggregatorNames])

instance ToPath DescribeConfigurationAggregators
         where
        toPath = const "/"

instance ToQuery DescribeConfigurationAggregators
         where
        toQuery = const mempty

-- | /See:/ 'describeConfigurationAggregatorsResponse' smart constructor.
data DescribeConfigurationAggregatorsResponse = DescribeConfigurationAggregatorsResponse'
  { _dcarsNextToken                :: !(Maybe Text)
  , _dcarsConfigurationAggregators :: !(Maybe [ConfigurationAggregator])
  , _dcarsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationAggregatorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcarsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dcarsConfigurationAggregators' - Returns a ConfigurationAggregators object.
--
-- * 'dcarsResponseStatus' - -- | The response status code.
describeConfigurationAggregatorsResponse
    :: Int -- ^ 'dcarsResponseStatus'
    -> DescribeConfigurationAggregatorsResponse
describeConfigurationAggregatorsResponse pResponseStatus_ =
  DescribeConfigurationAggregatorsResponse'
    { _dcarsNextToken = Nothing
    , _dcarsConfigurationAggregators = Nothing
    , _dcarsResponseStatus = pResponseStatus_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dcarsNextToken :: Lens' DescribeConfigurationAggregatorsResponse (Maybe Text)
dcarsNextToken = lens _dcarsNextToken (\ s a -> s{_dcarsNextToken = a})

-- | Returns a ConfigurationAggregators object.
dcarsConfigurationAggregators :: Lens' DescribeConfigurationAggregatorsResponse [ConfigurationAggregator]
dcarsConfigurationAggregators = lens _dcarsConfigurationAggregators (\ s a -> s{_dcarsConfigurationAggregators = a}) . _Default . _Coerce

-- | -- | The response status code.
dcarsResponseStatus :: Lens' DescribeConfigurationAggregatorsResponse Int
dcarsResponseStatus = lens _dcarsResponseStatus (\ s a -> s{_dcarsResponseStatus = a})

instance NFData
           DescribeConfigurationAggregatorsResponse
         where
