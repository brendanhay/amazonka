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
-- Module      : Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of compliant and noncompliant rules for one or more accounts and regions in an aggregator.
--
--
module Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
    (
    -- * Creating a Request
      getAggregateConfigRuleComplianceSummary
    , GetAggregateConfigRuleComplianceSummary
    -- * Request Lenses
    , gacrcsFilters
    , gacrcsNextToken
    , gacrcsLimit
    , gacrcsGroupByKey
    , gacrcsConfigurationAggregatorName

    -- * Destructuring the Response
    , getAggregateConfigRuleComplianceSummaryResponse
    , GetAggregateConfigRuleComplianceSummaryResponse
    -- * Response Lenses
    , gacrcsrsAggregateComplianceCounts
    , gacrcsrsNextToken
    , gacrcsrsGroupByKey
    , gacrcsrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAggregateConfigRuleComplianceSummary' smart constructor.
data GetAggregateConfigRuleComplianceSummary = GetAggregateConfigRuleComplianceSummary'
  { _gacrcsFilters :: !(Maybe ConfigRuleComplianceSummaryFilters)
  , _gacrcsNextToken :: !(Maybe Text)
  , _gacrcsLimit :: !(Maybe Nat)
  , _gacrcsGroupByKey :: !(Maybe ConfigRuleComplianceSummaryGroupKey)
  , _gacrcsConfigurationAggregatorName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAggregateConfigRuleComplianceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacrcsFilters' - Filters the results based on the ConfigRuleComplianceSummaryFilters object.
--
-- * 'gacrcsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gacrcsLimit' - The maximum number of evaluation results returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- * 'gacrcsGroupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- * 'gacrcsConfigurationAggregatorName' - The name of the configuration aggregator.
getAggregateConfigRuleComplianceSummary
    :: Text -- ^ 'gacrcsConfigurationAggregatorName'
    -> GetAggregateConfigRuleComplianceSummary
getAggregateConfigRuleComplianceSummary pConfigurationAggregatorName_ =
  GetAggregateConfigRuleComplianceSummary'
    { _gacrcsFilters = Nothing
    , _gacrcsNextToken = Nothing
    , _gacrcsLimit = Nothing
    , _gacrcsGroupByKey = Nothing
    , _gacrcsConfigurationAggregatorName = pConfigurationAggregatorName_
    }


-- | Filters the results based on the ConfigRuleComplianceSummaryFilters object.
gacrcsFilters :: Lens' GetAggregateConfigRuleComplianceSummary (Maybe ConfigRuleComplianceSummaryFilters)
gacrcsFilters = lens _gacrcsFilters (\ s a -> s{_gacrcsFilters = a})

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gacrcsNextToken :: Lens' GetAggregateConfigRuleComplianceSummary (Maybe Text)
gacrcsNextToken = lens _gacrcsNextToken (\ s a -> s{_gacrcsNextToken = a})

-- | The maximum number of evaluation results returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
gacrcsLimit :: Lens' GetAggregateConfigRuleComplianceSummary (Maybe Natural)
gacrcsLimit = lens _gacrcsLimit (\ s a -> s{_gacrcsLimit = a}) . mapping _Nat

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
gacrcsGroupByKey :: Lens' GetAggregateConfigRuleComplianceSummary (Maybe ConfigRuleComplianceSummaryGroupKey)
gacrcsGroupByKey = lens _gacrcsGroupByKey (\ s a -> s{_gacrcsGroupByKey = a})

-- | The name of the configuration aggregator.
gacrcsConfigurationAggregatorName :: Lens' GetAggregateConfigRuleComplianceSummary Text
gacrcsConfigurationAggregatorName = lens _gacrcsConfigurationAggregatorName (\ s a -> s{_gacrcsConfigurationAggregatorName = a})

instance AWSRequest
           GetAggregateConfigRuleComplianceSummary
         where
        type Rs GetAggregateConfigRuleComplianceSummary =
             GetAggregateConfigRuleComplianceSummaryResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetAggregateConfigRuleComplianceSummaryResponse' <$>
                   (x .?> "AggregateComplianceCounts" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "GroupByKey")
                     <*> (pure (fromEnum s)))

instance Hashable
           GetAggregateConfigRuleComplianceSummary
         where

instance NFData
           GetAggregateConfigRuleComplianceSummary
         where

instance ToHeaders
           GetAggregateConfigRuleComplianceSummary
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetAggregateConfigRuleComplianceSummary"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           GetAggregateConfigRuleComplianceSummary
         where
        toJSON GetAggregateConfigRuleComplianceSummary'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _gacrcsFilters,
                  ("NextToken" .=) <$> _gacrcsNextToken,
                  ("Limit" .=) <$> _gacrcsLimit,
                  ("GroupByKey" .=) <$> _gacrcsGroupByKey,
                  Just
                    ("ConfigurationAggregatorName" .=
                       _gacrcsConfigurationAggregatorName)])

instance ToPath
           GetAggregateConfigRuleComplianceSummary
         where
        toPath = const "/"

instance ToQuery
           GetAggregateConfigRuleComplianceSummary
         where
        toQuery = const mempty

-- | /See:/ 'getAggregateConfigRuleComplianceSummaryResponse' smart constructor.
data GetAggregateConfigRuleComplianceSummaryResponse = GetAggregateConfigRuleComplianceSummaryResponse'
  { _gacrcsrsAggregateComplianceCounts :: !(Maybe [AggregateComplianceCount])
  , _gacrcsrsNextToken                 :: !(Maybe Text)
  , _gacrcsrsGroupByKey                :: !(Maybe Text)
  , _gacrcsrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAggregateConfigRuleComplianceSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacrcsrsAggregateComplianceCounts' - Returns a list of AggregateComplianceCounts object.
--
-- * 'gacrcsrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gacrcsrsGroupByKey' - Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- * 'gacrcsrsResponseStatus' - -- | The response status code.
getAggregateConfigRuleComplianceSummaryResponse
    :: Int -- ^ 'gacrcsrsResponseStatus'
    -> GetAggregateConfigRuleComplianceSummaryResponse
getAggregateConfigRuleComplianceSummaryResponse pResponseStatus_ =
  GetAggregateConfigRuleComplianceSummaryResponse'
    { _gacrcsrsAggregateComplianceCounts = Nothing
    , _gacrcsrsNextToken = Nothing
    , _gacrcsrsGroupByKey = Nothing
    , _gacrcsrsResponseStatus = pResponseStatus_
    }


-- | Returns a list of AggregateComplianceCounts object.
gacrcsrsAggregateComplianceCounts :: Lens' GetAggregateConfigRuleComplianceSummaryResponse [AggregateComplianceCount]
gacrcsrsAggregateComplianceCounts = lens _gacrcsrsAggregateComplianceCounts (\ s a -> s{_gacrcsrsAggregateComplianceCounts = a}) . _Default . _Coerce

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gacrcsrsNextToken :: Lens' GetAggregateConfigRuleComplianceSummaryResponse (Maybe Text)
gacrcsrsNextToken = lens _gacrcsrsNextToken (\ s a -> s{_gacrcsrsNextToken = a})

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
gacrcsrsGroupByKey :: Lens' GetAggregateConfigRuleComplianceSummaryResponse (Maybe Text)
gacrcsrsGroupByKey = lens _gacrcsrsGroupByKey (\ s a -> s{_gacrcsrsGroupByKey = a})

-- | -- | The response status code.
gacrcsrsResponseStatus :: Lens' GetAggregateConfigRuleComplianceSummaryResponse Int
gacrcsrsResponseStatus = lens _gacrcsrsResponseStatus (\ s a -> s{_gacrcsrsResponseStatus = a})

instance NFData
           GetAggregateConfigRuleComplianceSummaryResponse
         where
