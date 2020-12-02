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
-- Module      : Network.AWS.Config.DescribeAggregateComplianceByConfigRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of compliant and noncompliant rules with the number of resources for compliant and noncompliant rules.
--
--
module Network.AWS.Config.DescribeAggregateComplianceByConfigRules
    (
    -- * Creating a Request
      describeAggregateComplianceByConfigRules
    , DescribeAggregateComplianceByConfigRules
    -- * Request Lenses
    , dacbcrFilters
    , dacbcrNextToken
    , dacbcrLimit
    , dacbcrConfigurationAggregatorName

    -- * Destructuring the Response
    , describeAggregateComplianceByConfigRulesResponse
    , DescribeAggregateComplianceByConfigRulesResponse
    -- * Response Lenses
    , dacbcrrsNextToken
    , dacbcrrsAggregateComplianceByConfigRules
    , dacbcrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAggregateComplianceByConfigRules' smart constructor.
data DescribeAggregateComplianceByConfigRules = DescribeAggregateComplianceByConfigRules'
  { _dacbcrFilters                     :: !(Maybe ConfigRuleComplianceFilters)
  , _dacbcrNextToken                   :: !(Maybe Text)
  , _dacbcrLimit                       :: !(Maybe Nat)
  , _dacbcrConfigurationAggregatorName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAggregateComplianceByConfigRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacbcrFilters' - Filters the results by ConfigRuleComplianceFilters object.
--
-- * 'dacbcrNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dacbcrLimit' - The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- * 'dacbcrConfigurationAggregatorName' - The name of the configuration aggregator.
describeAggregateComplianceByConfigRules
    :: Text -- ^ 'dacbcrConfigurationAggregatorName'
    -> DescribeAggregateComplianceByConfigRules
describeAggregateComplianceByConfigRules pConfigurationAggregatorName_ =
  DescribeAggregateComplianceByConfigRules'
    { _dacbcrFilters = Nothing
    , _dacbcrNextToken = Nothing
    , _dacbcrLimit = Nothing
    , _dacbcrConfigurationAggregatorName = pConfigurationAggregatorName_
    }


-- | Filters the results by ConfigRuleComplianceFilters object.
dacbcrFilters :: Lens' DescribeAggregateComplianceByConfigRules (Maybe ConfigRuleComplianceFilters)
dacbcrFilters = lens _dacbcrFilters (\ s a -> s{_dacbcrFilters = a})

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dacbcrNextToken :: Lens' DescribeAggregateComplianceByConfigRules (Maybe Text)
dacbcrNextToken = lens _dacbcrNextToken (\ s a -> s{_dacbcrNextToken = a})

-- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
dacbcrLimit :: Lens' DescribeAggregateComplianceByConfigRules (Maybe Natural)
dacbcrLimit = lens _dacbcrLimit (\ s a -> s{_dacbcrLimit = a}) . mapping _Nat

-- | The name of the configuration aggregator.
dacbcrConfigurationAggregatorName :: Lens' DescribeAggregateComplianceByConfigRules Text
dacbcrConfigurationAggregatorName = lens _dacbcrConfigurationAggregatorName (\ s a -> s{_dacbcrConfigurationAggregatorName = a})

instance AWSRequest
           DescribeAggregateComplianceByConfigRules
         where
        type Rs DescribeAggregateComplianceByConfigRules =
             DescribeAggregateComplianceByConfigRulesResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAggregateComplianceByConfigRulesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "AggregateComplianceByConfigRules" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeAggregateComplianceByConfigRules
         where

instance NFData
           DescribeAggregateComplianceByConfigRules
         where

instance ToHeaders
           DescribeAggregateComplianceByConfigRules
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeAggregateComplianceByConfigRules"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeAggregateComplianceByConfigRules
         where
        toJSON DescribeAggregateComplianceByConfigRules'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dacbcrFilters,
                  ("NextToken" .=) <$> _dacbcrNextToken,
                  ("Limit" .=) <$> _dacbcrLimit,
                  Just
                    ("ConfigurationAggregatorName" .=
                       _dacbcrConfigurationAggregatorName)])

instance ToPath
           DescribeAggregateComplianceByConfigRules
         where
        toPath = const "/"

instance ToQuery
           DescribeAggregateComplianceByConfigRules
         where
        toQuery = const mempty

-- | /See:/ 'describeAggregateComplianceByConfigRulesResponse' smart constructor.
data DescribeAggregateComplianceByConfigRulesResponse = DescribeAggregateComplianceByConfigRulesResponse'
  { _dacbcrrsNextToken :: !(Maybe Text)
  , _dacbcrrsAggregateComplianceByConfigRules :: !(Maybe [AggregateComplianceByConfigRule])
  , _dacbcrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAggregateComplianceByConfigRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacbcrrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dacbcrrsAggregateComplianceByConfigRules' - Returns a list of AggregateComplianceByConfigRule object.
--
-- * 'dacbcrrsResponseStatus' - -- | The response status code.
describeAggregateComplianceByConfigRulesResponse
    :: Int -- ^ 'dacbcrrsResponseStatus'
    -> DescribeAggregateComplianceByConfigRulesResponse
describeAggregateComplianceByConfigRulesResponse pResponseStatus_ =
  DescribeAggregateComplianceByConfigRulesResponse'
    { _dacbcrrsNextToken = Nothing
    , _dacbcrrsAggregateComplianceByConfigRules = Nothing
    , _dacbcrrsResponseStatus = pResponseStatus_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dacbcrrsNextToken :: Lens' DescribeAggregateComplianceByConfigRulesResponse (Maybe Text)
dacbcrrsNextToken = lens _dacbcrrsNextToken (\ s a -> s{_dacbcrrsNextToken = a})

-- | Returns a list of AggregateComplianceByConfigRule object.
dacbcrrsAggregateComplianceByConfigRules :: Lens' DescribeAggregateComplianceByConfigRulesResponse [AggregateComplianceByConfigRule]
dacbcrrsAggregateComplianceByConfigRules = lens _dacbcrrsAggregateComplianceByConfigRules (\ s a -> s{_dacbcrrsAggregateComplianceByConfigRules = a}) . _Default . _Coerce

-- | -- | The response status code.
dacbcrrsResponseStatus :: Lens' DescribeAggregateComplianceByConfigRulesResponse Int
dacbcrrsResponseStatus = lens _dacbcrrsResponseStatus (\ s a -> s{_dacbcrrsResponseStatus = a})

instance NFData
           DescribeAggregateComplianceByConfigRulesResponse
         where
