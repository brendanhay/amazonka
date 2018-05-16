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
-- Module      : Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS Config rule for a specific resource in a rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.
--
--
module Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
    (
    -- * Creating a Request
      getAggregateComplianceDetailsByConfigRule
    , GetAggregateComplianceDetailsByConfigRule
    -- * Request Lenses
    , gacdbcrNextToken
    , gacdbcrLimit
    , gacdbcrComplianceType
    , gacdbcrConfigurationAggregatorName
    , gacdbcrConfigRuleName
    , gacdbcrAccountId
    , gacdbcrAWSRegion

    -- * Destructuring the Response
    , getAggregateComplianceDetailsByConfigRuleResponse
    , GetAggregateComplianceDetailsByConfigRuleResponse
    -- * Response Lenses
    , gacdbcrrsNextToken
    , gacdbcrrsAggregateEvaluationResults
    , gacdbcrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAggregateComplianceDetailsByConfigRule' smart constructor.
data GetAggregateComplianceDetailsByConfigRule = GetAggregateComplianceDetailsByConfigRule'
  { _gacdbcrNextToken                   :: !(Maybe Text)
  , _gacdbcrLimit                       :: !(Maybe Nat)
  , _gacdbcrComplianceType              :: !(Maybe ComplianceType)
  , _gacdbcrConfigurationAggregatorName :: !Text
  , _gacdbcrConfigRuleName              :: !Text
  , _gacdbcrAccountId                   :: !Text
  , _gacdbcrAWSRegion                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAggregateComplianceDetailsByConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacdbcrNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gacdbcrLimit' - The maximum number of evaluation results returned on each page. The default is 50. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- * 'gacdbcrComplianceType' - The resource compliance status.
--
-- * 'gacdbcrConfigurationAggregatorName' - The name of the configuration aggregator.
--
-- * 'gacdbcrConfigRuleName' - The name of the AWS Config rule for which you want compliance information.
--
-- * 'gacdbcrAccountId' - The 12-digit account ID of the source account.
--
-- * 'gacdbcrAWSRegion' - The source region from where the data is aggregated.
getAggregateComplianceDetailsByConfigRule
    :: Text -- ^ 'gacdbcrConfigurationAggregatorName'
    -> Text -- ^ 'gacdbcrConfigRuleName'
    -> Text -- ^ 'gacdbcrAccountId'
    -> Text -- ^ 'gacdbcrAWSRegion'
    -> GetAggregateComplianceDetailsByConfigRule
getAggregateComplianceDetailsByConfigRule pConfigurationAggregatorName_ pConfigRuleName_ pAccountId_ pAWSRegion_ =
  GetAggregateComplianceDetailsByConfigRule'
    { _gacdbcrNextToken = Nothing
    , _gacdbcrLimit = Nothing
    , _gacdbcrComplianceType = Nothing
    , _gacdbcrConfigurationAggregatorName = pConfigurationAggregatorName_
    , _gacdbcrConfigRuleName = pConfigRuleName_
    , _gacdbcrAccountId = pAccountId_
    , _gacdbcrAWSRegion = pAWSRegion_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gacdbcrNextToken :: Lens' GetAggregateComplianceDetailsByConfigRule (Maybe Text)
gacdbcrNextToken = lens _gacdbcrNextToken (\ s a -> s{_gacdbcrNextToken = a})

-- | The maximum number of evaluation results returned on each page. The default is 50. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
gacdbcrLimit :: Lens' GetAggregateComplianceDetailsByConfigRule (Maybe Natural)
gacdbcrLimit = lens _gacdbcrLimit (\ s a -> s{_gacdbcrLimit = a}) . mapping _Nat

-- | The resource compliance status.
gacdbcrComplianceType :: Lens' GetAggregateComplianceDetailsByConfigRule (Maybe ComplianceType)
gacdbcrComplianceType = lens _gacdbcrComplianceType (\ s a -> s{_gacdbcrComplianceType = a})

-- | The name of the configuration aggregator.
gacdbcrConfigurationAggregatorName :: Lens' GetAggregateComplianceDetailsByConfigRule Text
gacdbcrConfigurationAggregatorName = lens _gacdbcrConfigurationAggregatorName (\ s a -> s{_gacdbcrConfigurationAggregatorName = a})

-- | The name of the AWS Config rule for which you want compliance information.
gacdbcrConfigRuleName :: Lens' GetAggregateComplianceDetailsByConfigRule Text
gacdbcrConfigRuleName = lens _gacdbcrConfigRuleName (\ s a -> s{_gacdbcrConfigRuleName = a})

-- | The 12-digit account ID of the source account.
gacdbcrAccountId :: Lens' GetAggregateComplianceDetailsByConfigRule Text
gacdbcrAccountId = lens _gacdbcrAccountId (\ s a -> s{_gacdbcrAccountId = a})

-- | The source region from where the data is aggregated.
gacdbcrAWSRegion :: Lens' GetAggregateComplianceDetailsByConfigRule Text
gacdbcrAWSRegion = lens _gacdbcrAWSRegion (\ s a -> s{_gacdbcrAWSRegion = a})

instance AWSRequest
           GetAggregateComplianceDetailsByConfigRule
         where
        type Rs GetAggregateComplianceDetailsByConfigRule =
             GetAggregateComplianceDetailsByConfigRuleResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetAggregateComplianceDetailsByConfigRuleResponse'
                   <$>
                   (x .?> "NextToken") <*>
                     (x .?> "AggregateEvaluationResults" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           GetAggregateComplianceDetailsByConfigRule
         where

instance NFData
           GetAggregateComplianceDetailsByConfigRule
         where

instance ToHeaders
           GetAggregateComplianceDetailsByConfigRule
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetAggregateComplianceDetailsByConfigRule"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           GetAggregateComplianceDetailsByConfigRule
         where
        toJSON GetAggregateComplianceDetailsByConfigRule'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gacdbcrNextToken,
                  ("Limit" .=) <$> _gacdbcrLimit,
                  ("ComplianceType" .=) <$> _gacdbcrComplianceType,
                  Just
                    ("ConfigurationAggregatorName" .=
                       _gacdbcrConfigurationAggregatorName),
                  Just ("ConfigRuleName" .= _gacdbcrConfigRuleName),
                  Just ("AccountId" .= _gacdbcrAccountId),
                  Just ("AwsRegion" .= _gacdbcrAWSRegion)])

instance ToPath
           GetAggregateComplianceDetailsByConfigRule
         where
        toPath = const "/"

instance ToQuery
           GetAggregateComplianceDetailsByConfigRule
         where
        toQuery = const mempty

-- | /See:/ 'getAggregateComplianceDetailsByConfigRuleResponse' smart constructor.
data GetAggregateComplianceDetailsByConfigRuleResponse = GetAggregateComplianceDetailsByConfigRuleResponse'
  { _gacdbcrrsNextToken                  :: !(Maybe Text)
  , _gacdbcrrsAggregateEvaluationResults :: !(Maybe [AggregateEvaluationResult])
  , _gacdbcrrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAggregateComplianceDetailsByConfigRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacdbcrrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gacdbcrrsAggregateEvaluationResults' - Returns an AggregateEvaluationResults object.
--
-- * 'gacdbcrrsResponseStatus' - -- | The response status code.
getAggregateComplianceDetailsByConfigRuleResponse
    :: Int -- ^ 'gacdbcrrsResponseStatus'
    -> GetAggregateComplianceDetailsByConfigRuleResponse
getAggregateComplianceDetailsByConfigRuleResponse pResponseStatus_ =
  GetAggregateComplianceDetailsByConfigRuleResponse'
    { _gacdbcrrsNextToken = Nothing
    , _gacdbcrrsAggregateEvaluationResults = Nothing
    , _gacdbcrrsResponseStatus = pResponseStatus_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gacdbcrrsNextToken :: Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Maybe Text)
gacdbcrrsNextToken = lens _gacdbcrrsNextToken (\ s a -> s{_gacdbcrrsNextToken = a})

-- | Returns an AggregateEvaluationResults object.
gacdbcrrsAggregateEvaluationResults :: Lens' GetAggregateComplianceDetailsByConfigRuleResponse [AggregateEvaluationResult]
gacdbcrrsAggregateEvaluationResults = lens _gacdbcrrsAggregateEvaluationResults (\ s a -> s{_gacdbcrrsAggregateEvaluationResults = a}) . _Default . _Coerce

-- | -- | The response status code.
gacdbcrrsResponseStatus :: Lens' GetAggregateComplianceDetailsByConfigRuleResponse Int
gacdbcrrsResponseStatus = lens _gacdbcrrsResponseStatus (\ s a -> s{_gacdbcrrsResponseStatus = a})

instance NFData
           GetAggregateComplianceDetailsByConfigRuleResponse
         where
