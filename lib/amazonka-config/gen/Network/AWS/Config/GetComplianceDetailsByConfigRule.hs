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
-- Module      : Network.AWS.Config.GetComplianceDetailsByConfigRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS Config rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByConfigRule
    (
    -- * Creating a Request
      getComplianceDetailsByConfigRule
    , GetComplianceDetailsByConfigRule
    -- * Request Lenses
    , gcdbcrComplianceTypes
    , gcdbcrNextToken
    , gcdbcrLimit
    , gcdbcrConfigRuleName

    -- * Destructuring the Response
    , getComplianceDetailsByConfigRuleResponse
    , GetComplianceDetailsByConfigRuleResponse
    -- * Response Lenses
    , gcdbcrrsEvaluationResults
    , gcdbcrrsNextToken
    , gcdbcrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'getComplianceDetailsByConfigRule' smart constructor.
data GetComplianceDetailsByConfigRule = GetComplianceDetailsByConfigRule'
  { _gcdbcrComplianceTypes :: !(Maybe [ComplianceType])
  , _gcdbcrNextToken       :: !(Maybe Text)
  , _gcdbcrLimit           :: !(Maybe Nat)
  , _gcdbcrConfigRuleName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceDetailsByConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdbcrComplianceTypes' - Filters the results by compliance. The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
--
-- * 'gcdbcrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gcdbcrLimit' - The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- * 'gcdbcrConfigRuleName' - The name of the AWS Config rule for which you want compliance information.
getComplianceDetailsByConfigRule
    :: Text -- ^ 'gcdbcrConfigRuleName'
    -> GetComplianceDetailsByConfigRule
getComplianceDetailsByConfigRule pConfigRuleName_ =
  GetComplianceDetailsByConfigRule'
    { _gcdbcrComplianceTypes = Nothing
    , _gcdbcrNextToken = Nothing
    , _gcdbcrLimit = Nothing
    , _gcdbcrConfigRuleName = pConfigRuleName_
    }


-- | Filters the results by compliance. The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
gcdbcrComplianceTypes :: Lens' GetComplianceDetailsByConfigRule [ComplianceType]
gcdbcrComplianceTypes = lens _gcdbcrComplianceTypes (\ s a -> s{_gcdbcrComplianceTypes = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
gcdbcrNextToken :: Lens' GetComplianceDetailsByConfigRule (Maybe Text)
gcdbcrNextToken = lens _gcdbcrNextToken (\ s a -> s{_gcdbcrNextToken = a})

-- | The maximum number of evaluation results returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
gcdbcrLimit :: Lens' GetComplianceDetailsByConfigRule (Maybe Natural)
gcdbcrLimit = lens _gcdbcrLimit (\ s a -> s{_gcdbcrLimit = a}) . mapping _Nat

-- | The name of the AWS Config rule for which you want compliance information.
gcdbcrConfigRuleName :: Lens' GetComplianceDetailsByConfigRule Text
gcdbcrConfigRuleName = lens _gcdbcrConfigRuleName (\ s a -> s{_gcdbcrConfigRuleName = a})

instance AWSPager GetComplianceDetailsByConfigRule
         where
        page rq rs
          | stop (rs ^. gcdbcrrsNextToken) = Nothing
          | stop (rs ^. gcdbcrrsEvaluationResults) = Nothing
          | otherwise =
            Just $ rq &
              gcdbcrNextToken .~ rs ^. gcdbcrrsNextToken

instance AWSRequest GetComplianceDetailsByConfigRule
         where
        type Rs GetComplianceDetailsByConfigRule =
             GetComplianceDetailsByConfigRuleResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetComplianceDetailsByConfigRuleResponse' <$>
                   (x .?> "EvaluationResults" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetComplianceDetailsByConfigRule
         where

instance NFData GetComplianceDetailsByConfigRule
         where

instance ToHeaders GetComplianceDetailsByConfigRule
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetComplianceDetailsByConfigRule"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetComplianceDetailsByConfigRule
         where
        toJSON GetComplianceDetailsByConfigRule'{..}
          = object
              (catMaybes
                 [("ComplianceTypes" .=) <$> _gcdbcrComplianceTypes,
                  ("NextToken" .=) <$> _gcdbcrNextToken,
                  ("Limit" .=) <$> _gcdbcrLimit,
                  Just ("ConfigRuleName" .= _gcdbcrConfigRuleName)])

instance ToPath GetComplianceDetailsByConfigRule
         where
        toPath = const "/"

instance ToQuery GetComplianceDetailsByConfigRule
         where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'getComplianceDetailsByConfigRuleResponse' smart constructor.
data GetComplianceDetailsByConfigRuleResponse = GetComplianceDetailsByConfigRuleResponse'
  { _gcdbcrrsEvaluationResults :: !(Maybe [EvaluationResult])
  , _gcdbcrrsNextToken         :: !(Maybe Text)
  , _gcdbcrrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceDetailsByConfigRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdbcrrsEvaluationResults' - Indicates whether the AWS resource complies with the specified AWS Config rule.
--
-- * 'gcdbcrrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'gcdbcrrsResponseStatus' - -- | The response status code.
getComplianceDetailsByConfigRuleResponse
    :: Int -- ^ 'gcdbcrrsResponseStatus'
    -> GetComplianceDetailsByConfigRuleResponse
getComplianceDetailsByConfigRuleResponse pResponseStatus_ =
  GetComplianceDetailsByConfigRuleResponse'
    { _gcdbcrrsEvaluationResults = Nothing
    , _gcdbcrrsNextToken = Nothing
    , _gcdbcrrsResponseStatus = pResponseStatus_
    }


-- | Indicates whether the AWS resource complies with the specified AWS Config rule.
gcdbcrrsEvaluationResults :: Lens' GetComplianceDetailsByConfigRuleResponse [EvaluationResult]
gcdbcrrsEvaluationResults = lens _gcdbcrrsEvaluationResults (\ s a -> s{_gcdbcrrsEvaluationResults = a}) . _Default . _Coerce

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
gcdbcrrsNextToken :: Lens' GetComplianceDetailsByConfigRuleResponse (Maybe Text)
gcdbcrrsNextToken = lens _gcdbcrrsNextToken (\ s a -> s{_gcdbcrrsNextToken = a})

-- | -- | The response status code.
gcdbcrrsResponseStatus :: Lens' GetComplianceDetailsByConfigRuleResponse Int
gcdbcrrsResponseStatus = lens _gcdbcrrsResponseStatus (\ s a -> s{_gcdbcrrsResponseStatus = a})

instance NFData
           GetComplianceDetailsByConfigRuleResponse
         where
