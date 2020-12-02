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
-- Module      : Network.AWS.Config.DescribeComplianceByConfigRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified AWS Config rules are compliant. If a rule is noncompliant, this action returns the number of AWS resources that do not comply with the rule.
--
--
-- A rule is compliant if all of the evaluated resources comply with it. It is noncompliant if any of these resources do not comply.
--
-- If AWS Config has no current evaluation results for the rule, it returns @INSUFFICIENT_DATA@ . This result might indicate one of the following conditions:
--
--     * AWS Config has never invoked an evaluation for the rule. To check whether it has, use the @DescribeConfigRuleEvaluationStatus@ action to get the @LastSuccessfulInvocationTime@ and @LastFailedInvocationTime@ .
--
--     * The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role you assigned to your configuration recorder includes the @config:PutEvaluations@ permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the @config:PutEvaluations@ permission.
--
--     * The rule's AWS Lambda function has returned @NOT_APPLICABLE@ for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeComplianceByConfigRule
    (
    -- * Creating a Request
      describeComplianceByConfigRule
    , DescribeComplianceByConfigRule
    -- * Request Lenses
    , dcbcrConfigRuleNames
    , dcbcrComplianceTypes
    , dcbcrNextToken

    -- * Destructuring the Response
    , describeComplianceByConfigRuleResponse
    , DescribeComplianceByConfigRuleResponse
    -- * Response Lenses
    , dcbcrrsComplianceByConfigRules
    , dcbcrrsNextToken
    , dcbcrrsResponseStatus
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
-- /See:/ 'describeComplianceByConfigRule' smart constructor.
data DescribeComplianceByConfigRule = DescribeComplianceByConfigRule'
  { _dcbcrConfigRuleNames :: !(Maybe [Text])
  , _dcbcrComplianceTypes :: !(Maybe [ComplianceType])
  , _dcbcrNextToken       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComplianceByConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbcrConfigRuleNames' - Specify one or more AWS Config rule names to filter the results by rule.
--
-- * 'dcbcrComplianceTypes' - Filters the results by compliance. The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
--
-- * 'dcbcrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
describeComplianceByConfigRule
    :: DescribeComplianceByConfigRule
describeComplianceByConfigRule =
  DescribeComplianceByConfigRule'
    { _dcbcrConfigRuleNames = Nothing
    , _dcbcrComplianceTypes = Nothing
    , _dcbcrNextToken = Nothing
    }


-- | Specify one or more AWS Config rule names to filter the results by rule.
dcbcrConfigRuleNames :: Lens' DescribeComplianceByConfigRule [Text]
dcbcrConfigRuleNames = lens _dcbcrConfigRuleNames (\ s a -> s{_dcbcrConfigRuleNames = a}) . _Default . _Coerce

-- | Filters the results by compliance. The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ .
dcbcrComplianceTypes :: Lens' DescribeComplianceByConfigRule [ComplianceType]
dcbcrComplianceTypes = lens _dcbcrComplianceTypes (\ s a -> s{_dcbcrComplianceTypes = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
dcbcrNextToken :: Lens' DescribeComplianceByConfigRule (Maybe Text)
dcbcrNextToken = lens _dcbcrNextToken (\ s a -> s{_dcbcrNextToken = a})

instance AWSPager DescribeComplianceByConfigRule
         where
        page rq rs
          | stop (rs ^. dcbcrrsNextToken) = Nothing
          | stop (rs ^. dcbcrrsComplianceByConfigRules) =
            Nothing
          | otherwise =
            Just $ rq & dcbcrNextToken .~ rs ^. dcbcrrsNextToken

instance AWSRequest DescribeComplianceByConfigRule
         where
        type Rs DescribeComplianceByConfigRule =
             DescribeComplianceByConfigRuleResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeComplianceByConfigRuleResponse' <$>
                   (x .?> "ComplianceByConfigRules" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeComplianceByConfigRule
         where

instance NFData DescribeComplianceByConfigRule where

instance ToHeaders DescribeComplianceByConfigRule
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeComplianceByConfigRule"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeComplianceByConfigRule where
        toJSON DescribeComplianceByConfigRule'{..}
          = object
              (catMaybes
                 [("ConfigRuleNames" .=) <$> _dcbcrConfigRuleNames,
                  ("ComplianceTypes" .=) <$> _dcbcrComplianceTypes,
                  ("NextToken" .=) <$> _dcbcrNextToken])

instance ToPath DescribeComplianceByConfigRule where
        toPath = const "/"

instance ToQuery DescribeComplianceByConfigRule where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeComplianceByConfigRuleResponse' smart constructor.
data DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse'
  { _dcbcrrsComplianceByConfigRules :: !(Maybe [ComplianceByConfigRule])
  , _dcbcrrsNextToken               :: !(Maybe Text)
  , _dcbcrrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeComplianceByConfigRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbcrrsComplianceByConfigRules' - Indicates whether each of the specified AWS Config rules is compliant.
--
-- * 'dcbcrrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'dcbcrrsResponseStatus' - -- | The response status code.
describeComplianceByConfigRuleResponse
    :: Int -- ^ 'dcbcrrsResponseStatus'
    -> DescribeComplianceByConfigRuleResponse
describeComplianceByConfigRuleResponse pResponseStatus_ =
  DescribeComplianceByConfigRuleResponse'
    { _dcbcrrsComplianceByConfigRules = Nothing
    , _dcbcrrsNextToken = Nothing
    , _dcbcrrsResponseStatus = pResponseStatus_
    }


-- | Indicates whether each of the specified AWS Config rules is compliant.
dcbcrrsComplianceByConfigRules :: Lens' DescribeComplianceByConfigRuleResponse [ComplianceByConfigRule]
dcbcrrsComplianceByConfigRules = lens _dcbcrrsComplianceByConfigRules (\ s a -> s{_dcbcrrsComplianceByConfigRules = a}) . _Default . _Coerce

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
dcbcrrsNextToken :: Lens' DescribeComplianceByConfigRuleResponse (Maybe Text)
dcbcrrsNextToken = lens _dcbcrrsNextToken (\ s a -> s{_dcbcrrsNextToken = a})

-- | -- | The response status code.
dcbcrrsResponseStatus :: Lens' DescribeComplianceByConfigRuleResponse Int
dcbcrrsResponseStatus = lens _dcbcrrsResponseStatus (\ s a -> s{_dcbcrrsResponseStatus = a})

instance NFData
           DescribeComplianceByConfigRuleResponse
         where
