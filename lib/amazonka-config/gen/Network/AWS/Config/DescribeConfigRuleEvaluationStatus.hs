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
-- Module      : Network.AWS.Config.DescribeConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for each of your AWS managed Config rules. The status includes information such as the last time AWS Config invoked the rule, the last time AWS Config failed to invoke the rule, and the related error for the last failure.
--
--
module Network.AWS.Config.DescribeConfigRuleEvaluationStatus
    (
    -- * Creating a Request
      describeConfigRuleEvaluationStatus
    , DescribeConfigRuleEvaluationStatus
    -- * Request Lenses
    , dcresConfigRuleNames
    , dcresNextToken
    , dcresLimit

    -- * Destructuring the Response
    , describeConfigRuleEvaluationStatusResponse
    , DescribeConfigRuleEvaluationStatusResponse
    -- * Response Lenses
    , dcresrsConfigRulesEvaluationStatus
    , dcresrsNextToken
    , dcresrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeConfigRuleEvaluationStatus' smart constructor.
data DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
  { _dcresConfigRuleNames :: !(Maybe [Text])
  , _dcresNextToken       :: !(Maybe Text)
  , _dcresLimit           :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcresConfigRuleNames' - The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
--
-- * 'dcresNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dcresLimit' - The number of rule evaluation results that you want returned. This parameter is required if the rule limit for your account is more than the default of 50 rules. For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
describeConfigRuleEvaluationStatus
    :: DescribeConfigRuleEvaluationStatus
describeConfigRuleEvaluationStatus =
  DescribeConfigRuleEvaluationStatus'
    { _dcresConfigRuleNames = Nothing
    , _dcresNextToken = Nothing
    , _dcresLimit = Nothing
    }


-- | The name of the AWS managed Config rules for which you want status information. If you do not specify any names, AWS Config returns status information for all AWS managed Config rules that you use.
dcresConfigRuleNames :: Lens' DescribeConfigRuleEvaluationStatus [Text]
dcresConfigRuleNames = lens _dcresConfigRuleNames (\ s a -> s{_dcresConfigRuleNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
dcresNextToken :: Lens' DescribeConfigRuleEvaluationStatus (Maybe Text)
dcresNextToken = lens _dcresNextToken (\ s a -> s{_dcresNextToken = a})

-- | The number of rule evaluation results that you want returned. This parameter is required if the rule limit for your account is more than the default of 50 rules. For information about requesting a rule limit increase, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits> in the /AWS General Reference Guide/ .
dcresLimit :: Lens' DescribeConfigRuleEvaluationStatus (Maybe Natural)
dcresLimit = lens _dcresLimit (\ s a -> s{_dcresLimit = a}) . mapping _Nat

instance AWSRequest
           DescribeConfigRuleEvaluationStatus
         where
        type Rs DescribeConfigRuleEvaluationStatus =
             DescribeConfigRuleEvaluationStatusResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigRuleEvaluationStatusResponse' <$>
                   (x .?> "ConfigRulesEvaluationStatus" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigRuleEvaluationStatus
         where

instance NFData DescribeConfigRuleEvaluationStatus
         where

instance ToHeaders DescribeConfigRuleEvaluationStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigRuleEvaluationStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigRuleEvaluationStatus
         where
        toJSON DescribeConfigRuleEvaluationStatus'{..}
          = object
              (catMaybes
                 [("ConfigRuleNames" .=) <$> _dcresConfigRuleNames,
                  ("NextToken" .=) <$> _dcresNextToken,
                  ("Limit" .=) <$> _dcresLimit])

instance ToPath DescribeConfigRuleEvaluationStatus
         where
        toPath = const "/"

instance ToQuery DescribeConfigRuleEvaluationStatus
         where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
  { _dcresrsConfigRulesEvaluationStatus :: !(Maybe [ConfigRuleEvaluationStatus])
  , _dcresrsNextToken                   :: !(Maybe Text)
  , _dcresrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigRuleEvaluationStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcresrsConfigRulesEvaluationStatus' - Status information about your AWS managed Config rules.
--
-- * 'dcresrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'dcresrsResponseStatus' - -- | The response status code.
describeConfigRuleEvaluationStatusResponse
    :: Int -- ^ 'dcresrsResponseStatus'
    -> DescribeConfigRuleEvaluationStatusResponse
describeConfigRuleEvaluationStatusResponse pResponseStatus_ =
  DescribeConfigRuleEvaluationStatusResponse'
    { _dcresrsConfigRulesEvaluationStatus = Nothing
    , _dcresrsNextToken = Nothing
    , _dcresrsResponseStatus = pResponseStatus_
    }


-- | Status information about your AWS managed Config rules.
dcresrsConfigRulesEvaluationStatus :: Lens' DescribeConfigRuleEvaluationStatusResponse [ConfigRuleEvaluationStatus]
dcresrsConfigRulesEvaluationStatus = lens _dcresrsConfigRulesEvaluationStatus (\ s a -> s{_dcresrsConfigRulesEvaluationStatus = a}) . _Default . _Coerce

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
dcresrsNextToken :: Lens' DescribeConfigRuleEvaluationStatusResponse (Maybe Text)
dcresrsNextToken = lens _dcresrsNextToken (\ s a -> s{_dcresrsNextToken = a})

-- | -- | The response status code.
dcresrsResponseStatus :: Lens' DescribeConfigRuleEvaluationStatusResponse Int
dcresrsResponseStatus = lens _dcresrsResponseStatus (\ s a -> s{_dcresrsResponseStatus = a})

instance NFData
           DescribeConfigRuleEvaluationStatusResponse
         where
