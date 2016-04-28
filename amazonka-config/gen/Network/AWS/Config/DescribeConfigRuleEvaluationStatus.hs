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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for each of your AWS managed Config rules.
-- The status includes information such as the last time AWS Config invoked
-- the rule, the last time AWS Config failed to invoke the rule, and the
-- related error for the last failure.
module Network.AWS.Config.DescribeConfigRuleEvaluationStatus
    (
    -- * Creating a Request
      describeConfigRuleEvaluationStatus
    , DescribeConfigRuleEvaluationStatus
    -- * Request Lenses
    , dcresConfigRuleNames

    -- * Destructuring the Response
    , describeConfigRuleEvaluationStatusResponse
    , DescribeConfigRuleEvaluationStatusResponse
    -- * Response Lenses
    , dcresrsConfigRulesEvaluationStatus
    , dcresrsResponseStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeConfigRuleEvaluationStatus' smart constructor.
newtype DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
    { _dcresConfigRuleNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigRuleEvaluationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcresConfigRuleNames'
describeConfigRuleEvaluationStatus
    :: DescribeConfigRuleEvaluationStatus
describeConfigRuleEvaluationStatus =
    DescribeConfigRuleEvaluationStatus'
    { _dcresConfigRuleNames = Nothing
    }

-- | The name of the AWS managed Config rules for which you want status
-- information. If you do not specify any names, AWS Config returns status
-- information for all AWS managed Config rules that you use.
dcresConfigRuleNames :: Lens' DescribeConfigRuleEvaluationStatus [Text]
dcresConfigRuleNames = lens _dcresConfigRuleNames (\ s a -> s{_dcresConfigRuleNames = a}) . _Default . _Coerce;

instance AWSRequest
         DescribeConfigRuleEvaluationStatus where
        type Rs DescribeConfigRuleEvaluationStatus =
             DescribeConfigRuleEvaluationStatusResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigRuleEvaluationStatusResponse' <$>
                   (x .?> "ConfigRulesEvaluationStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeConfigRuleEvaluationStatus

instance NFData DescribeConfigRuleEvaluationStatus

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
                 [("ConfigRuleNames" .=) <$> _dcresConfigRuleNames])

instance ToPath DescribeConfigRuleEvaluationStatus
         where
        toPath = const "/"

instance ToQuery DescribeConfigRuleEvaluationStatus
         where
        toQuery = const mempty

-- | /See:/ 'describeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
    { _dcresrsConfigRulesEvaluationStatus :: !(Maybe [ConfigRuleEvaluationStatus])
    , _dcresrsResponseStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigRuleEvaluationStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcresrsConfigRulesEvaluationStatus'
--
-- * 'dcresrsResponseStatus'
describeConfigRuleEvaluationStatusResponse
    :: Int -- ^ 'dcresrsResponseStatus'
    -> DescribeConfigRuleEvaluationStatusResponse
describeConfigRuleEvaluationStatusResponse pResponseStatus_ =
    DescribeConfigRuleEvaluationStatusResponse'
    { _dcresrsConfigRulesEvaluationStatus = Nothing
    , _dcresrsResponseStatus = pResponseStatus_
    }

-- | Status information about your AWS managed Config rules.
dcresrsConfigRulesEvaluationStatus :: Lens' DescribeConfigRuleEvaluationStatusResponse [ConfigRuleEvaluationStatus]
dcresrsConfigRulesEvaluationStatus = lens _dcresrsConfigRulesEvaluationStatus (\ s a -> s{_dcresrsConfigRulesEvaluationStatus = a}) . _Default . _Coerce;

-- | The response status code.
dcresrsResponseStatus :: Lens' DescribeConfigRuleEvaluationStatusResponse Int
dcresrsResponseStatus = lens _dcresrsResponseStatus (\ s a -> s{_dcresrsResponseStatus = a});
