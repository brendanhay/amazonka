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
-- Module      : Network.AWS.Config.StartConfigRulesEvaluation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand evaluation for the specified AWS Config rules against the last known configuration state of the resources. Use @StartConfigRulesEvaluation@ when you want to test that a rule you updated is working as expected. @StartConfigRulesEvaluation@ does not re-record the latest configuration state for your resources. It re-runs an evaluation against the last known state of your resources.
--
--
-- You can specify up to 25 AWS Config rules per request.
--
-- An existing @StartConfigRulesEvaluation@ call for the specified rules must complete before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a @ConfigRuleEvaluationStarted@ notification when the evaluation starts.
--
-- The @StartConfigRulesEvaluation@ API is useful if you want to run on-demand evaluations, such as the following example:
--
--     * You have a custom rule that evaluates your IAM resources every 24 hours.
--
--     * You update your Lambda function to add additional conditions to your rule.
--
--     * Instead of waiting for the next periodic evaluation, you call the @StartConfigRulesEvaluation@ API.
--
--     * AWS Config invokes your Lambda function and evaluates your IAM resources.
--
--     * Your custom rule will still run periodic evaluations every 24 hours.
--
--
--
module Network.AWS.Config.StartConfigRulesEvaluation
    (
    -- * Creating a Request
      startConfigRulesEvaluation
    , StartConfigRulesEvaluation
    -- * Request Lenses
    , screConfigRuleNames

    -- * Destructuring the Response
    , startConfigRulesEvaluationResponse
    , StartConfigRulesEvaluationResponse
    -- * Response Lenses
    , scrersResponseStatus
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
-- /See:/ 'startConfigRulesEvaluation' smart constructor.
newtype StartConfigRulesEvaluation = StartConfigRulesEvaluation'
  { _screConfigRuleNames :: Maybe (List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartConfigRulesEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'screConfigRuleNames' - The list of names of AWS Config rules that you want to run evaluations for.
startConfigRulesEvaluation
    :: StartConfigRulesEvaluation
startConfigRulesEvaluation =
  StartConfigRulesEvaluation' {_screConfigRuleNames = Nothing}


-- | The list of names of AWS Config rules that you want to run evaluations for.
screConfigRuleNames :: Lens' StartConfigRulesEvaluation (Maybe (NonEmpty Text))
screConfigRuleNames = lens _screConfigRuleNames (\ s a -> s{_screConfigRuleNames = a}) . mapping _List1

instance AWSRequest StartConfigRulesEvaluation where
        type Rs StartConfigRulesEvaluation =
             StartConfigRulesEvaluationResponse
        request = postJSON config
        response
          = receiveEmpty
              (\ s h x ->
                 StartConfigRulesEvaluationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StartConfigRulesEvaluation where

instance NFData StartConfigRulesEvaluation where

instance ToHeaders StartConfigRulesEvaluation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.StartConfigRulesEvaluation" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartConfigRulesEvaluation where
        toJSON StartConfigRulesEvaluation'{..}
          = object
              (catMaybes
                 [("ConfigRuleNames" .=) <$> _screConfigRuleNames])

instance ToPath StartConfigRulesEvaluation where
        toPath = const "/"

instance ToQuery StartConfigRulesEvaluation where
        toQuery = const mempty

-- | The output when you start the evaluation for the specified AWS Config rule.
--
--
--
-- /See:/ 'startConfigRulesEvaluationResponse' smart constructor.
newtype StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse'
  { _scrersResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartConfigRulesEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrersResponseStatus' - -- | The response status code.
startConfigRulesEvaluationResponse
    :: Int -- ^ 'scrersResponseStatus'
    -> StartConfigRulesEvaluationResponse
startConfigRulesEvaluationResponse pResponseStatus_ =
  StartConfigRulesEvaluationResponse' {_scrersResponseStatus = pResponseStatus_}


-- | -- | The response status code.
scrersResponseStatus :: Lens' StartConfigRulesEvaluationResponse Int
scrersResponseStatus = lens _scrersResponseStatus (\ s a -> s{_scrersResponseStatus = a})

instance NFData StartConfigRulesEvaluationResponse
         where
