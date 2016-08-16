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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates your resources against the specified Config rules. You can specify up to 25 Config rules per request.
--
-- An existing < StartConfigRulesEvaluation> call must complete for the rules that you specified before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a notification when the evaluation starts.
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

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startConfigRulesEvaluation' smart constructor.
newtype StartConfigRulesEvaluation = StartConfigRulesEvaluation'
    { _screConfigRuleNames :: Maybe (List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartConfigRulesEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'screConfigRuleNames'
startConfigRulesEvaluation
    :: StartConfigRulesEvaluation
startConfigRulesEvaluation =
    StartConfigRulesEvaluation'
    { _screConfigRuleNames = Nothing
    }

-- | The list of names of Config rules that you want to run evaluations for.
screConfigRuleNames :: Lens' StartConfigRulesEvaluation (Maybe (NonEmpty Text))
screConfigRuleNames = lens _screConfigRuleNames (\ s a -> s{_screConfigRuleNames = a}) . mapping _List1;

instance AWSRequest StartConfigRulesEvaluation where
        type Rs StartConfigRulesEvaluation =
             StartConfigRulesEvaluationResponse
        request = postJSON config
        response
          = receiveEmpty
              (\ s h x ->
                 StartConfigRulesEvaluationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StartConfigRulesEvaluation

instance NFData StartConfigRulesEvaluation

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

-- | The output when you start the evaluation for the specified Config rule.
--
-- /See:/ 'startConfigRulesEvaluationResponse' smart constructor.
newtype StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse'
    { _scrersResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartConfigRulesEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrersResponseStatus'
startConfigRulesEvaluationResponse
    :: Int -- ^ 'scrersResponseStatus'
    -> StartConfigRulesEvaluationResponse
startConfigRulesEvaluationResponse pResponseStatus_ =
    StartConfigRulesEvaluationResponse'
    { _scrersResponseStatus = pResponseStatus_
    }

-- | The response status code.
scrersResponseStatus :: Lens' StartConfigRulesEvaluationResponse Int
scrersResponseStatus = lens _scrersResponseStatus (\ s a -> s{_scrersResponseStatus = a});

instance NFData StartConfigRulesEvaluationResponse
