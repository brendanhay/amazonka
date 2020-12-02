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
-- Module      : Network.AWS.IoT.GetTopicRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the rule.
--
--
module Network.AWS.IoT.GetTopicRule
    (
    -- * Creating a Request
      getTopicRule
    , GetTopicRule
    -- * Request Lenses
    , gtrRuleName

    -- * Destructuring the Response
    , getTopicRuleResponse
    , GetTopicRuleResponse
    -- * Response Lenses
    , gtrrsRule
    , gtrrsRuleARN
    , gtrrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the GetTopicRule operation.
--
--
--
-- /See:/ 'getTopicRule' smart constructor.
newtype GetTopicRule = GetTopicRule'
  { _gtrRuleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrRuleName' - The name of the rule.
getTopicRule
    :: Text -- ^ 'gtrRuleName'
    -> GetTopicRule
getTopicRule pRuleName_ = GetTopicRule' {_gtrRuleName = pRuleName_}


-- | The name of the rule.
gtrRuleName :: Lens' GetTopicRule Text
gtrRuleName = lens _gtrRuleName (\ s a -> s{_gtrRuleName = a})

instance AWSRequest GetTopicRule where
        type Rs GetTopicRule = GetTopicRuleResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetTopicRuleResponse' <$>
                   (x .?> "rule") <*> (x .?> "ruleArn") <*>
                     (pure (fromEnum s)))

instance Hashable GetTopicRule where

instance NFData GetTopicRule where

instance ToHeaders GetTopicRule where
        toHeaders = const mempty

instance ToPath GetTopicRule where
        toPath GetTopicRule'{..}
          = mconcat ["/rules/", toBS _gtrRuleName]

instance ToQuery GetTopicRule where
        toQuery = const mempty

-- | The output from the GetTopicRule operation.
--
--
--
-- /See:/ 'getTopicRuleResponse' smart constructor.
data GetTopicRuleResponse = GetTopicRuleResponse'
  { _gtrrsRule           :: !(Maybe TopicRule)
  , _gtrrsRuleARN        :: !(Maybe Text)
  , _gtrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTopicRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrrsRule' - The rule.
--
-- * 'gtrrsRuleARN' - The rule ARN.
--
-- * 'gtrrsResponseStatus' - -- | The response status code.
getTopicRuleResponse
    :: Int -- ^ 'gtrrsResponseStatus'
    -> GetTopicRuleResponse
getTopicRuleResponse pResponseStatus_ =
  GetTopicRuleResponse'
    { _gtrrsRule = Nothing
    , _gtrrsRuleARN = Nothing
    , _gtrrsResponseStatus = pResponseStatus_
    }


-- | The rule.
gtrrsRule :: Lens' GetTopicRuleResponse (Maybe TopicRule)
gtrrsRule = lens _gtrrsRule (\ s a -> s{_gtrrsRule = a})

-- | The rule ARN.
gtrrsRuleARN :: Lens' GetTopicRuleResponse (Maybe Text)
gtrrsRuleARN = lens _gtrrsRuleARN (\ s a -> s{_gtrrsRuleARN = a})

-- | -- | The response status code.
gtrrsResponseStatus :: Lens' GetTopicRuleResponse Int
gtrrsResponseStatus = lens _gtrrsResponseStatus (\ s a -> s{_gtrrsResponseStatus = a})

instance NFData GetTopicRuleResponse where
