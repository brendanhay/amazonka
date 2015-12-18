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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified rule.
--
-- /See:/ <https://aws.amazon.com/iot#GetTopicRule.html AWS API Reference> for GetTopicRule.
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
    , gtrrsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the GetTopicRule operation.
--
-- /See:/ 'getTopicRule' smart constructor.
newtype GetTopicRule = GetTopicRule'
    { _gtrRuleName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrRuleName'
getTopicRule
    :: Text -- ^ 'gtrRuleName'
    -> GetTopicRule
getTopicRule pRuleName_ =
    GetTopicRule'
    { _gtrRuleName = pRuleName_
    }

-- | The name of the rule.
gtrRuleName :: Lens' GetTopicRule Text
gtrRuleName = lens _gtrRuleName (\ s a -> s{_gtrRuleName = a});

instance AWSRequest GetTopicRule where
        type Rs GetTopicRule = GetTopicRuleResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 GetTopicRuleResponse' <$>
                   (x .?> "rule") <*> (pure (fromEnum s)))

instance ToHeaders GetTopicRule where
        toHeaders = const mempty

instance ToPath GetTopicRule where
        toPath GetTopicRule'{..}
          = mconcat ["/rules/", toBS _gtrRuleName]

instance ToQuery GetTopicRule where
        toQuery = const mempty

-- | The output from the GetTopicRule operation.
--
-- /See:/ 'getTopicRuleResponse' smart constructor.
data GetTopicRuleResponse = GetTopicRuleResponse'
    { _gtrrsRule           :: !(Maybe TopicRule)
    , _gtrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTopicRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrrsRule'
--
-- * 'gtrrsResponseStatus'
getTopicRuleResponse
    :: Int -- ^ 'gtrrsResponseStatus'
    -> GetTopicRuleResponse
getTopicRuleResponse pResponseStatus_ =
    GetTopicRuleResponse'
    { _gtrrsRule = Nothing
    , _gtrrsResponseStatus = pResponseStatus_
    }

-- | The rule.
gtrrsRule :: Lens' GetTopicRuleResponse (Maybe TopicRule)
gtrrsRule = lens _gtrrsRule (\ s a -> s{_gtrrsRule = a});

-- | The response status code.
gtrrsResponseStatus :: Lens' GetTopicRuleResponse Int
gtrrsResponseStatus = lens _gtrrsResponseStatus (\ s a -> s{_gtrrsResponseStatus = a});
