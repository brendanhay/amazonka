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
-- Module      : Network.AWS.IoT.ReplaceTopicRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the rule. You must specify all parameters for the new rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.
--
--
module Network.AWS.IoT.ReplaceTopicRule
    (
    -- * Creating a Request
      replaceTopicRule
    , ReplaceTopicRule
    -- * Request Lenses
    , rtrRuleName
    , rtrTopicRulePayload

    -- * Destructuring the Response
    , replaceTopicRuleResponse
    , ReplaceTopicRuleResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ReplaceTopicRule operation.
--
--
--
-- /See:/ 'replaceTopicRule' smart constructor.
data ReplaceTopicRule = ReplaceTopicRule'
  { _rtrRuleName         :: !Text
  , _rtrTopicRulePayload :: !TopicRulePayload
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrRuleName' - The name of the rule.
--
-- * 'rtrTopicRulePayload' - The rule payload.
replaceTopicRule
    :: Text -- ^ 'rtrRuleName'
    -> TopicRulePayload -- ^ 'rtrTopicRulePayload'
    -> ReplaceTopicRule
replaceTopicRule pRuleName_ pTopicRulePayload_ =
  ReplaceTopicRule'
    {_rtrRuleName = pRuleName_, _rtrTopicRulePayload = pTopicRulePayload_}


-- | The name of the rule.
rtrRuleName :: Lens' ReplaceTopicRule Text
rtrRuleName = lens _rtrRuleName (\ s a -> s{_rtrRuleName = a})

-- | The rule payload.
rtrTopicRulePayload :: Lens' ReplaceTopicRule TopicRulePayload
rtrTopicRulePayload = lens _rtrTopicRulePayload (\ s a -> s{_rtrTopicRulePayload = a})

instance AWSRequest ReplaceTopicRule where
        type Rs ReplaceTopicRule = ReplaceTopicRuleResponse
        request = patchJSON ioT
        response = receiveNull ReplaceTopicRuleResponse'

instance Hashable ReplaceTopicRule where

instance NFData ReplaceTopicRule where

instance ToHeaders ReplaceTopicRule where
        toHeaders = const mempty

instance ToJSON ReplaceTopicRule where
        toJSON ReplaceTopicRule'{..}
          = object
              (catMaybes
                 [Just ("topicRulePayload" .= _rtrTopicRulePayload)])

instance ToPath ReplaceTopicRule where
        toPath ReplaceTopicRule'{..}
          = mconcat ["/rules/", toBS _rtrRuleName]

instance ToQuery ReplaceTopicRule where
        toQuery = const mempty

-- | /See:/ 'replaceTopicRuleResponse' smart constructor.
data ReplaceTopicRuleResponse =
  ReplaceTopicRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceTopicRuleResponse' with the minimum fields required to make a request.
--
replaceTopicRuleResponse
    :: ReplaceTopicRuleResponse
replaceTopicRuleResponse = ReplaceTopicRuleResponse'


instance NFData ReplaceTopicRuleResponse where
