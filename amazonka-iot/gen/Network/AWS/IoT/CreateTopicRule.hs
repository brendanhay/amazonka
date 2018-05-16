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
-- Module      : Network.AWS.IoT.CreateTopicRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.
--
--
module Network.AWS.IoT.CreateTopicRule
    (
    -- * Creating a Request
      createTopicRule
    , CreateTopicRule
    -- * Request Lenses
    , ctrRuleName
    , ctrTopicRulePayload

    -- * Destructuring the Response
    , createTopicRuleResponse
    , CreateTopicRuleResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreateTopicRule operation.
--
--
--
-- /See:/ 'createTopicRule' smart constructor.
data CreateTopicRule = CreateTopicRule'
  { _ctrRuleName         :: !Text
  , _ctrTopicRulePayload :: !TopicRulePayload
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrRuleName' - The name of the rule.
--
-- * 'ctrTopicRulePayload' - The rule payload.
createTopicRule
    :: Text -- ^ 'ctrRuleName'
    -> TopicRulePayload -- ^ 'ctrTopicRulePayload'
    -> CreateTopicRule
createTopicRule pRuleName_ pTopicRulePayload_ =
  CreateTopicRule'
    {_ctrRuleName = pRuleName_, _ctrTopicRulePayload = pTopicRulePayload_}


-- | The name of the rule.
ctrRuleName :: Lens' CreateTopicRule Text
ctrRuleName = lens _ctrRuleName (\ s a -> s{_ctrRuleName = a})

-- | The rule payload.
ctrTopicRulePayload :: Lens' CreateTopicRule TopicRulePayload
ctrTopicRulePayload = lens _ctrTopicRulePayload (\ s a -> s{_ctrTopicRulePayload = a})

instance AWSRequest CreateTopicRule where
        type Rs CreateTopicRule = CreateTopicRuleResponse
        request = postJSON ioT
        response = receiveNull CreateTopicRuleResponse'

instance Hashable CreateTopicRule where

instance NFData CreateTopicRule where

instance ToHeaders CreateTopicRule where
        toHeaders = const mempty

instance ToJSON CreateTopicRule where
        toJSON CreateTopicRule'{..}
          = object
              (catMaybes
                 [Just ("topicRulePayload" .= _ctrTopicRulePayload)])

instance ToPath CreateTopicRule where
        toPath CreateTopicRule'{..}
          = mconcat ["/rules/", toBS _ctrRuleName]

instance ToQuery CreateTopicRule where
        toQuery = const mempty

-- | /See:/ 'createTopicRuleResponse' smart constructor.
data CreateTopicRuleResponse =
  CreateTopicRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTopicRuleResponse' with the minimum fields required to make a request.
--
createTopicRuleResponse
    :: CreateTopicRuleResponse
createTopicRuleResponse = CreateTopicRuleResponse'


instance NFData CreateTopicRuleResponse where
