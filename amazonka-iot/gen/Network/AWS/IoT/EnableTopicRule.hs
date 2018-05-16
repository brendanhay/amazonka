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
-- Module      : Network.AWS.IoT.EnableTopicRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the rule.
--
--
module Network.AWS.IoT.EnableTopicRule
    (
    -- * Creating a Request
      enableTopicRule
    , EnableTopicRule
    -- * Request Lenses
    , etrRuleName

    -- * Destructuring the Response
    , enableTopicRuleResponse
    , EnableTopicRuleResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the EnableTopicRuleRequest operation.
--
--
--
-- /See:/ 'enableTopicRule' smart constructor.
newtype EnableTopicRule = EnableTopicRule'
  { _etrRuleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etrRuleName' - The name of the topic rule to enable.
enableTopicRule
    :: Text -- ^ 'etrRuleName'
    -> EnableTopicRule
enableTopicRule pRuleName_ = EnableTopicRule' {_etrRuleName = pRuleName_}


-- | The name of the topic rule to enable.
etrRuleName :: Lens' EnableTopicRule Text
etrRuleName = lens _etrRuleName (\ s a -> s{_etrRuleName = a})

instance AWSRequest EnableTopicRule where
        type Rs EnableTopicRule = EnableTopicRuleResponse
        request = postJSON ioT
        response = receiveNull EnableTopicRuleResponse'

instance Hashable EnableTopicRule where

instance NFData EnableTopicRule where

instance ToHeaders EnableTopicRule where
        toHeaders = const mempty

instance ToJSON EnableTopicRule where
        toJSON = const (Object mempty)

instance ToPath EnableTopicRule where
        toPath EnableTopicRule'{..}
          = mconcat ["/rules/", toBS _etrRuleName, "/enable"]

instance ToQuery EnableTopicRule where
        toQuery = const mempty

-- | /See:/ 'enableTopicRuleResponse' smart constructor.
data EnableTopicRuleResponse =
  EnableTopicRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableTopicRuleResponse' with the minimum fields required to make a request.
--
enableTopicRuleResponse
    :: EnableTopicRuleResponse
enableTopicRuleResponse = EnableTopicRuleResponse'


instance NFData EnableTopicRuleResponse where
