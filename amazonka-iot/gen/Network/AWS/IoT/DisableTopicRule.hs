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
-- Module      : Network.AWS.IoT.DisableTopicRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the rule.
--
--
module Network.AWS.IoT.DisableTopicRule
    (
    -- * Creating a Request
      disableTopicRule
    , DisableTopicRule
    -- * Request Lenses
    , dtrRuleName

    -- * Destructuring the Response
    , disableTopicRuleResponse
    , DisableTopicRuleResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DisableTopicRuleRequest operation.
--
--
--
-- /See:/ 'disableTopicRule' smart constructor.
newtype DisableTopicRule = DisableTopicRule'
  { _dtrRuleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableTopicRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrRuleName' - The name of the rule to disable.
disableTopicRule
    :: Text -- ^ 'dtrRuleName'
    -> DisableTopicRule
disableTopicRule pRuleName_ = DisableTopicRule' {_dtrRuleName = pRuleName_}


-- | The name of the rule to disable.
dtrRuleName :: Lens' DisableTopicRule Text
dtrRuleName = lens _dtrRuleName (\ s a -> s{_dtrRuleName = a})

instance AWSRequest DisableTopicRule where
        type Rs DisableTopicRule = DisableTopicRuleResponse
        request = postJSON ioT
        response = receiveNull DisableTopicRuleResponse'

instance Hashable DisableTopicRule where

instance NFData DisableTopicRule where

instance ToHeaders DisableTopicRule where
        toHeaders = const mempty

instance ToJSON DisableTopicRule where
        toJSON = const (Object mempty)

instance ToPath DisableTopicRule where
        toPath DisableTopicRule'{..}
          = mconcat ["/rules/", toBS _dtrRuleName, "/disable"]

instance ToQuery DisableTopicRule where
        toQuery = const mempty

-- | /See:/ 'disableTopicRuleResponse' smart constructor.
data DisableTopicRuleResponse =
  DisableTopicRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableTopicRuleResponse' with the minimum fields required to make a request.
--
disableTopicRuleResponse
    :: DisableTopicRuleResponse
disableTopicRuleResponse = DisableTopicRuleResponse'


instance NFData DisableTopicRuleResponse where
