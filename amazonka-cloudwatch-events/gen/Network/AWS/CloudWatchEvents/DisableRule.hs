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
-- Module      : Network.AWS.CloudWatchEvents.DisableRule
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a rule. A disabled rule won\'t match any events, and won\'t self-trigger if it has a schedule expression.
--
-- __Note:__ When you disable a rule, incoming events might still continue to match to the disabled rule. Please allow a short period of time for changes to take effect.
module Network.AWS.CloudWatchEvents.DisableRule
    (
    -- * Creating a Request
      disableRule
    , DisableRule
    -- * Request Lenses
    , dName

    -- * Destructuring the Response
    , disableRuleResponse
    , DisableRuleResponse
    ) where

import           Network.AWS.CloudWatchEvents.Types
import           Network.AWS.CloudWatchEvents.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the < DisableRule> operation.
--
-- /See:/ 'disableRule' smart constructor.
newtype DisableRule = DisableRule'
    { _dName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName'
disableRule
    :: Text -- ^ 'dName'
    -> DisableRule
disableRule pName_ =
    DisableRule'
    { _dName = pName_
    }

-- | The name of the rule you want to disable.
dName :: Lens' DisableRule Text
dName = lens _dName (\ s a -> s{_dName = a});

instance AWSRequest DisableRule where
        type Rs DisableRule = DisableRuleResponse
        request = postJSON cloudWatchEvents
        response = receiveNull DisableRuleResponse'

instance Hashable DisableRule

instance NFData DisableRule

instance ToHeaders DisableRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DisableRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableRule where
        toJSON DisableRule'{..}
          = object (catMaybes [Just ("Name" .= _dName)])

instance ToPath DisableRule where
        toPath = const "/"

instance ToQuery DisableRule where
        toQuery = const mempty

-- | /See:/ 'disableRuleResponse' smart constructor.
data DisableRuleResponse =
    DisableRuleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableRuleResponse' with the minimum fields required to make a request.
--
disableRuleResponse
    :: DisableRuleResponse
disableRuleResponse = DisableRuleResponse'

instance NFData DisableRuleResponse
