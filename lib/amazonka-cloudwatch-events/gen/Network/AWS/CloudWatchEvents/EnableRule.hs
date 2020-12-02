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
-- Module      : Network.AWS.CloudWatchEvents.EnableRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified rule. If the rule doesn't exist, the operation fails.
--
--
-- When you enable a rule, incoming events might not immediately start matching to a newly enabled rule. Allow a short period of time for changes to take effect.
--
module Network.AWS.CloudWatchEvents.EnableRule
    (
    -- * Creating a Request
      enableRule
    , EnableRule
    -- * Request Lenses
    , erEventBusName
    , erName

    -- * Destructuring the Response
    , enableRuleResponse
    , EnableRuleResponse
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableRule' smart constructor.
data EnableRule = EnableRule'
  { _erEventBusName :: !(Maybe Text)
  , _erName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erEventBusName' - The event bus associated with the rule. If you omit this, the default event bus is used.
--
-- * 'erName' - The name of the rule.
enableRule
    :: Text -- ^ 'erName'
    -> EnableRule
enableRule pName_ = EnableRule' {_erEventBusName = Nothing, _erName = pName_}


-- | The event bus associated with the rule. If you omit this, the default event bus is used.
erEventBusName :: Lens' EnableRule (Maybe Text)
erEventBusName = lens _erEventBusName (\ s a -> s{_erEventBusName = a})

-- | The name of the rule.
erName :: Lens' EnableRule Text
erName = lens _erName (\ s a -> s{_erName = a})

instance AWSRequest EnableRule where
        type Rs EnableRule = EnableRuleResponse
        request = postJSON cloudWatchEvents
        response = receiveNull EnableRuleResponse'

instance Hashable EnableRule where

instance NFData EnableRule where

instance ToHeaders EnableRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.EnableRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableRule where
        toJSON EnableRule'{..}
          = object
              (catMaybes
                 [("EventBusName" .=) <$> _erEventBusName,
                  Just ("Name" .= _erName)])

instance ToPath EnableRule where
        toPath = const "/"

instance ToQuery EnableRule where
        toQuery = const mempty

-- | /See:/ 'enableRuleResponse' smart constructor.
data EnableRuleResponse =
  EnableRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableRuleResponse' with the minimum fields required to make a request.
--
enableRuleResponse
    :: EnableRuleResponse
enableRuleResponse = EnableRuleResponse'


instance NFData EnableRuleResponse where
