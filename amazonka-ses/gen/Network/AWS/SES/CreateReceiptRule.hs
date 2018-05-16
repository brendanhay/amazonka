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
-- Module      : Network.AWS.SES.CreateReceiptRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a receipt rule.
--
--
-- For information about setting up receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.CreateReceiptRule
    (
    -- * Creating a Request
      createReceiptRule
    , CreateReceiptRule
    -- * Request Lenses
    , crrAfter
    , crrRuleSetName
    , crrRule

    -- * Destructuring the Response
    , createReceiptRuleResponse
    , CreateReceiptRuleResponse
    -- * Response Lenses
    , crrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to create a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'createReceiptRule' smart constructor.
data CreateReceiptRule = CreateReceiptRule'
  { _crrAfter       :: !(Maybe Text)
  , _crrRuleSetName :: !Text
  , _crrRule        :: !ReceiptRule
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReceiptRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrAfter' - The name of an existing rule after which the new rule will be placed. If this parameter is null, the new rule will be inserted at the beginning of the rule list.
--
-- * 'crrRuleSetName' - The name of the rule set that the receipt rule will be added to.
--
-- * 'crrRule' - A data structure that contains the specified rule's name, actions, recipients, domains, enabled status, scan status, and TLS policy.
createReceiptRule
    :: Text -- ^ 'crrRuleSetName'
    -> ReceiptRule -- ^ 'crrRule'
    -> CreateReceiptRule
createReceiptRule pRuleSetName_ pRule_ =
  CreateReceiptRule'
    {_crrAfter = Nothing, _crrRuleSetName = pRuleSetName_, _crrRule = pRule_}


-- | The name of an existing rule after which the new rule will be placed. If this parameter is null, the new rule will be inserted at the beginning of the rule list.
crrAfter :: Lens' CreateReceiptRule (Maybe Text)
crrAfter = lens _crrAfter (\ s a -> s{_crrAfter = a})

-- | The name of the rule set that the receipt rule will be added to.
crrRuleSetName :: Lens' CreateReceiptRule Text
crrRuleSetName = lens _crrRuleSetName (\ s a -> s{_crrRuleSetName = a})

-- | A data structure that contains the specified rule's name, actions, recipients, domains, enabled status, scan status, and TLS policy.
crrRule :: Lens' CreateReceiptRule ReceiptRule
crrRule = lens _crrRule (\ s a -> s{_crrRule = a})

instance AWSRequest CreateReceiptRule where
        type Rs CreateReceiptRule = CreateReceiptRuleResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "CreateReceiptRuleResult"
              (\ s h x ->
                 CreateReceiptRuleResponse' <$> (pure (fromEnum s)))

instance Hashable CreateReceiptRule where

instance NFData CreateReceiptRule where

instance ToHeaders CreateReceiptRule where
        toHeaders = const mempty

instance ToPath CreateReceiptRule where
        toPath = const "/"

instance ToQuery CreateReceiptRule where
        toQuery CreateReceiptRule'{..}
          = mconcat
              ["Action" =: ("CreateReceiptRule" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "After" =: _crrAfter,
               "RuleSetName" =: _crrRuleSetName, "Rule" =: _crrRule]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'createReceiptRuleResponse' smart constructor.
newtype CreateReceiptRuleResponse = CreateReceiptRuleResponse'
  { _crrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReceiptRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrrsResponseStatus' - -- | The response status code.
createReceiptRuleResponse
    :: Int -- ^ 'crrrsResponseStatus'
    -> CreateReceiptRuleResponse
createReceiptRuleResponse pResponseStatus_ =
  CreateReceiptRuleResponse' {_crrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crrrsResponseStatus :: Lens' CreateReceiptRuleResponse Int
crrrsResponseStatus = lens _crrrsResponseStatus (\ s a -> s{_crrrsResponseStatus = a})

instance NFData CreateReceiptRuleResponse where
