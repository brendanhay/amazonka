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
-- Module      : Network.AWS.SES.DescribeReceiptRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified receipt rule.
--
--
-- For information about setting up receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DescribeReceiptRule
    (
    -- * Creating a Request
      describeReceiptRule
    , DescribeReceiptRule
    -- * Request Lenses
    , drrRuleSetName
    , drrRuleName

    -- * Destructuring the Response
    , describeReceiptRuleResponse
    , DescribeReceiptRuleResponse
    -- * Response Lenses
    , drrrsRule
    , drrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to return the details of a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'describeReceiptRule' smart constructor.
data DescribeReceiptRule = DescribeReceiptRule'
  { _drrRuleSetName :: !Text
  , _drrRuleName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReceiptRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrRuleSetName' - The name of the receipt rule set that the receipt rule belongs to.
--
-- * 'drrRuleName' - The name of the receipt rule.
describeReceiptRule
    :: Text -- ^ 'drrRuleSetName'
    -> Text -- ^ 'drrRuleName'
    -> DescribeReceiptRule
describeReceiptRule pRuleSetName_ pRuleName_ =
  DescribeReceiptRule'
    {_drrRuleSetName = pRuleSetName_, _drrRuleName = pRuleName_}


-- | The name of the receipt rule set that the receipt rule belongs to.
drrRuleSetName :: Lens' DescribeReceiptRule Text
drrRuleSetName = lens _drrRuleSetName (\ s a -> s{_drrRuleSetName = a})

-- | The name of the receipt rule.
drrRuleName :: Lens' DescribeReceiptRule Text
drrRuleName = lens _drrRuleName (\ s a -> s{_drrRuleName = a})

instance AWSRequest DescribeReceiptRule where
        type Rs DescribeReceiptRule =
             DescribeReceiptRuleResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DescribeReceiptRuleResult"
              (\ s h x ->
                 DescribeReceiptRuleResponse' <$>
                   (x .@? "Rule") <*> (pure (fromEnum s)))

instance Hashable DescribeReceiptRule where

instance NFData DescribeReceiptRule where

instance ToHeaders DescribeReceiptRule where
        toHeaders = const mempty

instance ToPath DescribeReceiptRule where
        toPath = const "/"

instance ToQuery DescribeReceiptRule where
        toQuery DescribeReceiptRule'{..}
          = mconcat
              ["Action" =: ("DescribeReceiptRule" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _drrRuleSetName,
               "RuleName" =: _drrRuleName]

-- | Represents the details of a receipt rule.
--
--
--
-- /See:/ 'describeReceiptRuleResponse' smart constructor.
data DescribeReceiptRuleResponse = DescribeReceiptRuleResponse'
  { _drrrsRule           :: !(Maybe ReceiptRule)
  , _drrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReceiptRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrrsRule' - A data structure that contains the specified receipt rule's name, actions, recipients, domains, enabled status, scan status, and Transport Layer Security (TLS) policy.
--
-- * 'drrrsResponseStatus' - -- | The response status code.
describeReceiptRuleResponse
    :: Int -- ^ 'drrrsResponseStatus'
    -> DescribeReceiptRuleResponse
describeReceiptRuleResponse pResponseStatus_ =
  DescribeReceiptRuleResponse'
    {_drrrsRule = Nothing, _drrrsResponseStatus = pResponseStatus_}


-- | A data structure that contains the specified receipt rule's name, actions, recipients, domains, enabled status, scan status, and Transport Layer Security (TLS) policy.
drrrsRule :: Lens' DescribeReceiptRuleResponse (Maybe ReceiptRule)
drrrsRule = lens _drrrsRule (\ s a -> s{_drrrsRule = a})

-- | -- | The response status code.
drrrsResponseStatus :: Lens' DescribeReceiptRuleResponse Int
drrrsResponseStatus = lens _drrrsResponseStatus (\ s a -> s{_drrrsResponseStatus = a})

instance NFData DescribeReceiptRuleResponse where
