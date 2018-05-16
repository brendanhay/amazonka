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
-- Module      : Network.AWS.SES.DeleteReceiptRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified receipt rule.
--
--
-- For information about managing receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteReceiptRule
    (
    -- * Creating a Request
      deleteReceiptRule
    , DeleteReceiptRule
    -- * Request Lenses
    , delRuleSetName
    , delRuleName

    -- * Destructuring the Response
    , deleteReceiptRuleResponse
    , DeleteReceiptRuleResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'deleteReceiptRule' smart constructor.
data DeleteReceiptRule = DeleteReceiptRule'
  { _delRuleSetName :: !Text
  , _delRuleName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReceiptRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delRuleSetName' - The name of the receipt rule set that contains the receipt rule to delete.
--
-- * 'delRuleName' - The name of the receipt rule to delete.
deleteReceiptRule
    :: Text -- ^ 'delRuleSetName'
    -> Text -- ^ 'delRuleName'
    -> DeleteReceiptRule
deleteReceiptRule pRuleSetName_ pRuleName_ =
  DeleteReceiptRule'
    {_delRuleSetName = pRuleSetName_, _delRuleName = pRuleName_}


-- | The name of the receipt rule set that contains the receipt rule to delete.
delRuleSetName :: Lens' DeleteReceiptRule Text
delRuleSetName = lens _delRuleSetName (\ s a -> s{_delRuleSetName = a})

-- | The name of the receipt rule to delete.
delRuleName :: Lens' DeleteReceiptRule Text
delRuleName = lens _delRuleName (\ s a -> s{_delRuleName = a})

instance AWSRequest DeleteReceiptRule where
        type Rs DeleteReceiptRule = DeleteReceiptRuleResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DeleteReceiptRuleResult"
              (\ s h x ->
                 DeleteReceiptRuleResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteReceiptRule where

instance NFData DeleteReceiptRule where

instance ToHeaders DeleteReceiptRule where
        toHeaders = const mempty

instance ToPath DeleteReceiptRule where
        toPath = const "/"

instance ToQuery DeleteReceiptRule where
        toQuery DeleteReceiptRule'{..}
          = mconcat
              ["Action" =: ("DeleteReceiptRule" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RuleSetName" =: _delRuleSetName,
               "RuleName" =: _delRuleName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'deleteReceiptRuleResponse' smart constructor.
newtype DeleteReceiptRuleResponse = DeleteReceiptRuleResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReceiptRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteReceiptRuleResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteReceiptRuleResponse
deleteReceiptRuleResponse pResponseStatus_ =
  DeleteReceiptRuleResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteReceiptRuleResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteReceiptRuleResponse where
