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
-- Module      : Network.AWS.SES.ListReceiptRuleSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the receipt rule sets that exist under your AWS account. If there are additional receipt rule sets to be retrieved, you will receive a @NextToken@ that you can provide to the next call to @ListReceiptRuleSets@ to retrieve the additional entries.
--
--
-- For information about managing receipt rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.ListReceiptRuleSets
    (
    -- * Creating a Request
      listReceiptRuleSets
    , ListReceiptRuleSets
    -- * Request Lenses
    , lrrsNextToken

    -- * Destructuring the Response
    , listReceiptRuleSetsResponse
    , ListReceiptRuleSetsResponse
    -- * Response Lenses
    , lrrsrsRuleSets
    , lrrsrsNextToken
    , lrrsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to list the receipt rule sets that exist under your AWS account. You use receipt rule sets to receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'listReceiptRuleSets' smart constructor.
newtype ListReceiptRuleSets = ListReceiptRuleSets'
  { _lrrsNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReceiptRuleSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsNextToken' - A token returned from a previous call to @ListReceiptRuleSets@ to indicate the position in the receipt rule set list.
listReceiptRuleSets
    :: ListReceiptRuleSets
listReceiptRuleSets = ListReceiptRuleSets' {_lrrsNextToken = Nothing}


-- | A token returned from a previous call to @ListReceiptRuleSets@ to indicate the position in the receipt rule set list.
lrrsNextToken :: Lens' ListReceiptRuleSets (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

instance AWSRequest ListReceiptRuleSets where
        type Rs ListReceiptRuleSets =
             ListReceiptRuleSetsResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "ListReceiptRuleSetsResult"
              (\ s h x ->
                 ListReceiptRuleSetsResponse' <$>
                   (x .@? "RuleSets" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListReceiptRuleSets where

instance NFData ListReceiptRuleSets where

instance ToHeaders ListReceiptRuleSets where
        toHeaders = const mempty

instance ToPath ListReceiptRuleSets where
        toPath = const "/"

instance ToQuery ListReceiptRuleSets where
        toQuery ListReceiptRuleSets'{..}
          = mconcat
              ["Action" =: ("ListReceiptRuleSets" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "NextToken" =: _lrrsNextToken]

-- | A list of receipt rule sets that exist under your AWS account.
--
--
--
-- /See:/ 'listReceiptRuleSetsResponse' smart constructor.
data ListReceiptRuleSetsResponse = ListReceiptRuleSetsResponse'
  { _lrrsrsRuleSets       :: !(Maybe [ReceiptRuleSetMetadata])
  , _lrrsrsNextToken      :: !(Maybe Text)
  , _lrrsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReceiptRuleSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsrsRuleSets' - The metadata for the currently active receipt rule set. The metadata consists of the rule set name and the timestamp of when the rule set was created.
--
-- * 'lrrsrsNextToken' - A token indicating that there are additional receipt rule sets available to be listed. Pass this token to successive calls of @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
--
-- * 'lrrsrsResponseStatus' - -- | The response status code.
listReceiptRuleSetsResponse
    :: Int -- ^ 'lrrsrsResponseStatus'
    -> ListReceiptRuleSetsResponse
listReceiptRuleSetsResponse pResponseStatus_ =
  ListReceiptRuleSetsResponse'
    { _lrrsrsRuleSets = Nothing
    , _lrrsrsNextToken = Nothing
    , _lrrsrsResponseStatus = pResponseStatus_
    }


-- | The metadata for the currently active receipt rule set. The metadata consists of the rule set name and the timestamp of when the rule set was created.
lrrsrsRuleSets :: Lens' ListReceiptRuleSetsResponse [ReceiptRuleSetMetadata]
lrrsrsRuleSets = lens _lrrsrsRuleSets (\ s a -> s{_lrrsrsRuleSets = a}) . _Default . _Coerce

-- | A token indicating that there are additional receipt rule sets available to be listed. Pass this token to successive calls of @ListReceiptRuleSets@ to retrieve up to 100 receipt rule sets at a time.
lrrsrsNextToken :: Lens' ListReceiptRuleSetsResponse (Maybe Text)
lrrsrsNextToken = lens _lrrsrsNextToken (\ s a -> s{_lrrsrsNextToken = a})

-- | -- | The response status code.
lrrsrsResponseStatus :: Lens' ListReceiptRuleSetsResponse Int
lrrsrsResponseStatus = lens _lrrsrsResponseStatus (\ s a -> s{_lrrsrsResponseStatus = a})

instance NFData ListReceiptRuleSetsResponse where
