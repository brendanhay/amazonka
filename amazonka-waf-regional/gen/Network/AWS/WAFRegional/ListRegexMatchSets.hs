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
-- Module      : Network.AWS.WAFRegional.ListRegexMatchSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RegexMatchSetSummary' objects.
--
--
module Network.AWS.WAFRegional.ListRegexMatchSets
    (
    -- * Creating a Request
      listRegexMatchSets
    , ListRegexMatchSets
    -- * Request Lenses
    , lrmsNextMarker
    , lrmsLimit

    -- * Destructuring the Response
    , listRegexMatchSetsResponse
    , ListRegexMatchSetsResponse
    -- * Response Lenses
    , lrmsrsRegexMatchSets
    , lrmsrsNextMarker
    , lrmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listRegexMatchSets' smart constructor.
data ListRegexMatchSets = ListRegexMatchSets'
  { _lrmsNextMarker :: !(Maybe Text)
  , _lrmsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRegexMatchSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrmsNextMarker' - If you specify a value for @Limit@ and you have more @RegexMatchSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListRegexMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexMatchSet@ objects.
--
-- * 'lrmsLimit' - Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to return for this request. If you have more @RegexMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexMatchSet@ objects.
listRegexMatchSets
    :: ListRegexMatchSets
listRegexMatchSets =
  ListRegexMatchSets' {_lrmsNextMarker = Nothing, _lrmsLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @RegexMatchSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListRegexMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexMatchSet@ objects.
lrmsNextMarker :: Lens' ListRegexMatchSets (Maybe Text)
lrmsNextMarker = lens _lrmsNextMarker (\ s a -> s{_lrmsNextMarker = a})

-- | Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to return for this request. If you have more @RegexMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexMatchSet@ objects.
lrmsLimit :: Lens' ListRegexMatchSets (Maybe Natural)
lrmsLimit = lens _lrmsLimit (\ s a -> s{_lrmsLimit = a}) . mapping _Nat

instance AWSRequest ListRegexMatchSets where
        type Rs ListRegexMatchSets =
             ListRegexMatchSetsResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListRegexMatchSetsResponse' <$>
                   (x .?> "RegexMatchSets" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListRegexMatchSets where

instance NFData ListRegexMatchSets where

instance ToHeaders ListRegexMatchSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListRegexMatchSets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRegexMatchSets where
        toJSON ListRegexMatchSets'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lrmsNextMarker,
                  ("Limit" .=) <$> _lrmsLimit])

instance ToPath ListRegexMatchSets where
        toPath = const "/"

instance ToQuery ListRegexMatchSets where
        toQuery = const mempty

-- | /See:/ 'listRegexMatchSetsResponse' smart constructor.
data ListRegexMatchSetsResponse = ListRegexMatchSetsResponse'
  { _lrmsrsRegexMatchSets :: !(Maybe [RegexMatchSetSummary])
  , _lrmsrsNextMarker     :: !(Maybe Text)
  , _lrmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRegexMatchSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrmsrsRegexMatchSets' - An array of 'RegexMatchSetSummary' objects.
--
-- * 'lrmsrsNextMarker' - If you have more @RegexMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another @ListRegexMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lrmsrsResponseStatus' - -- | The response status code.
listRegexMatchSetsResponse
    :: Int -- ^ 'lrmsrsResponseStatus'
    -> ListRegexMatchSetsResponse
listRegexMatchSetsResponse pResponseStatus_ =
  ListRegexMatchSetsResponse'
    { _lrmsrsRegexMatchSets = Nothing
    , _lrmsrsNextMarker = Nothing
    , _lrmsrsResponseStatus = pResponseStatus_
    }


-- | An array of 'RegexMatchSetSummary' objects.
lrmsrsRegexMatchSets :: Lens' ListRegexMatchSetsResponse [RegexMatchSetSummary]
lrmsrsRegexMatchSets = lens _lrmsrsRegexMatchSets (\ s a -> s{_lrmsrsRegexMatchSets = a}) . _Default . _Coerce

-- | If you have more @RegexMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another @ListRegexMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lrmsrsNextMarker :: Lens' ListRegexMatchSetsResponse (Maybe Text)
lrmsrsNextMarker = lens _lrmsrsNextMarker (\ s a -> s{_lrmsrsNextMarker = a})

-- | -- | The response status code.
lrmsrsResponseStatus :: Lens' ListRegexMatchSetsResponse Int
lrmsrsResponseStatus = lens _lrmsrsResponseStatus (\ s a -> s{_lrmsrsResponseStatus = a})

instance NFData ListRegexMatchSetsResponse where
