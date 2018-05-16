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
-- Module      : Network.AWS.WAF.ListRegexPatternSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RegexPatternSetSummary' objects.
--
--
module Network.AWS.WAF.ListRegexPatternSets
    (
    -- * Creating a Request
      listRegexPatternSets
    , ListRegexPatternSets
    -- * Request Lenses
    , lrpsNextMarker
    , lrpsLimit

    -- * Destructuring the Response
    , listRegexPatternSetsResponse
    , ListRegexPatternSetsResponse
    -- * Response Lenses
    , lrpsrsRegexPatternSets
    , lrpsrsNextMarker
    , lrpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'listRegexPatternSets' smart constructor.
data ListRegexPatternSets = ListRegexPatternSets'
  { _lrpsNextMarker :: !(Maybe Text)
  , _lrpsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRegexPatternSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpsNextMarker' - If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
--
-- * 'lrpsLimit' - Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
listRegexPatternSets
    :: ListRegexPatternSets
listRegexPatternSets =
  ListRegexPatternSets' {_lrpsNextMarker = Nothing, _lrpsLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @RegexPatternSet@ objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @RegexPatternSet@ objects. For the second and subsequent @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @RegexPatternSet@ objects.
lrpsNextMarker :: Lens' ListRegexPatternSets (Maybe Text)
lrpsNextMarker = lens _lrpsNextMarker (\ s a -> s{_lrpsNextMarker = a})

-- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF to return for this request. If you have more @RegexPatternSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @RegexPatternSet@ objects.
lrpsLimit :: Lens' ListRegexPatternSets (Maybe Natural)
lrpsLimit = lens _lrpsLimit (\ s a -> s{_lrpsLimit = a}) . mapping _Nat

instance AWSRequest ListRegexPatternSets where
        type Rs ListRegexPatternSets =
             ListRegexPatternSetsResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 ListRegexPatternSetsResponse' <$>
                   (x .?> "RegexPatternSets" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListRegexPatternSets where

instance NFData ListRegexPatternSets where

instance ToHeaders ListRegexPatternSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.ListRegexPatternSets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRegexPatternSets where
        toJSON ListRegexPatternSets'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lrpsNextMarker,
                  ("Limit" .=) <$> _lrpsLimit])

instance ToPath ListRegexPatternSets where
        toPath = const "/"

instance ToQuery ListRegexPatternSets where
        toQuery = const mempty

-- | /See:/ 'listRegexPatternSetsResponse' smart constructor.
data ListRegexPatternSetsResponse = ListRegexPatternSetsResponse'
  { _lrpsrsRegexPatternSets :: !(Maybe [RegexPatternSetSummary])
  , _lrpsrsNextMarker       :: !(Maybe Text)
  , _lrpsrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRegexPatternSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpsrsRegexPatternSets' - An array of 'RegexPatternSetSummary' objects.
--
-- * 'lrpsrsNextMarker' - If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lrpsrsResponseStatus' - -- | The response status code.
listRegexPatternSetsResponse
    :: Int -- ^ 'lrpsrsResponseStatus'
    -> ListRegexPatternSetsResponse
listRegexPatternSetsResponse pResponseStatus_ =
  ListRegexPatternSetsResponse'
    { _lrpsrsRegexPatternSets = Nothing
    , _lrpsrsNextMarker = Nothing
    , _lrpsrsResponseStatus = pResponseStatus_
    }


-- | An array of 'RegexPatternSetSummary' objects.
lrpsrsRegexPatternSets :: Lens' ListRegexPatternSetsResponse [RegexPatternSetSummary]
lrpsrsRegexPatternSets = lens _lrpsrsRegexPatternSets (\ s a -> s{_lrpsrsRegexPatternSets = a}) . _Default . _Coerce

-- | If you have more @RegexPatternSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @RegexPatternSet@ objects, submit another @ListRegexPatternSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lrpsrsNextMarker :: Lens' ListRegexPatternSetsResponse (Maybe Text)
lrpsrsNextMarker = lens _lrpsrsNextMarker (\ s a -> s{_lrpsrsNextMarker = a})

-- | -- | The response status code.
lrpsrsResponseStatus :: Lens' ListRegexPatternSetsResponse Int
lrpsrsResponseStatus = lens _lrpsrsResponseStatus (\ s a -> s{_lrpsrsResponseStatus = a})

instance NFData ListRegexPatternSetsResponse where
