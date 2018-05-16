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
-- Module      : Network.AWS.Polly.ListLexicons
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pronunciation lexicons stored in an AWS Region. For more information, see <http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
--
module Network.AWS.Polly.ListLexicons
    (
    -- * Creating a Request
      listLexicons
    , ListLexicons
    -- * Request Lenses
    , llNextToken

    -- * Destructuring the Response
    , listLexiconsResponse
    , ListLexiconsResponse
    -- * Response Lenses
    , llrsLexicons
    , llrsNextToken
    , llrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLexicons' smart constructor.
newtype ListLexicons = ListLexicons'
  { _llNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLexicons' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llNextToken' - An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
listLexicons
    :: ListLexicons
listLexicons = ListLexicons' {_llNextToken = Nothing}


-- | An opaque pagination token returned from previous @ListLexicons@ operation. If present, indicates where to continue the list of lexicons.
llNextToken :: Lens' ListLexicons (Maybe Text)
llNextToken = lens _llNextToken (\ s a -> s{_llNextToken = a})

instance AWSRequest ListLexicons where
        type Rs ListLexicons = ListLexiconsResponse
        request = get polly
        response
          = receiveJSON
              (\ s h x ->
                 ListLexiconsResponse' <$>
                   (x .?> "Lexicons" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListLexicons where

instance NFData ListLexicons where

instance ToHeaders ListLexicons where
        toHeaders = const mempty

instance ToPath ListLexicons where
        toPath = const "/v1/lexicons"

instance ToQuery ListLexicons where
        toQuery ListLexicons'{..}
          = mconcat ["NextToken" =: _llNextToken]

-- | /See:/ 'listLexiconsResponse' smart constructor.
data ListLexiconsResponse = ListLexiconsResponse'
  { _llrsLexicons       :: !(Maybe [LexiconDescription])
  , _llrsNextToken      :: !(Maybe Text)
  , _llrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLexiconsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llrsLexicons' - A list of lexicon names and attributes.
--
-- * 'llrsNextToken' - The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
--
-- * 'llrsResponseStatus' - -- | The response status code.
listLexiconsResponse
    :: Int -- ^ 'llrsResponseStatus'
    -> ListLexiconsResponse
listLexiconsResponse pResponseStatus_ =
  ListLexiconsResponse'
    { _llrsLexicons = Nothing
    , _llrsNextToken = Nothing
    , _llrsResponseStatus = pResponseStatus_
    }


-- | A list of lexicon names and attributes.
llrsLexicons :: Lens' ListLexiconsResponse [LexiconDescription]
llrsLexicons = lens _llrsLexicons (\ s a -> s{_llrsLexicons = a}) . _Default . _Coerce

-- | The pagination token to use in the next request to continue the listing of lexicons. @NextToken@ is returned only if the response is truncated.
llrsNextToken :: Lens' ListLexiconsResponse (Maybe Text)
llrsNextToken = lens _llrsNextToken (\ s a -> s{_llrsNextToken = a})

-- | -- | The response status code.
llrsResponseStatus :: Lens' ListLexiconsResponse Int
llrsResponseStatus = lens _llrsResponseStatus (\ s a -> s{_llrsResponseStatus = a})

instance NFData ListLexiconsResponse where
