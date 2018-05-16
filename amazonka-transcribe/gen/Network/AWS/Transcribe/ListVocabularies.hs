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
-- Module      : Network.AWS.Transcribe.ListVocabularies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If no criteria are specified, returns the entire list of vocabularies.
--
--
module Network.AWS.Transcribe.ListVocabularies
    (
    -- * Creating a Request
      listVocabularies
    , ListVocabularies
    -- * Request Lenses
    , lvNameContains
    , lvNextToken
    , lvStateEquals
    , lvMaxResults

    -- * Destructuring the Response
    , listVocabulariesResponse
    , ListVocabulariesResponse
    -- * Response Lenses
    , lvrsVocabularies
    , lvrsStatus
    , lvrsNextToken
    , lvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'listVocabularies' smart constructor.
data ListVocabularies = ListVocabularies'
  { _lvNameContains :: !(Maybe Text)
  , _lvNextToken    :: !(Maybe Text)
  , _lvStateEquals  :: !(Maybe VocabularyState)
  , _lvMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVocabularies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvNameContains' - When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is case-insensitive, @ListVocabularies@ will return both "vocabularyname" and "VocabularyName" in the response list.
--
-- * 'lvNextToken' - If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- * 'lvStateEquals' - When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
--
-- * 'lvMaxResults' - The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
listVocabularies
    :: ListVocabularies
listVocabularies =
  ListVocabularies'
    { _lvNameContains = Nothing
    , _lvNextToken = Nothing
    , _lvStateEquals = Nothing
    , _lvMaxResults = Nothing
    }


-- | When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is case-insensitive, @ListVocabularies@ will return both "vocabularyname" and "VocabularyName" in the response list.
lvNameContains :: Lens' ListVocabularies (Maybe Text)
lvNameContains = lens _lvNameContains (\ s a -> s{_lvNameContains = a})

-- | If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
lvNextToken :: Lens' ListVocabularies (Maybe Text)
lvNextToken = lens _lvNextToken (\ s a -> s{_lvNextToken = a})

-- | When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
lvStateEquals :: Lens' ListVocabularies (Maybe VocabularyState)
lvStateEquals = lens _lvStateEquals (\ s a -> s{_lvStateEquals = a})

-- | The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
lvMaxResults :: Lens' ListVocabularies (Maybe Natural)
lvMaxResults = lens _lvMaxResults (\ s a -> s{_lvMaxResults = a}) . mapping _Nat

instance AWSRequest ListVocabularies where
        type Rs ListVocabularies = ListVocabulariesResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 ListVocabulariesResponse' <$>
                   (x .?> "Vocabularies" .!@ mempty) <*>
                     (x .?> "Status")
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListVocabularies where

instance NFData ListVocabularies where

instance ToHeaders ListVocabularies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.ListVocabularies" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListVocabularies where
        toJSON ListVocabularies'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lvNameContains,
                  ("NextToken" .=) <$> _lvNextToken,
                  ("StateEquals" .=) <$> _lvStateEquals,
                  ("MaxResults" .=) <$> _lvMaxResults])

instance ToPath ListVocabularies where
        toPath = const "/"

instance ToQuery ListVocabularies where
        toQuery = const mempty

-- | /See:/ 'listVocabulariesResponse' smart constructor.
data ListVocabulariesResponse = ListVocabulariesResponse'
  { _lvrsVocabularies   :: !(Maybe [VocabularyInfo])
  , _lvrsStatus         :: !(Maybe TranscriptionJobStatus)
  , _lvrsNextToken      :: !(Maybe Text)
  , _lvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVocabulariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrsVocabularies' - A list of objects that describe the vocabularies that match the search criteria in the request.
--
-- * 'lvrsStatus' - The requested vocabulary state.
--
-- * 'lvrsNextToken' - The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListVocabularies@ operation to return in the next page of jobs.
--
-- * 'lvrsResponseStatus' - -- | The response status code.
listVocabulariesResponse
    :: Int -- ^ 'lvrsResponseStatus'
    -> ListVocabulariesResponse
listVocabulariesResponse pResponseStatus_ =
  ListVocabulariesResponse'
    { _lvrsVocabularies = Nothing
    , _lvrsStatus = Nothing
    , _lvrsNextToken = Nothing
    , _lvrsResponseStatus = pResponseStatus_
    }


-- | A list of objects that describe the vocabularies that match the search criteria in the request.
lvrsVocabularies :: Lens' ListVocabulariesResponse [VocabularyInfo]
lvrsVocabularies = lens _lvrsVocabularies (\ s a -> s{_lvrsVocabularies = a}) . _Default . _Coerce

-- | The requested vocabulary state.
lvrsStatus :: Lens' ListVocabulariesResponse (Maybe TranscriptionJobStatus)
lvrsStatus = lens _lvrsStatus (\ s a -> s{_lvrsStatus = a})

-- | The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListVocabularies@ operation to return in the next page of jobs.
lvrsNextToken :: Lens' ListVocabulariesResponse (Maybe Text)
lvrsNextToken = lens _lvrsNextToken (\ s a -> s{_lvrsNextToken = a})

-- | -- | The response status code.
lvrsResponseStatus :: Lens' ListVocabulariesResponse Int
lvrsResponseStatus = lens _lvrsResponseStatus (\ s a -> s{_lvrsResponseStatus = a})

instance NFData ListVocabulariesResponse where
