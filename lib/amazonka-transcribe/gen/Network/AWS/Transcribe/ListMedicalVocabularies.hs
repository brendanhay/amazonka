{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListMedicalVocabularies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If you don't enter a value in any of the request parameters, returns the entire list of vocabularies.
module Network.AWS.Transcribe.ListMedicalVocabularies
  ( -- * Creating a Request
    listMedicalVocabularies,
    ListMedicalVocabularies,

    -- * Request Lenses
    lmvNameContains,
    lmvNextToken,
    lmvStateEquals,
    lmvMaxResults,

    -- * Destructuring the Response
    listMedicalVocabulariesResponse,
    ListMedicalVocabulariesResponse,

    -- * Response Lenses
    lmvrsVocabularies,
    lmvrsStatus,
    lmvrsNextToken,
    lmvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'listMedicalVocabularies' smart constructor.
data ListMedicalVocabularies = ListMedicalVocabularies'
  { _lmvNameContains ::
      !(Maybe Text),
    _lmvNextToken :: !(Maybe Text),
    _lmvStateEquals :: !(Maybe VocabularyState),
    _lmvMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMedicalVocabularies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmvNameContains' - Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
--
-- * 'lmvNextToken' - If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
--
-- * 'lmvStateEquals' - When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
--
-- * 'lmvMaxResults' - The maximum number of vocabularies to return in the response.
listMedicalVocabularies ::
  ListMedicalVocabularies
listMedicalVocabularies =
  ListMedicalVocabularies'
    { _lmvNameContains = Nothing,
      _lmvNextToken = Nothing,
      _lmvStateEquals = Nothing,
      _lmvMaxResults = Nothing
    }

-- | Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
lmvNameContains :: Lens' ListMedicalVocabularies (Maybe Text)
lmvNameContains = lens _lmvNameContains (\s a -> s {_lmvNameContains = a})

-- | If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
lmvNextToken :: Lens' ListMedicalVocabularies (Maybe Text)
lmvNextToken = lens _lmvNextToken (\s a -> s {_lmvNextToken = a})

-- | When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
lmvStateEquals :: Lens' ListMedicalVocabularies (Maybe VocabularyState)
lmvStateEquals = lens _lmvStateEquals (\s a -> s {_lmvStateEquals = a})

-- | The maximum number of vocabularies to return in the response.
lmvMaxResults :: Lens' ListMedicalVocabularies (Maybe Natural)
lmvMaxResults = lens _lmvMaxResults (\s a -> s {_lmvMaxResults = a}) . mapping _Nat

instance AWSRequest ListMedicalVocabularies where
  type Rs ListMedicalVocabularies = ListMedicalVocabulariesResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          ListMedicalVocabulariesResponse'
            <$> (x .?> "Vocabularies" .!@ mempty)
            <*> (x .?> "Status")
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListMedicalVocabularies

instance NFData ListMedicalVocabularies

instance ToHeaders ListMedicalVocabularies where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.ListMedicalVocabularies" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListMedicalVocabularies where
  toJSON ListMedicalVocabularies' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lmvNameContains,
            ("NextToken" .=) <$> _lmvNextToken,
            ("StateEquals" .=) <$> _lmvStateEquals,
            ("MaxResults" .=) <$> _lmvMaxResults
          ]
      )

instance ToPath ListMedicalVocabularies where
  toPath = const "/"

instance ToQuery ListMedicalVocabularies where
  toQuery = const mempty

-- | /See:/ 'listMedicalVocabulariesResponse' smart constructor.
data ListMedicalVocabulariesResponse = ListMedicalVocabulariesResponse'
  { _lmvrsVocabularies ::
      !(Maybe [VocabularyInfo]),
    _lmvrsStatus ::
      !(Maybe VocabularyState),
    _lmvrsNextToken ::
      !(Maybe Text),
    _lmvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMedicalVocabulariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmvrsVocabularies' - A list of objects that describe the vocabularies that match your search criteria.
--
-- * 'lmvrsStatus' - The requested vocabulary state.
--
-- * 'lmvrsNextToken' - The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
--
-- * 'lmvrsResponseStatus' - -- | The response status code.
listMedicalVocabulariesResponse ::
  -- | 'lmvrsResponseStatus'
  Int ->
  ListMedicalVocabulariesResponse
listMedicalVocabulariesResponse pResponseStatus_ =
  ListMedicalVocabulariesResponse'
    { _lmvrsVocabularies = Nothing,
      _lmvrsStatus = Nothing,
      _lmvrsNextToken = Nothing,
      _lmvrsResponseStatus = pResponseStatus_
    }

-- | A list of objects that describe the vocabularies that match your search criteria.
lmvrsVocabularies :: Lens' ListMedicalVocabulariesResponse [VocabularyInfo]
lmvrsVocabularies = lens _lmvrsVocabularies (\s a -> s {_lmvrsVocabularies = a}) . _Default . _Coerce

-- | The requested vocabulary state.
lmvrsStatus :: Lens' ListMedicalVocabulariesResponse (Maybe VocabularyState)
lmvrsStatus = lens _lmvrsStatus (\s a -> s {_lmvrsStatus = a})

-- | The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
lmvrsNextToken :: Lens' ListMedicalVocabulariesResponse (Maybe Text)
lmvrsNextToken = lens _lmvrsNextToken (\s a -> s {_lmvrsNextToken = a})

-- | -- | The response status code.
lmvrsResponseStatus :: Lens' ListMedicalVocabulariesResponse Int
lmvrsResponseStatus = lens _lmvrsResponseStatus (\s a -> s {_lmvrsResponseStatus = a})

instance NFData ListMedicalVocabulariesResponse
