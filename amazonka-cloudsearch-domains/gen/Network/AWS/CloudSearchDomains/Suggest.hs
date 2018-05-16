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
-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves autocomplete suggestions for a partial query string. You can use suggestions enable you to display likely matches before users finish typing. In Amazon CloudSearch, suggestions are based on the contents of a particular text field. When you request suggestions, Amazon CloudSearch finds all of the documents whose values in the suggester field start with the specified query string. The beginning of the field must match the query string to be considered a match.
--
--
-- For more information about configuring suggesters and retrieving suggestions, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Suggestions> in the /Amazon CloudSearch Developer Guide/ .
--
-- The endpoint for submitting @Suggest@ requests is domain-specific. You submit suggest requests to a domain's search endpoint. To get the search endpoint for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console.
--
module Network.AWS.CloudSearchDomains.Suggest
    (
    -- * Creating a Request
      suggest
    , Suggest
    -- * Request Lenses
    , sSize
    , sQuery
    , sSuggester

    -- * Destructuring the Response
    , suggestResponse
    , SuggestResponse
    -- * Response Lenses
    , srsSuggest
    , srsStatus
    , srsResponseStatus
    ) where

import Network.AWS.CloudSearchDomains.Types
import Network.AWS.CloudSearchDomains.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @Suggest@ request.
--
--
--
-- /See:/ 'suggest' smart constructor.
data Suggest = Suggest'
  { _sSize      :: !(Maybe Integer)
  , _sQuery     :: !Text
  , _sSuggester :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Suggest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSize' - Specifies the maximum number of suggestions to return.
--
-- * 'sQuery' - Specifies the string for which you want to get suggestions.
--
-- * 'sSuggester' - Specifies the name of the suggester to use to find suggested matches.
suggest
    :: Text -- ^ 'sQuery'
    -> Text -- ^ 'sSuggester'
    -> Suggest
suggest pQuery_ pSuggester_ =
  Suggest' {_sSize = Nothing, _sQuery = pQuery_, _sSuggester = pSuggester_}


-- | Specifies the maximum number of suggestions to return.
sSize :: Lens' Suggest (Maybe Integer)
sSize = lens _sSize (\ s a -> s{_sSize = a})

-- | Specifies the string for which you want to get suggestions.
sQuery :: Lens' Suggest Text
sQuery = lens _sQuery (\ s a -> s{_sQuery = a})

-- | Specifies the name of the suggester to use to find suggested matches.
sSuggester :: Lens' Suggest Text
sSuggester = lens _sSuggester (\ s a -> s{_sSuggester = a})

instance AWSRequest Suggest where
        type Rs Suggest = SuggestResponse
        request = get cloudSearchDomains
        response
          = receiveJSON
              (\ s h x ->
                 SuggestResponse' <$>
                   (x .?> "suggest") <*> (x .?> "status") <*>
                     (pure (fromEnum s)))

instance Hashable Suggest where

instance NFData Suggest where

instance ToHeaders Suggest where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath Suggest where
        toPath = const "/2013-01-01/suggest"

instance ToQuery Suggest where
        toQuery Suggest'{..}
          = mconcat
              ["size" =: _sSize, "q" =: _sQuery,
               "suggester" =: _sSuggester, "format=sdk&pretty=true"]

-- | Contains the response to a @Suggest@ request.
--
--
--
-- /See:/ 'suggestResponse' smart constructor.
data SuggestResponse = SuggestResponse'
  { _srsSuggest        :: !(Maybe SuggestModel)
  , _srsStatus         :: !(Maybe SuggestStatus)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuggestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsSuggest' - Container for the matching search suggestion information.
--
-- * 'srsStatus' - The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
--
-- * 'srsResponseStatus' - -- | The response status code.
suggestResponse
    :: Int -- ^ 'srsResponseStatus'
    -> SuggestResponse
suggestResponse pResponseStatus_ =
  SuggestResponse'
    { _srsSuggest = Nothing
    , _srsStatus = Nothing
    , _srsResponseStatus = pResponseStatus_
    }


-- | Container for the matching search suggestion information.
srsSuggest :: Lens' SuggestResponse (Maybe SuggestModel)
srsSuggest = lens _srsSuggest (\ s a -> s{_srsSuggest = a})

-- | The status of a @SuggestRequest@ . Contains the resource ID (@rid@ ) and how long it took to process the request (@timems@ ).
srsStatus :: Lens' SuggestResponse (Maybe SuggestStatus)
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' SuggestResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData SuggestResponse where
