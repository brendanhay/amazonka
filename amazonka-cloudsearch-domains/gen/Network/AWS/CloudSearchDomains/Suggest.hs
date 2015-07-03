{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves autocomplete suggestions for a partial query string. You can
-- use suggestions enable you to display likely matches before users finish
-- typing. In Amazon CloudSearch, suggestions are based on the contents of
-- a particular text field. When you request suggestions, Amazon
-- CloudSearch finds all of the documents whose values in the suggester
-- field start with the specified query string. The beginning of the field
-- must match the query string to be considered a match.
--
-- For more information about configuring suggesters and retrieving
-- suggestions, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- The endpoint for submitting @Suggest@ requests is domain-specific. You
-- submit suggest requests to a domain\'s search endpoint. To get the
-- search endpoint for your domain, use the Amazon CloudSearch
-- configuration service @DescribeDomains@ action. A domain\'s endpoints
-- are also displayed on the domain dashboard in the Amazon CloudSearch
-- console.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_Suggest.html>
module Network.AWS.CloudSearchDomains.Suggest
    (
    -- * Request
      Suggest
    -- ** Request constructor
    , suggest
    -- ** Request lenses
    , sugSize
    , sugQuery
    , sugSuggester

    -- * Response
    , SuggestResponse
    -- ** Response constructor
    , suggestResponse
    -- ** Response lenses
    , srSuggest
    , srStatus
    ) where

import           Network.AWS.CloudSearchDomains.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @Suggest@ request.
--
-- /See:/ 'suggest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sugSize'
--
-- * 'sugQuery'
--
-- * 'sugSuggester'
data Suggest = Suggest'
    { _sugSize      :: !(Maybe Integer)
    , _sugQuery     :: !Text
    , _sugSuggester :: !Text
    } deriving (Eq,Read,Show)

-- | 'Suggest' smart constructor.
suggest :: Text -> Text -> Suggest
suggest pQuery pSuggester =
    Suggest'
    { _sugSize = Nothing
    , _sugQuery = pQuery
    , _sugSuggester = pSuggester
    }

-- | Specifies the maximum number of suggestions to return.
sugSize :: Lens' Suggest (Maybe Integer)
sugSize = lens _sugSize (\ s a -> s{_sugSize = a});

-- | Specifies the string for which you want to get suggestions.
sugQuery :: Lens' Suggest Text
sugQuery = lens _sugQuery (\ s a -> s{_sugQuery = a});

-- | Specifies the name of the suggester to use to find suggested matches.
sugSuggester :: Lens' Suggest Text
sugSuggester = lens _sugSuggester (\ s a -> s{_sugSuggester = a});

instance AWSRequest Suggest where
        type Sv Suggest = CloudSearchDomains
        type Rs Suggest = SuggestResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 SuggestResponse' <$>
                   (x .?> "suggest") <*> (pure (fromEnum s)))

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
              ["size" =: _sugSize, "q" =: _sugQuery,
               "suggester" =: _sugSuggester,
               "format=sdk&pretty=true"]

-- | Contains the response to a @Suggest@ request.
--
-- /See:/ 'suggestResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srSuggest'
--
-- * 'srStatus'
data SuggestResponse = SuggestResponse'
    { _srSuggest :: !(Maybe SuggestModel)
    , _srStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'SuggestResponse' smart constructor.
suggestResponse :: Int -> SuggestResponse
suggestResponse pStatus =
    SuggestResponse'
    { _srSuggest = Nothing
    , _srStatus = pStatus
    }

-- | Container for the matching search suggestion information.
srSuggest :: Lens' SuggestResponse (Maybe SuggestModel)
srSuggest = lens _srSuggest (\ s a -> s{_srSuggest = a});

-- | FIXME: Undocumented member.
srStatus :: Lens' SuggestResponse Int
srStatus = lens _srStatus (\ s a -> s{_srStatus = a});
