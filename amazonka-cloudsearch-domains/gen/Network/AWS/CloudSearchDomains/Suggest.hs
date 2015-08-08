{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves autocomplete suggestions for a partial query string. You can
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
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_Suggest.html AWS API Reference> for Suggest.
module Network.AWS.CloudSearchDomains.Suggest
    (
    -- * Creating a Request
      Suggest
    , suggest
    -- * Request Lenses
    , sSize
    , sQuery
    , sSuggester

    -- * Destructuring the Response
    , SuggestResponse
    , suggestResponse
    -- * Response Lenses
    , srsSuggest
    , srsStatus
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
-- * 'sSize'
--
-- * 'sQuery'
--
-- * 'sSuggester'
data Suggest = Suggest'
    { _sSize      :: !(Maybe Integer)
    , _sQuery     :: !Text
    , _sSuggester :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Suggest' smart constructor.
suggest :: Text -> Text -> Suggest
suggest pQuery_ pSuggester_ =
    Suggest'
    { _sSize = Nothing
    , _sQuery = pQuery_
    , _sSuggester = pSuggester_
    }

-- | Specifies the maximum number of suggestions to return.
sSize :: Lens' Suggest (Maybe Integer)
sSize = lens _sSize (\ s a -> s{_sSize = a});

-- | Specifies the string for which you want to get suggestions.
sQuery :: Lens' Suggest Text
sQuery = lens _sQuery (\ s a -> s{_sQuery = a});

-- | Specifies the name of the suggester to use to find suggested matches.
sSuggester :: Lens' Suggest Text
sSuggester = lens _sSuggester (\ s a -> s{_sSuggester = a});

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
              ["size" =: _sSize, "q" =: _sQuery,
               "suggester" =: _sSuggester, "format=sdk&pretty=true"]

-- | Contains the response to a @Suggest@ request.
--
-- /See:/ 'suggestResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsSuggest'
--
-- * 'srsStatus'
data SuggestResponse = SuggestResponse'
    { _srsSuggest :: !(Maybe SuggestModel)
    , _srsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SuggestResponse' smart constructor.
suggestResponse :: Int -> SuggestResponse
suggestResponse pStatus_ =
    SuggestResponse'
    { _srsSuggest = Nothing
    , _srsStatus = pStatus_
    }

-- | Container for the matching search suggestion information.
srsSuggest :: Lens' SuggestResponse (Maybe SuggestModel)
srsSuggest = lens _srsSuggest (\ s a -> s{_srsSuggest = a});

-- | Undocumented member.
srsStatus :: Lens' SuggestResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
