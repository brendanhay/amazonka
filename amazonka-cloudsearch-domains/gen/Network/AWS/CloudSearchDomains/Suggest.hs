{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearchDomains.Suggest
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves autocomplete suggestions for a partial query string. You can use
-- suggestions enable you to display likely matches before users finish
-- typing. In Amazon CloudSearch, suggestions are based on the contents of a
-- particular text field. When you request suggestions, Amazon CloudSearch
-- finds all of the documents whose values in the suggester field start with
-- the specified query string. The beginning of the field must match the query
-- string to be considered a match. For more information about configuring
-- suggesters and retrieving suggestions, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html
-- Getting Suggestions> in the /Amazon CloudSearch Developer Guide/. The
-- endpoint for submitting 'Suggest' requests is domain-specific. You submit
-- suggest requests to a domain's search endpoint. To get the search endpoint
-- for your domain, use the Amazon CloudSearch configuration service
-- 'DescribeDomains' action. A domain's endpoints are also displayed on the
-- domain dashboard in the Amazon CloudSearch console.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_Suggest.html>
module Network.AWS.CloudSearchDomains.Suggest
    (
    -- * Request
      Suggest
    -- ** Request constructor
    , suggest
    -- ** Request lenses
    , sQuery
    , sSize
    , sSuggester

    -- * Response
    , SuggestResponse
    -- ** Response constructor
    , suggestResponse
    -- ** Response lenses
    , srStatus
    , srSuggest
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CloudSearchDomains.Types
import qualified GHC.Exts

data Suggest = Suggest
    { _sQuery     :: Text
    , _sSize      :: Maybe Integer
    , _sSuggester :: Text
    } deriving (Eq, Ord, Show)

-- | 'Suggest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sQuery' @::@ 'Text'
--
-- * 'sSize' @::@ 'Maybe' 'Integer'
--
-- * 'sSuggester' @::@ 'Text'
--
suggest :: Text -- ^ 'sQuery'
        -> Text -- ^ 'sSuggester'
        -> Suggest
suggest p1 p2 = Suggest
    { _sQuery     = p1
    , _sSuggester = p2
    , _sSize      = Nothing
    }

-- | Specifies the string for which you want to get suggestions.
sQuery :: Lens' Suggest Text
sQuery = lens _sQuery (\s a -> s { _sQuery = a })

-- | Specifies the maximum number of suggestions to return.
sSize :: Lens' Suggest (Maybe Integer)
sSize = lens _sSize (\s a -> s { _sSize = a })

-- | Specifies the name of the suggester to use to find suggested matches.
sSuggester :: Lens' Suggest Text
sSuggester = lens _sSuggester (\s a -> s { _sSuggester = a })

data SuggestResponse = SuggestResponse
    { _srStatus  :: Maybe SuggestStatus
    , _srSuggest :: Maybe SuggestModel
    } deriving (Eq, Show)

-- | 'SuggestResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srStatus' @::@ 'Maybe' 'SuggestStatus'
--
-- * 'srSuggest' @::@ 'Maybe' 'SuggestModel'
--
suggestResponse :: SuggestResponse
suggestResponse = SuggestResponse
    { _srStatus  = Nothing
    , _srSuggest = Nothing
    }

-- | The status of a 'SuggestRequest'. Contains the resource ID ('rid') and
-- how long it took to process the request ('timems').
srStatus :: Lens' SuggestResponse (Maybe SuggestStatus)
srStatus = lens _srStatus (\s a -> s { _srStatus = a })

-- | Container for the matching search suggestion information.
srSuggest :: Lens' SuggestResponse (Maybe SuggestModel)
srSuggest = lens _srSuggest (\s a -> s { _srSuggest = a })

instance ToPath Suggest where
    toPath = const "/2013-01-01/suggest"

instance ToQuery Suggest where
    toQuery Suggest{..} = mconcat
        [ "q"         =? _sQuery
        , "size"      =? _sSize
        , "suggester" =? _sSuggester
        ]

instance ToHeaders Suggest

instance ToJSON Suggest where
    toJSON = const (toJSON Empty)

instance AWSRequest Suggest where
    type Sv Suggest = CloudSearchDomains
    type Rs Suggest = SuggestResponse

    request  = get
    response = jsonResponse

instance FromJSON SuggestResponse where
    parseJSON = withObject "SuggestResponse" $ \o -> SuggestResponse
        <$> o .:? "status"
        <*> o .:? "suggest"
