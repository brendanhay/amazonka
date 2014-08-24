{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearchDomains.V2013_01_01.Suggest
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
-- suggesters and retrieving suggestions, see Getting Suggestions in the
-- Amazon CloudSearch Developer Guide. The endpoint for submitting Suggest
-- requests is domain-specific. You submit suggest requests to a domain's
-- search endpoint. To get the search endpoint for your domain, use the Amazon
-- CloudSearch configuration service DescribeDomains action. A domain's
-- endpoints are also displayed on the domain dashboard in the Amazon
-- CloudSearch console.
module Network.AWS.CloudSearchDomains.V2013_01_01.Suggest where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudSearchDomains.V2013_01_01.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'Suggest' request.
suggest :: Text -- ^ '_srQuery'
        -> Text -- ^ '_srSuggester'
        -> Suggest
suggest p1 p2 = Suggest
    { _srQuery = p1
    , _srSuggester = p2
    , _srSize = Nothing
    }

data Suggest = Suggest
    { _srQuery :: Text
      -- ^ Specifies the string for which you want to get suggestions.
    , _srSuggester :: Text
      -- ^ Specifies the name of the suggester to use to find suggested
      -- matches.
    , _srSize :: Maybe Integer
      -- ^ Specifies the maximum number of suggestions to return.
    } deriving (Show, Generic)

makeLenses ''Suggest

instance ToPath Suggest where
    toPath = const "/2013-01-01/suggest"

instance ToQuery Suggest where
    toQuery Suggest{..} = mconcat
        [ "format=sdk&pretty=true&q" =? _srQuery
        , "size" =? _srSize
        , "suggester" =? _srSuggester
        ]

instance ToHeaders Suggest

instance ToJSON Suggest

data SuggestResponse = SuggestResponse
    { _stSuggest :: Maybe SuggestModel
      -- ^ Container for the matching search suggestion information.
    , _stStatus :: Maybe SuggestStatus
      -- ^ The status of a SuggestRequest. Contains the resource ID (rid)
      -- and how long it took to process the request (timems).
    } deriving (Show, Generic)

makeLenses ''SuggestResponse

instance FromJSON SuggestResponse

instance AWSRequest Suggest where
    type Sv Suggest = CloudSearchDomains
    type Rs Suggest = SuggestResponse

    request = get
    response _ = jsonResponse
