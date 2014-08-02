{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.BuildSuggesters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Indexes the search suggestions.
module Network.AWS.CloudSearch.V2013_01_01.BuildSuggesters where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data BuildSuggesters = BuildSuggesters
    { _bsrDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    } deriving (Generic)

makeLenses ''BuildSuggesters

instance ToQuery BuildSuggesters where
    toQuery = genericToQuery def

data BuildSuggestersResponse = BuildSuggestersResponse
    { _bssFieldNames :: [Text]
      -- ^ A list of field names.
    } deriving (Generic)

makeLenses ''BuildSuggestersResponse

instance FromXML BuildSuggestersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest BuildSuggesters where
    type Sv BuildSuggesters = CloudSearch
    type Rs BuildSuggesters = BuildSuggestersResponse

    request = post "BuildSuggesters"
    response _ = xmlResponse
