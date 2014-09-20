{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Indexes the search suggestions.
module Network.AWS.CloudSearch.BuildSuggesters
    (
    -- * Request
      BuildSuggesters
    -- ** Request constructor
    , buildSuggesters
    -- ** Request lenses
    , bsDomainName

    -- * Response
    , BuildSuggestersResponse
    -- ** Response constructor
    , buildSuggestersResponse
    -- ** Response lenses
    , bsrFieldNames
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the BuildSuggester operation. Specifies the
-- name of the domain you want to update.
newtype BuildSuggesters = BuildSuggesters
    { _bsDomainName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'BuildSuggesters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
buildSuggesters :: Text -- ^ 'bsDomainName'
                -> BuildSuggesters
buildSuggesters p1 = BuildSuggesters
    { _bsDomainName = p1
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
bsDomainName :: Lens' BuildSuggesters Text
bsDomainName = lens _bsDomainName (\s a -> s { _bsDomainName = a })

instance ToQuery BuildSuggesters where
    toQuery = genericQuery def

-- | The result of a BuildSuggester request. Contains a list of the fields used
-- for suggestions.
newtype BuildSuggestersResponse = BuildSuggestersResponse
    { _bsrFieldNames :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'BuildSuggestersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @FieldNames ::@ @[Text]@
--
buildSuggestersResponse :: BuildSuggestersResponse
buildSuggestersResponse = BuildSuggestersResponse
    { _bsrFieldNames = mempty
    }

-- | A list of field names.
bsrFieldNames :: Lens' BuildSuggestersResponse [Text]
bsrFieldNames = lens _bsrFieldNames (\s a -> s { _bsrFieldNames = a })

instance FromXML BuildSuggestersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest BuildSuggesters where
    type Sv BuildSuggesters = CloudSearch
    type Rs BuildSuggesters = BuildSuggestersResponse

    request = post "BuildSuggesters"
    response _ = xmlResponse
