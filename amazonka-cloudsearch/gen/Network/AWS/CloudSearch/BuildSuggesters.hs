{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Indexes the search suggestions. For more information, see Configuring
-- Suggesters in the Amazon CloudSearch Developer Guide.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types

newtype BuildSuggesters = BuildSuggesters
    { _bsDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'BuildSuggesters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsDomainName' @::@ 'Text'
--
buildSuggesters :: Text -- ^ 'bsDomainName'
                -> BuildSuggesters
buildSuggesters p1 = BuildSuggesters
    { _bsDomainName = p1
    }

bsDomainName :: Lens' BuildSuggesters Text
bsDomainName = lens _bsDomainName (\s a -> s { _bsDomainName = a })

instance ToQuery BuildSuggesters

instance ToPath BuildSuggesters where
    toPath = const "/"

newtype BuildSuggestersResponse = BuildSuggestersResponse
    { _bsrFieldNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList BuildSuggestersResponse where
    type Item BuildSuggestersResponse = Text

    fromList = BuildSuggestersResponse . fromList
    toList   = toList . _bsrFieldNames

-- | 'BuildSuggestersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsrFieldNames' @::@ ['Text']
--
buildSuggestersResponse :: BuildSuggestersResponse
buildSuggestersResponse = BuildSuggestersResponse
    { _bsrFieldNames = mempty
    }

bsrFieldNames :: Lens' BuildSuggestersResponse [Text]
bsrFieldNames = lens _bsrFieldNames (\s a -> s { _bsrFieldNames = a })

instance FromXML BuildSuggestersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BuildSuggestersResponse"

instance AWSRequest BuildSuggesters where
    type Sv BuildSuggesters = CloudSearch
    type Rs BuildSuggesters = BuildSuggestersResponse

    request  = post "BuildSuggesters"
    response = xmlResponse $ \h x -> BuildSuggestersResponse
        <$> x %| "FieldNames"
