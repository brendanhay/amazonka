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

-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Indexes the search suggestions. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters ConfiguringSuggesters> in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_BuildSuggesters.html>
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
import qualified GHC.Exts

newtype BuildSuggesters = BuildSuggesters
    { _bsDomainName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

newtype BuildSuggestersResponse = BuildSuggestersResponse
    { _bsrFieldNames :: List "member" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList BuildSuggestersResponse where
    type Item BuildSuggestersResponse = Text

    fromList = BuildSuggestersResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bsrFieldNames

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
bsrFieldNames = lens _bsrFieldNames (\s a -> s { _bsrFieldNames = a }) . _List

instance ToPath BuildSuggesters where
    toPath = const "/"

instance ToQuery BuildSuggesters where
    toQuery BuildSuggesters{..} = mconcat
        [ "DomainName" =? _bsDomainName
        ]

instance ToHeaders BuildSuggesters

instance AWSRequest BuildSuggesters where
    type Sv BuildSuggesters = CloudSearch
    type Rs BuildSuggesters = BuildSuggestersResponse

    request  = post "BuildSuggesters"
    response = xmlResponse

instance FromXML BuildSuggestersResponse where
    parseXML = withElement "BuildSuggestersResult" $ \x -> BuildSuggestersResponse
        <$> x .@? "FieldNames" .!@ mempty
