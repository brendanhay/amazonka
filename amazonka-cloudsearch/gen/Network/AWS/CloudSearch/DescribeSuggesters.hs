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

-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the suggesters configured for a domain. A suggester enables you to
-- display possible matches before users finish typing their queries. Can be
-- limited to specific suggesters by name. By default, shows all suggesters
-- and includes any pending changes to the configuration. Set the Deployed
-- option to true to show the active configuration and exclude pending
-- changes. For more information, see Getting Search Suggestions in the Amazon
-- CloudSearch Developer Guide.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeSuggesters.html>
module Network.AWS.CloudSearch.DescribeSuggesters
    (
    -- * Request
      DescribeSuggesters
    -- ** Request constructor
    , describeSuggesters
    -- ** Request lenses
    , ds1Deployed
    , ds1DomainName
    , ds1SuggesterNames

    -- * Response
    , DescribeSuggestersResponse
    -- ** Response constructor
    , describeSuggestersResponse
    -- ** Response lenses
    , dsrSuggesters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DescribeSuggesters = DescribeSuggesters
    { _ds1Deployed       :: Maybe Bool
    , _ds1DomainName     :: Text
    , _ds1SuggesterNames :: List "SuggesterNames" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeSuggesters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1Deployed' @::@ 'Maybe' 'Bool'
--
-- * 'ds1DomainName' @::@ 'Text'
--
-- * 'ds1SuggesterNames' @::@ ['Text']
--
describeSuggesters :: Text -- ^ 'ds1DomainName'
                   -> DescribeSuggesters
describeSuggesters p1 = DescribeSuggesters
    { _ds1DomainName     = p1
    , _ds1SuggesterNames = mempty
    , _ds1Deployed       = Nothing
    }

-- | Whether to display the deployed configuration (true) or include any
-- pending changes (false). Defaults to false.
ds1Deployed :: Lens' DescribeSuggesters (Maybe Bool)
ds1Deployed = lens _ds1Deployed (\s a -> s { _ds1Deployed = a })

-- | The name of the domain you want to describe.
ds1DomainName :: Lens' DescribeSuggesters Text
ds1DomainName = lens _ds1DomainName (\s a -> s { _ds1DomainName = a })

-- | The suggesters you want to describe.
ds1SuggesterNames :: Lens' DescribeSuggesters [Text]
ds1SuggesterNames =
    lens _ds1SuggesterNames (\s a -> s { _ds1SuggesterNames = a })
        . _List

newtype DescribeSuggestersResponse = DescribeSuggestersResponse
    { _dsrSuggesters :: List "Suggesters" SuggesterStatus
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeSuggestersResponse where
    type Item DescribeSuggestersResponse = SuggesterStatus

    fromList = DescribeSuggestersResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsrSuggesters

-- | 'DescribeSuggestersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSuggesters' @::@ ['SuggesterStatus']
--
describeSuggestersResponse :: DescribeSuggestersResponse
describeSuggestersResponse = DescribeSuggestersResponse
    { _dsrSuggesters = mempty
    }

-- | The suggesters configured for the domain specified in the request.
dsrSuggesters :: Lens' DescribeSuggestersResponse [SuggesterStatus]
dsrSuggesters = lens _dsrSuggesters (\s a -> s { _dsrSuggesters = a }) . _List

instance ToPath DescribeSuggesters where
    toPath = const "/"

instance ToQuery DescribeSuggesters where
    toQuery DescribeSuggesters{..} = mconcat
        [ "Deployed"       =? _ds1Deployed
        , "DomainName"     =? _ds1DomainName
        , "SuggesterNames" =? _ds1SuggesterNames
        ]

instance ToHeaders DescribeSuggesters

query

instance AWSRequest DescribeSuggesters where
    type Sv DescribeSuggesters = CloudSearch
    type Rs DescribeSuggesters = DescribeSuggestersResponse

    request  = post "DescribeSuggesters"
    response = xmlResponse

instance FromXML DescribeSuggestersResponse where
    parseXML = withElement "DescribeSuggestersResult" $ \x -> DescribeSuggestersResponse
        <$> x .@  "Suggesters"
