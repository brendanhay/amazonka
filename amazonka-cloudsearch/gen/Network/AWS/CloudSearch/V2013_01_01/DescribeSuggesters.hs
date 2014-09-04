{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeSuggesters
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
module Network.AWS.CloudSearch.V2013_01_01.DescribeSuggesters
    (
    -- * Request
      DescribeSuggesters
    -- ** Request constructor
    , describeSuggesters
    -- ** Request lenses
    , dsvDomainName
    , dsvDeployed
    , dsvSuggesterNames

    -- * Response
    , DescribeSuggestersResponse
    -- ** Response lenses
    , dswSuggesters
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSuggesters' request.
describeSuggesters :: Text -- ^ 'dsvDomainName'
                   -> DescribeSuggesters
describeSuggesters p1 = DescribeSuggesters
    { _dsvDomainName = p1
    , _dsvDeployed = Nothing
    , _dsvSuggesterNames = mempty
    }
{-# INLINE describeSuggesters #-}

data DescribeSuggesters = DescribeSuggesters
    { _dsvDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _dsvDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _dsvSuggesterNames :: [Text]
      -- ^ The suggesters you want to describe.
    } deriving (Show, Generic)

-- | The name of the domain you want to describe.
dsvDomainName :: Lens' DescribeSuggesters (Text)
dsvDomainName f x =
    f (_dsvDomainName x)
        <&> \y -> x { _dsvDomainName = y }
{-# INLINE dsvDomainName #-}

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
dsvDeployed :: Lens' DescribeSuggesters (Maybe Bool)
dsvDeployed f x =
    f (_dsvDeployed x)
        <&> \y -> x { _dsvDeployed = y }
{-# INLINE dsvDeployed #-}

-- | The suggesters you want to describe.
dsvSuggesterNames :: Lens' DescribeSuggesters ([Text])
dsvSuggesterNames f x =
    f (_dsvSuggesterNames x)
        <&> \y -> x { _dsvSuggesterNames = y }
{-# INLINE dsvSuggesterNames #-}

instance ToQuery DescribeSuggesters where
    toQuery = genericQuery def

data DescribeSuggestersResponse = DescribeSuggestersResponse
    { _dswSuggesters :: [SuggesterStatus]
      -- ^ The suggesters configured for the domain specified in the
      -- request.
    } deriving (Show, Generic)

-- | The suggesters configured for the domain specified in the request.
dswSuggesters :: Lens' DescribeSuggestersResponse ([SuggesterStatus])
dswSuggesters f x =
    f (_dswSuggesters x)
        <&> \y -> x { _dswSuggesters = y }
{-# INLINE dswSuggesters #-}

instance FromXML DescribeSuggestersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSuggesters where
    type Sv DescribeSuggesters = CloudSearch
    type Rs DescribeSuggesters = DescribeSuggestersResponse

    request = post "DescribeSuggesters"
    response _ = xmlResponse
