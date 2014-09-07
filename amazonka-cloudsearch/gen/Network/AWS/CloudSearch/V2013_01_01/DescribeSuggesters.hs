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
    , mkDescribeSuggesters
    -- ** Request lenses
    , ds3DomainName
    , ds3SuggesterNames
    , ds3Deployed

    -- * Response
    , DescribeSuggestersResponse
    -- ** Response lenses
    , dsrs1Suggesters
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeSuggester operation. Specifies
-- the name of the domain you want to describe. To restrict the response to
-- particular suggesters, specify the names of the suggesters you want to
-- describe. To show the active configuration and exclude any pending changes,
-- set the Deployed option to true.
data DescribeSuggesters = DescribeSuggesters
    { _ds3DomainName :: Text
    , _ds3SuggesterNames :: [Text]
    , _ds3Deployed :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSuggesters' request.
mkDescribeSuggesters :: Text -- ^ 'ds3DomainName'
                     -> DescribeSuggesters
mkDescribeSuggesters p1 = DescribeSuggesters
    { _ds3DomainName = p1
    , _ds3SuggesterNames = mempty
    , _ds3Deployed = Nothing
    }

-- | The name of the domain you want to describe.
ds3DomainName :: Lens' DescribeSuggesters Text
ds3DomainName = lens _ds3DomainName (\s a -> s { _ds3DomainName = a })

-- | The suggesters you want to describe.
ds3SuggesterNames :: Lens' DescribeSuggesters [Text]
ds3SuggesterNames =
    lens _ds3SuggesterNames (\s a -> s { _ds3SuggesterNames = a })

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
ds3Deployed :: Lens' DescribeSuggesters (Maybe Bool)
ds3Deployed = lens _ds3Deployed (\s a -> s { _ds3Deployed = a })

instance ToQuery DescribeSuggesters where
    toQuery = genericQuery def

-- | The result of a DescribeSuggesters request.
newtype DescribeSuggestersResponse = DescribeSuggestersResponse
    { _dsrs1Suggesters :: [SuggesterStatus]
    } deriving (Show, Generic)

-- | The suggesters configured for the domain specified in the request.
dsrs1Suggesters :: Lens' DescribeSuggestersResponse [SuggesterStatus]
dsrs1Suggesters = lens _dsrs1Suggesters (\s a -> s { _dsrs1Suggesters = a })

instance FromXML DescribeSuggestersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSuggesters where
    type Sv DescribeSuggesters = CloudSearch
    type Rs DescribeSuggesters = DescribeSuggestersResponse

    request = post "DescribeSuggesters"
    response _ = xmlResponse
