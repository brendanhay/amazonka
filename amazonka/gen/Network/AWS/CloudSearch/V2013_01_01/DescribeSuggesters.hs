{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudSearch.V2013_01_01.DescribeSuggesters where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeSuggesters' request.
describeSuggesters :: Text -- ^ '_dsrDomainName'
                   -> DescribeSuggesters
describeSuggesters p1 = DescribeSuggesters
    { _dsrDomainName = p1
    , _dsrDeployed = Nothing
    , _dsrSuggesterNames = mempty
    }

data DescribeSuggesters = DescribeSuggesters
    { _dsrDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _dsrDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _dsrSuggesterNames :: [Text]
      -- ^ The suggesters you want to describe.
    } deriving (Generic)

makeLenses ''DescribeSuggesters

instance ToQuery DescribeSuggesters where
    toQuery = genericToQuery def

data DescribeSuggestersResponse = DescribeSuggestersResponse
    { _dssSuggesters :: [SuggesterStatus]
      -- ^ The suggesters configured for the domain specified in the
      -- request.
    } deriving (Generic)

makeLenses ''DescribeSuggestersResponse

instance FromXML DescribeSuggestersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSuggesters where
    type Sv DescribeSuggesters = CloudSearch
    type Rs DescribeSuggesters = DescribeSuggestersResponse

    request = post "DescribeSuggesters"
    response _ = xmlResponse
