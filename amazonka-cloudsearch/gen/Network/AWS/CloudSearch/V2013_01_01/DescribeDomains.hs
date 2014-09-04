{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeDomains
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default. To get the
-- number of searchable documents in a domain, use the console or submit a
-- matchall request to your domain's search endpoint:
-- q=matchall&amp;q.parser=structured&amp;size=0. For more information, see
-- Getting Information about a Search Domain in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DescribeDomains
    (
    -- * Request
      DescribeDomains
    -- ** Request constructor
    , describeDomains
    -- ** Request lenses
    , ddwDomainNames

    -- * Response
    , DescribeDomainsResponse
    -- ** Response lenses
    , ddxDomainStatusList
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDomains' request.
describeDomains :: DescribeDomains
describeDomains = DescribeDomains
    { _ddwDomainNames = mempty
    }
{-# INLINE describeDomains #-}

data DescribeDomains = DescribeDomains
    { _ddwDomainNames :: [Text]
      -- ^ The names of the domains you want to include in the response.
    } deriving (Show, Generic)

-- | The names of the domains you want to include in the response.
ddwDomainNames :: Lens' DescribeDomains ([Text])
ddwDomainNames f x =
    f (_ddwDomainNames x)
        <&> \y -> x { _ddwDomainNames = y }
{-# INLINE ddwDomainNames #-}

instance ToQuery DescribeDomains where
    toQuery = genericQuery def

data DescribeDomainsResponse = DescribeDomainsResponse
    { _ddxDomainStatusList :: [DomainStatus]
      -- ^ A list that contains the status of each requested domain.
    } deriving (Show, Generic)

-- | A list that contains the status of each requested domain.
ddxDomainStatusList :: Lens' DescribeDomainsResponse ([DomainStatus])
ddxDomainStatusList f x =
    f (_ddxDomainStatusList x)
        <&> \y -> x { _ddxDomainStatusList = y }
{-# INLINE ddxDomainStatusList #-}

instance FromXML DescribeDomainsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDomains where
    type Sv DescribeDomains = CloudSearch
    type Rs DescribeDomains = DescribeDomainsResponse

    request = post "DescribeDomains"
    response _ = xmlResponse
