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
    , mkDescribeDomains
    -- ** Request lenses
    , dd1DomainNames

    -- * Response
    , DescribeDomainsResponse
    -- ** Response lenses
    , ddrsrsDomainStatusList
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeDomains operation. By default
-- shows the status of all domains. To restrict the response to particular
-- domains, specify the names of the domains you want to describe.
newtype DescribeDomains = DescribeDomains
    { _dd1DomainNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDomains' request.
mkDescribeDomains :: DescribeDomains
mkDescribeDomains = DescribeDomains
    { _dd1DomainNames = mempty
    }

-- | The names of the domains you want to include in the response.
dd1DomainNames :: Lens' DescribeDomains [Text]
dd1DomainNames = lens _dd1DomainNames (\s a -> s { _dd1DomainNames = a })

instance ToQuery DescribeDomains where
    toQuery = genericQuery def

-- | The result of a DescribeDomains request. Contains the status of the domains
-- specified in the request or all domains owned by the account.
newtype DescribeDomainsResponse = DescribeDomainsResponse
    { _ddrsrsDomainStatusList :: [DomainStatus]
    } deriving (Show, Generic)

-- | A list that contains the status of each requested domain.
ddrsrsDomainStatusList :: Lens' DescribeDomainsResponse [DomainStatus]
ddrsrsDomainStatusList =
    lens _ddrsrsDomainStatusList (\s a -> s { _ddrsrsDomainStatusList = a })

instance FromXML DescribeDomainsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDomains where
    type Sv DescribeDomains = CloudSearch
    type Rs DescribeDomains = DescribeDomainsResponse

    request = post "DescribeDomains"
    response _ = xmlResponse
