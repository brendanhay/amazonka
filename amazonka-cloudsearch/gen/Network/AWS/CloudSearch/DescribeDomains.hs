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

-- Module      : Network.AWS.CloudSearch.DescribeDomains
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
-- q=matchall&amp;amp;q.parser=structured&amp;amp;size=0. For more
-- information, see Getting Information about a Search Domain in the Amazon
-- CloudSearch Developer Guide.
module Network.AWS.CloudSearch.DescribeDomains
    (
    -- * Request
      DescribeDomains
    -- ** Request constructor
    , describeDomains
    -- ** Request lenses
    , ddDomainNames

    -- * Response
    , DescribeDomainsResponse
    -- ** Response constructor
    , describeDomainsResponse
    -- ** Response lenses
    , ddrDomainStatusList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

newtype DescribeDomains = DescribeDomains
    { _ddDomainNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDomains where
    type Item DescribeDomains = Text

    fromList = DescribeDomains . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddDomainNames

-- | 'DescribeDomains' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDomainNames' @::@ ['Text']
--
describeDomains :: DescribeDomains
describeDomains = DescribeDomains
    { _ddDomainNames = mempty
    }

-- | The names of the domains you want to include in the response.
ddDomainNames :: Lens' DescribeDomains [Text]
ddDomainNames = lens _ddDomainNames (\s a -> s { _ddDomainNames = a })

instance ToQuery DescribeDomains

instance ToPath DescribeDomains where
    toPath = const "/"

newtype DescribeDomainsResponse = DescribeDomainsResponse
    { _ddrDomainStatusList :: [DomainStatus]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDomainsResponse where
    type Item DescribeDomainsResponse = DomainStatus

    fromList = DescribeDomainsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddrDomainStatusList

-- | 'DescribeDomainsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDomainStatusList' @::@ ['DomainStatus']
--
describeDomainsResponse :: DescribeDomainsResponse
describeDomainsResponse = DescribeDomainsResponse
    { _ddrDomainStatusList = mempty
    }

ddrDomainStatusList :: Lens' DescribeDomainsResponse [DomainStatus]
ddrDomainStatusList =
    lens _ddrDomainStatusList (\s a -> s { _ddrDomainStatusList = a })

instance AWSRequest DescribeDomains where
    type Sv DescribeDomains = CloudSearch
    type Rs DescribeDomains = DescribeDomainsResponse

    request  = post "DescribeDomains"
    response = xmlResponse $ \h x -> DescribeDomainsResponse
        <$> x %| "DomainStatusList"
