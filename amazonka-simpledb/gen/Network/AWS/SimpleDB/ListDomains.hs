{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.ListDomains
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListDomains operation lists all domains associated with the Access Key
-- ID. It returns domain names up to the limit set by MaxNumberOfDomains. A
-- NextToken is returned if there are more than MaxNumberOfDomains domains.
-- Calling ListDomains successive times with the NextToken provided by the
-- operation returns up to MaxNumberOfDomains more domain names with each
-- successive operation call.
module Network.AWS.SimpleDB.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldMaxNumberOfDomains
    , ldNextToken

    -- * Response
    , ListDomainsResponse
    -- ** Response constructor
    , listDomainsResponse
    -- ** Response lenses
    , ldrDomainNames
    , ldrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types
import Network.AWS.Prelude

data ListDomains = ListDomains
    { _ldMaxNumberOfDomains :: Maybe Integer
    , _ldNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDomains' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MaxNumberOfDomains ::@ @Maybe Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
listDomains :: ListDomains
listDomains = ListDomains
    { _ldMaxNumberOfDomains = Nothing
    , _ldNextToken = Nothing
    }

-- | The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
ldMaxNumberOfDomains :: Lens' ListDomains (Maybe Integer)
ldMaxNumberOfDomains =
    lens _ldMaxNumberOfDomains (\s a -> s { _ldMaxNumberOfDomains = a })

-- | A string informing Amazon SimpleDB where to start the next list of domain
-- names.
ldNextToken :: Lens' ListDomains (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s { _ldNextToken = a })

instance ToQuery ListDomains where
    toQuery = genericQuery def

data ListDomainsResponse = ListDomainsResponse
    { _ldrDomainNames :: [Text]
    , _ldrNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDomainsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainNames ::@ @[Text]@
--
-- * @NextToken ::@ @Maybe Text@
--
listDomainsResponse :: ListDomainsResponse
listDomainsResponse = ListDomainsResponse
    { _ldrDomainNames = mempty
    , _ldrNextToken = Nothing
    }

-- | A list of domain names that match the expression.
ldrDomainNames :: Lens' ListDomainsResponse [Text]
ldrDomainNames = lens _ldrDomainNames (\s a -> s { _ldrDomainNames = a })

-- | An opaque token indicating that there are more domains than the specified
-- MaxNumberOfDomains still available.
ldrNextToken :: Lens' ListDomainsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\s a -> s { _ldrNextToken = a })

instance FromXML ListDomainsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDomains where
    type Sv ListDomains = SimpleDB
    type Rs ListDomains = ListDomainsResponse

    request = post "ListDomains"
    response _ = xmlResponse

instance AWSPager ListDomains where
    next rq rs = (\x -> rq & ldNextToken ?~ x)
        <$> (rs ^. ldrNextToken)
