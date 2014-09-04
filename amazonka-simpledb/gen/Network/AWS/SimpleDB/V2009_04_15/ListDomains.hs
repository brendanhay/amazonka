{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.ListDomains
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
module Network.AWS.SimpleDB.V2009_04_15.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldrMaxNumberOfDomains
    , ldrNextToken

    -- * Response
    , ListDomainsResponse
    -- ** Response lenses
    , ldsDomainNames
    , ldsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListDomains' request.
listDomains :: ListDomains
listDomains = ListDomains
    { _ldrMaxNumberOfDomains = Nothing
    , _ldrNextToken = Nothing
    }
{-# INLINE listDomains #-}

data ListDomains = ListDomains
    { _ldrMaxNumberOfDomains :: Maybe Integer
      -- ^ The maximum number of domain names you want returned. The range
      -- is 1 to 100. The default setting is 100.
    , _ldrNextToken :: Maybe Text
      -- ^ A string informing Amazon SimpleDB where to start the next list
      -- of domain names.
    } deriving (Show, Generic)

-- | The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
ldrMaxNumberOfDomains :: Lens' ListDomains (Maybe Integer)
ldrMaxNumberOfDomains f x =
    f (_ldrMaxNumberOfDomains x)
        <&> \y -> x { _ldrMaxNumberOfDomains = y }
{-# INLINE ldrMaxNumberOfDomains #-}

-- | A string informing Amazon SimpleDB where to start the next list of domain
-- names.
ldrNextToken :: Lens' ListDomains (Maybe Text)
ldrNextToken f x =
    f (_ldrNextToken x)
        <&> \y -> x { _ldrNextToken = y }
{-# INLINE ldrNextToken #-}

instance ToQuery ListDomains where
    toQuery = genericQuery def

data ListDomainsResponse = ListDomainsResponse
    { _ldsDomainNames :: [Text]
      -- ^ A list of domain names that match the expression.
    , _ldsNextToken :: Maybe Text
      -- ^ An opaque token indicating that there are more domains than the
      -- specified MaxNumberOfDomains still available.
    } deriving (Show, Generic)

-- | A list of domain names that match the expression.
ldsDomainNames :: Lens' ListDomainsResponse ([Text])
ldsDomainNames f x =
    f (_ldsDomainNames x)
        <&> \y -> x { _ldsDomainNames = y }
{-# INLINE ldsDomainNames #-}

-- | An opaque token indicating that there are more domains than the specified
-- MaxNumberOfDomains still available.
ldsNextToken :: Lens' ListDomainsResponse (Maybe Text)
ldsNextToken f x =
    f (_ldsNextToken x)
        <&> \y -> x { _ldsNextToken = y }
{-# INLINE ldsNextToken #-}

instance FromXML ListDomainsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDomains where
    type Sv ListDomains = SimpleDB
    type Rs ListDomains = ListDomainsResponse

    request = post "ListDomains"
    response _ = xmlResponse

instance AWSPager ListDomains where
    next rq rs = (\x -> rq { _ldrNextToken = Just x })
        <$> (_ldsNextToken rs)
