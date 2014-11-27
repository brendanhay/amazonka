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

-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns all the domain names registered with Amazon Route 53
-- for the current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-ListDomains.html>
module Network.AWS.Route53Domains.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldMarker
    , ldMaxItems

    -- * Response
    , ListDomainsResponse
    -- ** Response constructor
    , listDomainsResponse
    -- ** Response lenses
    , ldrDomains
    , ldrNextPageMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data ListDomains = ListDomains
    { _ldMarker   :: Maybe Text
    , _ldMaxItems :: Maybe Int
    } deriving (Eq, Ord, Show)

-- | 'ListDomains' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMarker' @::@ 'Maybe' 'Text'
--
-- * 'ldMaxItems' @::@ 'Maybe' 'Int'
--
listDomains :: ListDomains
listDomains = ListDomains
    { _ldMarker   = Nothing
    , _ldMaxItems = Nothing
    }

-- | For an initial request for a list of domains, omit this element. If the
-- number of domains that are associated with the current AWS account is greater
-- than the value that you specified for 'MaxItems', you can use 'Marker' to return
-- additional domains. Get the value of 'NextPageMarker' from the previous
-- response, and submit another request that includes the value of 'NextPageMarker'
-- in the 'Marker' element.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The marker must match the value specified in the previous
-- request.
--
-- Required: No
ldMarker :: Lens' ListDomains (Maybe Text)
ldMarker = lens _ldMarker (\s a -> s { _ldMarker = a })

-- | Number of domains to be returned.
--
-- Type: Integer
--
-- Default: 20
--
-- Constraints: A numeral between 1 and 100.
--
-- Required: No
ldMaxItems :: Lens' ListDomains (Maybe Int)
ldMaxItems = lens _ldMaxItems (\s a -> s { _ldMaxItems = a })

data ListDomainsResponse = ListDomainsResponse
    { _ldrDomains        :: List "Domains" DomainSummary
    , _ldrNextPageMarker :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListDomainsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDomains' @::@ ['DomainSummary']
--
-- * 'ldrNextPageMarker' @::@ 'Maybe' 'Text'
--
listDomainsResponse :: ListDomainsResponse
listDomainsResponse = ListDomainsResponse
    { _ldrDomains        = mempty
    , _ldrNextPageMarker = Nothing
    }

-- | A summary of domains.
--
-- Type: Complex type containing a list of domain summaries.
--
-- Children: 'AutoRenew', 'DomainName', 'Expiry', 'TransferLock'
ldrDomains :: Lens' ListDomainsResponse [DomainSummary]
ldrDomains = lens _ldrDomains (\s a -> s { _ldrDomains = a }) . _List

-- | If there are more domains than you specified for 'MaxItems' in the request,
-- submit another request and include the value of 'NextPageMarker' in the value
-- of 'Marker'.
--
-- Type: String
--
-- Parent: 'Operations'
ldrNextPageMarker :: Lens' ListDomainsResponse (Maybe Text)
ldrNextPageMarker =
    lens _ldrNextPageMarker (\s a -> s { _ldrNextPageMarker = a })

instance ToPath ListDomains where
    toPath = const "/"

instance ToQuery ListDomains where
    toQuery = const mempty

instance ToHeaders ListDomains

instance ToJSON ListDomains where
    toJSON ListDomains{..} = object
        [ "Marker"   .= _ldMarker
        , "MaxItems" .= _ldMaxItems
        ]

instance AWSRequest ListDomains where
    type Sv ListDomains = Route53Domains
    type Rs ListDomains = ListDomainsResponse

    request  = post "ListDomains"
    response = jsonResponse

instance FromJSON ListDomainsResponse where
    parseJSON = withObject "ListDomainsResponse" $ \o -> ListDomainsResponse
        <$> o .:  "Domains"
        <*> o .:? "NextPageMarker"
