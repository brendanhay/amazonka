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

-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists gateways owned by an AWS account in a region specified
-- in the request. The returned list is ordered by gateway Amazon Resource
-- Name (ARN). By default, the operation returns a maximum of 100 gateways.
-- This operation supports pagination that allows you to optionally reduce the
-- number of gateways returned in a response. If you have more gateways than
-- are returned in a response-that is, the response returns only a truncated
-- list of your gateways-the response contains a marker that you can specify
-- in your next request to fetch the next page of gateways.
module Network.AWS.StorageGateway.ListGateways
    (
    -- * Request
      ListGateways
    -- ** Request constructor
    , listGateways
    -- ** Request lenses
    , lgLimit
    , lgMarker

    -- * Response
    , ListGatewaysResponse
    -- ** Response constructor
    , listGatewaysResponse
    -- ** Response lenses
    , lgrGateways
    , lgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data ListGateways = ListGateways
    { _lgLimit  :: Maybe Natural
    , _lgMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListGateways' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgLimit' @::@ 'Maybe' 'Natural'
--
-- * 'lgMarker' @::@ 'Maybe' 'Text'
--
listGateways :: ListGateways
listGateways = ListGateways
    { _lgMarker = Nothing
    , _lgLimit  = Nothing
    }

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
lgLimit :: Lens' ListGateways (Maybe Natural)
lgLimit = lens _lgLimit (\s a -> s { _lgLimit = a })

-- | An opaque string that indicates the position at which to begin the
-- returned list of gateways.
lgMarker :: Lens' ListGateways (Maybe Text)
lgMarker = lens _lgMarker (\s a -> s { _lgMarker = a })

instance ToPath ListGateways where
    toPath = const "/"

instance ToQuery ListGateways where
    toQuery = const mempty

instance ToHeaders ListGateways

instance ToBody ListGateways where
    toBody = toBody . encode . _lgMarker

data ListGatewaysResponse = ListGatewaysResponse
    { _lgrGateways :: [GatewayInfo]
    , _lgrMarker   :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListGatewaysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrGateways' @::@ ['GatewayInfo']
--
-- * 'lgrMarker' @::@ 'Maybe' 'Text'
--
listGatewaysResponse :: ListGatewaysResponse
listGatewaysResponse = ListGatewaysResponse
    { _lgrGateways = mempty
    , _lgrMarker   = Nothing
    }

lgrGateways :: Lens' ListGatewaysResponse [GatewayInfo]
lgrGateways = lens _lgrGateways (\s a -> s { _lgrGateways = a })

lgrMarker :: Lens' ListGatewaysResponse (Maybe Text)
lgrMarker = lens _lgrMarker (\s a -> s { _lgrMarker = a })

instance AWSRequest ListGateways where
    type Sv ListGateways = StorageGateway
    type Rs ListGateways = ListGatewaysResponse

    request  = post
    response = jsonResponse $ \h o -> ListGatewaysResponse
        <$> o .: "Gateways"
        <*> o .: "Marker"
