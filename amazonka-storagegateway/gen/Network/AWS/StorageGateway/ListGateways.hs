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

-- Module      : Network.AWS.StorageGateway.ListGateways
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

-- | This operation lists gateways owned by an AWS account in a region specified
-- in the request. The returned list is ordered by gateway Amazon Resource Name
-- (ARN).
--
-- By default, the operation returns a maximum of 100 gateways. This operation
-- supports pagination that allows you to optionally reduce the number of
-- gateways returned in a response.
--
-- If you have more gateways than are returned in a response-that is, the
-- response returns only a truncated list of your gateways-the response contains
-- a marker that you can specify in your next request to fetch the next page of
-- gateways.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListGateways.html>
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
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data ListGateways = ListGateways
    { _lgLimit  :: Maybe Nat
    , _lgMarker :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

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
lgLimit = lens _lgLimit (\s a -> s { _lgLimit = a }) . mapping _Nat

-- | An opaque string that indicates the position at which to begin the returned
-- list of gateways.
lgMarker :: Lens' ListGateways (Maybe Text)
lgMarker = lens _lgMarker (\s a -> s { _lgMarker = a })

data ListGatewaysResponse = ListGatewaysResponse
    { _lgrGateways :: List "Gateways" GatewayInfo
    , _lgrMarker   :: Maybe Text
    } deriving (Eq, Read, Show)

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
lgrGateways = lens _lgrGateways (\s a -> s { _lgrGateways = a }) . _List

lgrMarker :: Lens' ListGatewaysResponse (Maybe Text)
lgrMarker = lens _lgrMarker (\s a -> s { _lgrMarker = a })

instance ToPath ListGateways where
    toPath = const "/"

instance ToQuery ListGateways where
    toQuery = const mempty

instance ToHeaders ListGateways

instance ToJSON ListGateways where
    toJSON ListGateways{..} = object
        [ "Marker" .= _lgMarker
        , "Limit"  .= _lgLimit
        ]

instance AWSRequest ListGateways where
    type Sv ListGateways = StorageGateway
    type Rs ListGateways = ListGatewaysResponse

    request  = post "ListGateways"
    response = jsonResponse

instance FromJSON ListGatewaysResponse where
    parseJSON = withObject "ListGatewaysResponse" $ \o -> ListGatewaysResponse
        <$> o .:? "Gateways" .!= mempty
        <*> o .:? "Marker"

instance AWSPager ListGateways where
    page rq rs
        | stop (rs ^. lgrMarker) = Nothing
        | otherwise = (\x -> rq & lgMarker ?~ x)
            <$> (rs ^. lgrMarker)
