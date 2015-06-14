{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation lists gateways owned by an AWS account in a region
-- specified in the request. The returned list is ordered by gateway Amazon
-- Resource Name (ARN).
--
-- By default, the operation returns a maximum of 100 gateways. This
-- operation supports pagination that allows you to optionally reduce the
-- number of gateways returned in a response.
--
-- If you have more gateways than are returned in a response-that is, the
-- response returns only a truncated list of your gateways-the response
-- contains a marker that you can specify in your next request to fetch the
-- next page of gateways.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListGateways.html>
module Network.AWS.StorageGateway.ListGateways
    (
    -- * Request
      ListGateways
    -- ** Request constructor
    , listGateways
    -- ** Request lenses
    , lgMarker
    , lgLimit

    -- * Response
    , ListGatewaysResponse
    -- ** Response constructor
    , listGatewaysResponse
    -- ** Response lenses
    , lgrGateways
    , lgrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'listGateways' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgMarker'
--
-- * 'lgLimit'
data ListGateways = ListGateways'{_lgMarker :: Text, _lgLimit :: Nat} deriving (Eq, Read, Show)

-- | 'ListGateways' smart constructor.
listGateways :: Text -> Natural -> ListGateways
listGateways pMarker pLimit = ListGateways'{_lgMarker = pMarker, _lgLimit = _Nat # pLimit};

-- | An opaque string that indicates the position at which to begin the
-- returned list of gateways.
lgMarker :: Lens' ListGateways Text
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
lgLimit :: Lens' ListGateways Natural
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . _Nat;

instance AWSRequest ListGateways where
        type Sv ListGateways = StorageGateway
        type Rs ListGateways = ListGatewaysResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListGatewaysResponse' <$>
                   x .?> "Gateways" .!@ mempty <*> x .:> "Marker")

instance ToHeaders ListGateways where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListGateways" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGateways where
        toJSON ListGateways'{..}
          = object ["Marker" .= _lgMarker, "Limit" .= _lgLimit]

instance ToPath ListGateways where
        toPath = const "/"

instance ToQuery ListGateways where
        toQuery = const mempty

-- | /See:/ 'listGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrGateways'
--
-- * 'lgrMarker'
data ListGatewaysResponse = ListGatewaysResponse'{_lgrGateways :: [GatewayInfo], _lgrMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGatewaysResponse' smart constructor.
listGatewaysResponse :: Text -> ListGatewaysResponse
listGatewaysResponse pMarker = ListGatewaysResponse'{_lgrGateways = mempty, _lgrMarker = pMarker};

-- | FIXME: Undocumented member.
lgrGateways :: Lens' ListGatewaysResponse [GatewayInfo]
lgrGateways = lens _lgrGateways (\ s a -> s{_lgrGateways = a});

-- | FIXME: Undocumented member.
lgrMarker :: Lens' ListGatewaysResponse Text
lgrMarker = lens _lgrMarker (\ s a -> s{_lgrMarker = a});
