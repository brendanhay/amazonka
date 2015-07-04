{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , lgrMarker
    , lgrGateways
    , lgrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing zero or more of the following fields:
--
-- -   ListGatewaysInput$Limit
-- -   ListGatewaysInput$Marker
--
-- /See:/ 'listGateways' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgMarker'
--
-- * 'lgLimit'
data ListGateways = ListGateways'
    { _lgMarker :: !(Maybe Text)
    , _lgLimit  :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGateways' smart constructor.
listGateways :: ListGateways
listGateways =
    ListGateways'
    { _lgMarker = Nothing
    , _lgLimit = Nothing
    }

-- | An opaque string that indicates the position at which to begin the
-- returned list of gateways.
lgMarker :: Lens' ListGateways (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
lgLimit :: Lens' ListGateways (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat;

instance AWSPager ListGateways where
        page rq rs
          | stop (rs ^. lgrMarker) = Nothing
          | stop (rs ^. lgrGateways) = Nothing
          | otherwise = Just $ rq & lgMarker .~ rs ^. lgrMarker

instance AWSRequest ListGateways where
        type Sv ListGateways = StorageGateway
        type Rs ListGateways = ListGatewaysResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListGatewaysResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Gateways" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
-- * 'lgrMarker'
--
-- * 'lgrGateways'
--
-- * 'lgrStatus'
data ListGatewaysResponse = ListGatewaysResponse'
    { _lgrMarker   :: !(Maybe Text)
    , _lgrGateways :: !(Maybe [GatewayInfo])
    , _lgrStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGatewaysResponse' smart constructor.
listGatewaysResponse :: Int -> ListGatewaysResponse
listGatewaysResponse pStatus =
    ListGatewaysResponse'
    { _lgrMarker = Nothing
    , _lgrGateways = Nothing
    , _lgrStatus = pStatus
    }

-- | FIXME: Undocumented member.
lgrMarker :: Lens' ListGatewaysResponse (Maybe Text)
lgrMarker = lens _lgrMarker (\ s a -> s{_lgrMarker = a});

-- | FIXME: Undocumented member.
lgrGateways :: Lens' ListGatewaysResponse [GatewayInfo]
lgrGateways = lens _lgrGateways (\ s a -> s{_lgrGateways = a}) . _Default;

-- | FIXME: Undocumented member.
lgrStatus :: Lens' ListGatewaysResponse Int
lgrStatus = lens _lgrStatus (\ s a -> s{_lgrStatus = a});
