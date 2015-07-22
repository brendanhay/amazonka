{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation lists gateways owned by an AWS account in a region
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
    , lgrqMarker
    , lgrqLimit

    -- * Response
    , ListGatewaysResponse
    -- ** Response constructor
    , listGatewaysResponse
    -- ** Response lenses
    , lgrsMarker
    , lgrsGateways
    , lgrsStatus
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
-- * 'lgrqMarker'
--
-- * 'lgrqLimit'
data ListGateways = ListGateways'
    { _lgrqMarker :: !(Maybe Text)
    , _lgrqLimit  :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGateways' smart constructor.
listGateways :: ListGateways
listGateways =
    ListGateways'
    { _lgrqMarker = Nothing
    , _lgrqLimit = Nothing
    }

-- | An opaque string that indicates the position at which to begin the
-- returned list of gateways.
lgrqMarker :: Lens' ListGateways (Maybe Text)
lgrqMarker = lens _lgrqMarker (\ s a -> s{_lgrqMarker = a});

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
lgrqLimit :: Lens' ListGateways (Maybe Natural)
lgrqLimit = lens _lgrqLimit (\ s a -> s{_lgrqLimit = a}) . mapping _Nat;

instance AWSPager ListGateways where
        page rq rs
          | stop (rs ^. lgrsMarker) = Nothing
          | stop (rs ^. lgrsGateways) = Nothing
          | otherwise =
            Just $ rq & lgrqMarker .~ rs ^. lgrsMarker

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
          = object
              ["Marker" .= _lgrqMarker, "Limit" .= _lgrqLimit]

instance ToPath ListGateways where
        toPath = const "/"

instance ToQuery ListGateways where
        toQuery = const mempty

-- | /See:/ 'listGatewaysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrsMarker'
--
-- * 'lgrsGateways'
--
-- * 'lgrsStatus'
data ListGatewaysResponse = ListGatewaysResponse'
    { _lgrsMarker   :: !(Maybe Text)
    , _lgrsGateways :: !(Maybe [GatewayInfo])
    , _lgrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGatewaysResponse' smart constructor.
listGatewaysResponse :: Int -> ListGatewaysResponse
listGatewaysResponse pStatus_ =
    ListGatewaysResponse'
    { _lgrsMarker = Nothing
    , _lgrsGateways = Nothing
    , _lgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
lgrsMarker :: Lens' ListGatewaysResponse (Maybe Text)
lgrsMarker = lens _lgrsMarker (\ s a -> s{_lgrsMarker = a});

-- | FIXME: Undocumented member.
lgrsGateways :: Lens' ListGatewaysResponse [GatewayInfo]
lgrsGateways = lens _lgrsGateways (\ s a -> s{_lgrsGateways = a}) . _Default;

-- | FIXME: Undocumented member.
lgrsStatus :: Lens' ListGatewaysResponse Int
lgrsStatus = lens _lgrsStatus (\ s a -> s{_lgrsStatus = a});
