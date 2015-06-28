{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
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

-- | This operation lists the recovery points for a specified gateway. This
-- operation is supported only for the gateway-cached volume architecture.
--
-- Each gateway-cached volume has one recovery point. A volume recovery
-- point is a point in time at which all data of the volume is consistent
-- and from which you can create a snapshot. To create a snapshot from a
-- volume recovery point use the CreateSnapshotFromVolumeRecoveryPoint
-- operation.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListVolumeRecoveryPoints.html>
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
    (
    -- * Request
      ListVolumeRecoveryPoints
    -- ** Request constructor
    , listVolumeRecoveryPoints
    -- ** Request lenses
    , lvrpGatewayARN

    -- * Response
    , ListVolumeRecoveryPointsResponse
    -- ** Response constructor
    , listVolumeRecoveryPointsResponse
    -- ** Response lenses
    , lvrprVolumeRecoveryPointInfos
    , lvrprGatewayARN
    , lvrprStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'listVolumeRecoveryPoints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvrpGatewayARN'
newtype ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
    { _lvrpGatewayARN :: Text
    } deriving (Eq,Read,Show)

-- | 'ListVolumeRecoveryPoints' smart constructor.
listVolumeRecoveryPoints :: Text -> ListVolumeRecoveryPoints
listVolumeRecoveryPoints pGatewayARN =
    ListVolumeRecoveryPoints'
    { _lvrpGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
lvrpGatewayARN :: Lens' ListVolumeRecoveryPoints Text
lvrpGatewayARN = lens _lvrpGatewayARN (\ s a -> s{_lvrpGatewayARN = a});

instance AWSRequest ListVolumeRecoveryPoints where
        type Sv ListVolumeRecoveryPoints = StorageGateway
        type Rs ListVolumeRecoveryPoints =
             ListVolumeRecoveryPointsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumeRecoveryPointsResponse' <$>
                   (x .?> "VolumeRecoveryPointInfos" .!@ mempty) <*>
                     (x .?> "GatewayARN")
                     <*> (pure s))

instance ToHeaders ListVolumeRecoveryPoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListVolumeRecoveryPoints"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListVolumeRecoveryPoints where
        toJSON ListVolumeRecoveryPoints'{..}
          = object ["GatewayARN" .= _lvrpGatewayARN]

instance ToPath ListVolumeRecoveryPoints where
        toPath = const "/"

instance ToQuery ListVolumeRecoveryPoints where
        toQuery = const mempty

-- | /See:/ 'listVolumeRecoveryPointsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvrprVolumeRecoveryPointInfos'
--
-- * 'lvrprGatewayARN'
--
-- * 'lvrprStatus'
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
    { _lvrprVolumeRecoveryPointInfos :: !(Maybe [VolumeRecoveryPointInfo])
    , _lvrprGatewayARN               :: !(Maybe Text)
    , _lvrprStatus                   :: !Status
    } deriving (Eq,Read,Show)

-- | 'ListVolumeRecoveryPointsResponse' smart constructor.
listVolumeRecoveryPointsResponse :: Status -> ListVolumeRecoveryPointsResponse
listVolumeRecoveryPointsResponse pStatus =
    ListVolumeRecoveryPointsResponse'
    { _lvrprVolumeRecoveryPointInfos = Nothing
    , _lvrprGatewayARN = Nothing
    , _lvrprStatus = pStatus
    }

-- | FIXME: Undocumented member.
lvrprVolumeRecoveryPointInfos :: Lens' ListVolumeRecoveryPointsResponse [VolumeRecoveryPointInfo]
lvrprVolumeRecoveryPointInfos = lens _lvrprVolumeRecoveryPointInfos (\ s a -> s{_lvrprVolumeRecoveryPointInfos = a}) . _Default;

-- | FIXME: Undocumented member.
lvrprGatewayARN :: Lens' ListVolumeRecoveryPointsResponse (Maybe Text)
lvrprGatewayARN = lens _lvrprGatewayARN (\ s a -> s{_lvrprGatewayARN = a});

-- | FIXME: Undocumented member.
lvrprStatus :: Lens' ListVolumeRecoveryPointsResponse Status
lvrprStatus = lens _lvrprStatus (\ s a -> s{_lvrprStatus = a});
