{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
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

-- | This operation returns metadata about a gateway such as its name,
-- network interfaces, configured time zone, and the state (whether the
-- gateway is running or not). To specify which gateway to describe, use
-- the Amazon Resource Name (ARN) of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeGatewayInformation.html>
module Network.AWS.StorageGateway.DescribeGatewayInformation
    (
    -- * Request
      DescribeGatewayInformation
    -- ** Request constructor
    , describeGatewayInformation
    -- ** Request lenses
    , dgiGatewayARN

    -- * Response
    , DescribeGatewayInformationResponse
    -- ** Response constructor
    , describeGatewayInformationResponse
    -- ** Response lenses
    , dgirGatewayNetworkInterfaces
    , dgirGatewayState
    , dgirGatewayARN
    , dgirNextUpdateAvailabilityDate
    , dgirLastSoftwareUpdate
    , dgirGatewayId
    , dgirGatewayType
    , dgirGatewayTimezone
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeGatewayInformation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgiGatewayARN'
newtype DescribeGatewayInformation = DescribeGatewayInformation'{_dgiGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeGatewayInformation' smart constructor.
describeGatewayInformation :: Text -> DescribeGatewayInformation
describeGatewayInformation pGatewayARN = DescribeGatewayInformation'{_dgiGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
dgiGatewayARN :: Lens' DescribeGatewayInformation Text
dgiGatewayARN = lens _dgiGatewayARN (\ s a -> s{_dgiGatewayARN = a});

instance AWSRequest DescribeGatewayInformation where
        type Sv DescribeGatewayInformation = StorageGateway
        type Rs DescribeGatewayInformation =
             DescribeGatewayInformationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGatewayInformationResponse' <$>
                   x .?> "GatewayNetworkInterfaces" .!@ mempty <*>
                     x .:> "GatewayState"
                     <*> x .:> "GatewayARN"
                     <*> x .:> "NextUpdateAvailabilityDate"
                     <*> x .:> "LastSoftwareUpdate"
                     <*> x .:> "GatewayId"
                     <*> x .:> "GatewayType"
                     <*> x .:> "GatewayTimezone")

instance ToHeaders DescribeGatewayInformation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeGatewayInformation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGatewayInformation where
        toJSON DescribeGatewayInformation'{..}
          = object ["GatewayARN" .= _dgiGatewayARN]

instance ToPath DescribeGatewayInformation where
        toPath = const "/"

instance ToQuery DescribeGatewayInformation where
        toQuery = const mempty

-- | /See:/ 'describeGatewayInformationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgirGatewayNetworkInterfaces'
--
-- * 'dgirGatewayState'
--
-- * 'dgirGatewayARN'
--
-- * 'dgirNextUpdateAvailabilityDate'
--
-- * 'dgirLastSoftwareUpdate'
--
-- * 'dgirGatewayId'
--
-- * 'dgirGatewayType'
--
-- * 'dgirGatewayTimezone'
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'{_dgirGatewayNetworkInterfaces :: [NetworkInterface], _dgirGatewayState :: Text, _dgirGatewayARN :: Text, _dgirNextUpdateAvailabilityDate :: Text, _dgirLastSoftwareUpdate :: Text, _dgirGatewayId :: Text, _dgirGatewayType :: Text, _dgirGatewayTimezone :: Text} deriving (Eq, Read, Show)

-- | 'DescribeGatewayInformationResponse' smart constructor.
describeGatewayInformationResponse :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> DescribeGatewayInformationResponse
describeGatewayInformationResponse pGatewayState pGatewayARN pNextUpdateAvailabilityDate pLastSoftwareUpdate pGatewayId pGatewayType pGatewayTimezone = DescribeGatewayInformationResponse'{_dgirGatewayNetworkInterfaces = mempty, _dgirGatewayState = pGatewayState, _dgirGatewayARN = pGatewayARN, _dgirNextUpdateAvailabilityDate = pNextUpdateAvailabilityDate, _dgirLastSoftwareUpdate = pLastSoftwareUpdate, _dgirGatewayId = pGatewayId, _dgirGatewayType = pGatewayType, _dgirGatewayTimezone = pGatewayTimezone};

-- | A NetworkInterface array that contains descriptions of the gateway
-- network interfaces.
dgirGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse [NetworkInterface]
dgirGatewayNetworkInterfaces = lens _dgirGatewayNetworkInterfaces (\ s a -> s{_dgirGatewayNetworkInterfaces = a});

-- | One of the values that indicates the operating state of the gateway.
dgirGatewayState :: Lens' DescribeGatewayInformationResponse Text
dgirGatewayState = lens _dgirGatewayState (\ s a -> s{_dgirGatewayState = a});

-- | FIXME: Undocumented member.
dgirGatewayARN :: Lens' DescribeGatewayInformationResponse Text
dgirGatewayARN = lens _dgirGatewayARN (\ s a -> s{_dgirGatewayARN = a});

-- | The date on which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an
-- update this field is not returned in the response.
dgirNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse Text
dgirNextUpdateAvailabilityDate = lens _dgirNextUpdateAvailabilityDate (\ s a -> s{_dgirNextUpdateAvailabilityDate = a});

-- | The date on which the last software update was applied to the gateway.
-- If the gateway has never been updated, this field does not return a
-- value in the response.
dgirLastSoftwareUpdate :: Lens' DescribeGatewayInformationResponse Text
dgirLastSoftwareUpdate = lens _dgirLastSoftwareUpdate (\ s a -> s{_dgirLastSoftwareUpdate = a});

-- | The gateway ID.
dgirGatewayId :: Lens' DescribeGatewayInformationResponse Text
dgirGatewayId = lens _dgirGatewayId (\ s a -> s{_dgirGatewayId = a});

-- | The type of the gateway.
dgirGatewayType :: Lens' DescribeGatewayInformationResponse Text
dgirGatewayType = lens _dgirGatewayType (\ s a -> s{_dgirGatewayType = a});

-- | One of the values that indicates the time zone configured for the
-- gateway.
dgirGatewayTimezone :: Lens' DescribeGatewayInformationResponse Text
dgirGatewayTimezone = lens _dgirGatewayTimezone (\ s a -> s{_dgirGatewayTimezone = a});
