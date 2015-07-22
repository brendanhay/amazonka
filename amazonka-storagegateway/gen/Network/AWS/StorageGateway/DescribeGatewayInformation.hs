{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns metadata about a gateway such as its name,
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
    , dgirqGatewayARN

    -- * Response
    , DescribeGatewayInformationResponse
    -- ** Response constructor
    , describeGatewayInformationResponse
    -- ** Response lenses
    , dgirsGatewayState
    , dgirsGatewayARN
    , dgirsGatewayNetworkInterfaces
    , dgirsNextUpdateAvailabilityDate
    , dgirsLastSoftwareUpdate
    , dgirsGatewayId
    , dgirsGatewayType
    , dgirsGatewayTimezone
    , dgirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the id of the gateway.
--
-- /See:/ 'describeGatewayInformation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgirqGatewayARN'
newtype DescribeGatewayInformation = DescribeGatewayInformation'
    { _dgirqGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeGatewayInformation' smart constructor.
describeGatewayInformation :: Text -> DescribeGatewayInformation
describeGatewayInformation pGatewayARN =
    DescribeGatewayInformation'
    { _dgirqGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
dgirqGatewayARN :: Lens' DescribeGatewayInformation Text
dgirqGatewayARN = lens _dgirqGatewayARN (\ s a -> s{_dgirqGatewayARN = a});

instance AWSRequest DescribeGatewayInformation where
        type Sv DescribeGatewayInformation = StorageGateway
        type Rs DescribeGatewayInformation =
             DescribeGatewayInformationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGatewayInformationResponse' <$>
                   (x .?> "GatewayState") <*> (x .?> "GatewayARN") <*>
                     (x .?> "GatewayNetworkInterfaces" .!@ mempty)
                     <*> (x .?> "NextUpdateAvailabilityDate")
                     <*> (x .?> "LastSoftwareUpdate")
                     <*> (x .?> "GatewayId")
                     <*> (x .?> "GatewayType")
                     <*> (x .?> "GatewayTimezone")
                     <*> (pure (fromEnum s)))

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
          = object ["GatewayARN" .= _dgirqGatewayARN]

instance ToPath DescribeGatewayInformation where
        toPath = const "/"

instance ToQuery DescribeGatewayInformation where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'describeGatewayInformationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgirsGatewayState'
--
-- * 'dgirsGatewayARN'
--
-- * 'dgirsGatewayNetworkInterfaces'
--
-- * 'dgirsNextUpdateAvailabilityDate'
--
-- * 'dgirsLastSoftwareUpdate'
--
-- * 'dgirsGatewayId'
--
-- * 'dgirsGatewayType'
--
-- * 'dgirsGatewayTimezone'
--
-- * 'dgirsStatus'
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'
    { _dgirsGatewayState               :: !(Maybe Text)
    , _dgirsGatewayARN                 :: !(Maybe Text)
    , _dgirsGatewayNetworkInterfaces   :: !(Maybe [NetworkInterface])
    , _dgirsNextUpdateAvailabilityDate :: !(Maybe Text)
    , _dgirsLastSoftwareUpdate         :: !(Maybe Text)
    , _dgirsGatewayId                  :: !(Maybe Text)
    , _dgirsGatewayType                :: !(Maybe Text)
    , _dgirsGatewayTimezone            :: !(Maybe Text)
    , _dgirsStatus                     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeGatewayInformationResponse' smart constructor.
describeGatewayInformationResponse :: Int -> DescribeGatewayInformationResponse
describeGatewayInformationResponse pStatus =
    DescribeGatewayInformationResponse'
    { _dgirsGatewayState = Nothing
    , _dgirsGatewayARN = Nothing
    , _dgirsGatewayNetworkInterfaces = Nothing
    , _dgirsNextUpdateAvailabilityDate = Nothing
    , _dgirsLastSoftwareUpdate = Nothing
    , _dgirsGatewayId = Nothing
    , _dgirsGatewayType = Nothing
    , _dgirsGatewayTimezone = Nothing
    , _dgirsStatus = pStatus
    }

-- | One of the values that indicates the operating state of the gateway.
dgirsGatewayState :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayState = lens _dgirsGatewayState (\ s a -> s{_dgirsGatewayState = a});

-- | FIXME: Undocumented member.
dgirsGatewayARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayARN = lens _dgirsGatewayARN (\ s a -> s{_dgirsGatewayARN = a});

-- | A NetworkInterface array that contains descriptions of the gateway
-- network interfaces.
dgirsGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse [NetworkInterface]
dgirsGatewayNetworkInterfaces = lens _dgirsGatewayNetworkInterfaces (\ s a -> s{_dgirsGatewayNetworkInterfaces = a}) . _Default;

-- | The date on which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an
-- update this field is not returned in the response.
dgirsNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsNextUpdateAvailabilityDate = lens _dgirsNextUpdateAvailabilityDate (\ s a -> s{_dgirsNextUpdateAvailabilityDate = a});

-- | The date on which the last software update was applied to the gateway.
-- If the gateway has never been updated, this field does not return a
-- value in the response.
dgirsLastSoftwareUpdate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsLastSoftwareUpdate = lens _dgirsLastSoftwareUpdate (\ s a -> s{_dgirsLastSoftwareUpdate = a});

-- | The gateway ID.
dgirsGatewayId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayId = lens _dgirsGatewayId (\ s a -> s{_dgirsGatewayId = a});

-- | The type of the gateway.
dgirsGatewayType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayType = lens _dgirsGatewayType (\ s a -> s{_dgirsGatewayType = a});

-- | One of the values that indicates the time zone configured for the
-- gateway.
dgirsGatewayTimezone :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayTimezone = lens _dgirsGatewayTimezone (\ s a -> s{_dgirsGatewayTimezone = a});

-- | FIXME: Undocumented member.
dgirsStatus :: Lens' DescribeGatewayInformationResponse Int
dgirsStatus = lens _dgirsStatus (\ s a -> s{_dgirsStatus = a});
