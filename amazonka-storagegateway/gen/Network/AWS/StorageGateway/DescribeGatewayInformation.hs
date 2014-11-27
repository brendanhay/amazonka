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

-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
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

-- | This operation returns metadata about a gateway such as its name, network
-- interfaces, configured time zone, and the state (whether the gateway is
-- running or not). To specify which gateway to describe, use the Amazon
-- Resource Name (ARN) of the gateway in your request.
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
    , dgirGatewayARN
    , dgirGatewayId
    , dgirGatewayNetworkInterfaces
    , dgirGatewayState
    , dgirGatewayTimezone
    , dgirGatewayType
    , dgirNextUpdateAvailabilityDate
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeGatewayInformation = DescribeGatewayInformation
    { _dgiGatewayARN :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DescribeGatewayInformation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgiGatewayARN' @::@ 'Text'
--
describeGatewayInformation :: Text -- ^ 'dgiGatewayARN'
                           -> DescribeGatewayInformation
describeGatewayInformation p1 = DescribeGatewayInformation
    { _dgiGatewayARN = p1
    }

dgiGatewayARN :: Lens' DescribeGatewayInformation Text
dgiGatewayARN = lens _dgiGatewayARN (\s a -> s { _dgiGatewayARN = a })

data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse
    { _dgirGatewayARN                 :: Maybe Text
    , _dgirGatewayId                  :: Maybe Text
    , _dgirGatewayNetworkInterfaces   :: List "GatewayNetworkInterfaces" NetworkInterface
    , _dgirGatewayState               :: Maybe Text
    , _dgirGatewayTimezone            :: Maybe Text
    , _dgirGatewayType                :: Maybe Text
    , _dgirNextUpdateAvailabilityDate :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeGatewayInformationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgirGatewayARN' @::@ 'Maybe' 'Text'
--
-- * 'dgirGatewayId' @::@ 'Maybe' 'Text'
--
-- * 'dgirGatewayNetworkInterfaces' @::@ ['NetworkInterface']
--
-- * 'dgirGatewayState' @::@ 'Maybe' 'Text'
--
-- * 'dgirGatewayTimezone' @::@ 'Maybe' 'Text'
--
-- * 'dgirGatewayType' @::@ 'Maybe' 'Text'
--
-- * 'dgirNextUpdateAvailabilityDate' @::@ 'Maybe' 'Text'
--
describeGatewayInformationResponse :: DescribeGatewayInformationResponse
describeGatewayInformationResponse = DescribeGatewayInformationResponse
    { _dgirGatewayARN                 = Nothing
    , _dgirGatewayId                  = Nothing
    , _dgirGatewayTimezone            = Nothing
    , _dgirGatewayState               = Nothing
    , _dgirGatewayNetworkInterfaces   = mempty
    , _dgirGatewayType                = Nothing
    , _dgirNextUpdateAvailabilityDate = Nothing
    }

dgirGatewayARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayARN = lens _dgirGatewayARN (\s a -> s { _dgirGatewayARN = a })

-- | The gateway ID.
dgirGatewayId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayId = lens _dgirGatewayId (\s a -> s { _dgirGatewayId = a })

-- | A 'NetworkInterface' array that contains descriptions of the gateway network
-- interfaces.
dgirGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse [NetworkInterface]
dgirGatewayNetworkInterfaces =
    lens _dgirGatewayNetworkInterfaces
        (\s a -> s { _dgirGatewayNetworkInterfaces = a })
            . _List

-- | One of the values that indicates the operating state of the gateway.
dgirGatewayState :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayState = lens _dgirGatewayState (\s a -> s { _dgirGatewayState = a })

-- | One of the values that indicates the time zone configured for the gateway.
dgirGatewayTimezone :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayTimezone =
    lens _dgirGatewayTimezone (\s a -> s { _dgirGatewayTimezone = a })

-- | TBD
dgirGatewayType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayType = lens _dgirGatewayType (\s a -> s { _dgirGatewayType = a })

-- | The date at which an update to the gateway is available. This date is in the
-- time zone of the gateway. If the gateway is not available for an update this
-- field is not returned in the response.
dgirNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirNextUpdateAvailabilityDate =
    lens _dgirNextUpdateAvailabilityDate
        (\s a -> s { _dgirNextUpdateAvailabilityDate = a })

instance ToPath DescribeGatewayInformation where
    toPath = const "/"

instance ToQuery DescribeGatewayInformation where
    toQuery = const mempty

instance ToHeaders DescribeGatewayInformation

instance ToJSON DescribeGatewayInformation where
    toJSON DescribeGatewayInformation{..} = object
        [ "GatewayARN" .= _dgiGatewayARN
        ]

instance AWSRequest DescribeGatewayInformation where
    type Sv DescribeGatewayInformation = StorageGateway
    type Rs DescribeGatewayInformation = DescribeGatewayInformationResponse

    request  = post "DescribeGatewayInformation"
    response = jsonResponse

instance FromJSON DescribeGatewayInformationResponse where
    parseJSON = withObject "DescribeGatewayInformationResponse" $ \o -> DescribeGatewayInformationResponse
        <$> o .:? "GatewayARN"
        <*> o .:? "GatewayId"
        <*> o .:  "GatewayNetworkInterfaces"
        <*> o .:? "GatewayState"
        <*> o .:? "GatewayTimezone"
        <*> o .:? "GatewayType"
        <*> o .:? "NextUpdateAvailabilityDate"
