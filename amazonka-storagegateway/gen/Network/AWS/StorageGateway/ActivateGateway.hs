{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation activates the gateway you previously deployed on your host.
-- For more information, see Activate the AWS Storage Gateway. In the
-- activation process, you specify information such as the region you want to
-- use for storing snapshots, the time zone for scheduled snapshots the
-- gateway snapshot schedule window, an activation key, and a name for your
-- gateway. The activation process also associates your gateway with your
-- account; for more information, see UpdateGatewayInformation.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ActivateGateway.html>
module Network.AWS.StorageGateway.ActivateGateway
    (
    -- * Request
      ActivateGateway
    -- ** Request constructor
    , activateGateway
    -- ** Request lenses
    , agActivationKey
    , agGatewayName
    , agGatewayRegion
    , agGatewayTimezone
    , agGatewayType
    , agMediumChangerType
    , agTapeDriveType

    -- * Response
    , ActivateGatewayResponse
    -- ** Response constructor
    , activateGatewayResponse
    -- ** Response lenses
    , agrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data ActivateGateway = ActivateGateway
    { _agActivationKey     :: Text
    , _agGatewayName       :: Text
    , _agGatewayRegion     :: Text
    , _agGatewayTimezone   :: Text
    , _agGatewayType       :: Maybe Text
    , _agMediumChangerType :: Maybe Text
    , _agTapeDriveType     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ActivateGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agActivationKey' @::@ 'Text'
--
-- * 'agGatewayName' @::@ 'Text'
--
-- * 'agGatewayRegion' @::@ 'Text'
--
-- * 'agGatewayTimezone' @::@ 'Text'
--
-- * 'agGatewayType' @::@ 'Maybe' 'Text'
--
-- * 'agMediumChangerType' @::@ 'Maybe' 'Text'
--
-- * 'agTapeDriveType' @::@ 'Maybe' 'Text'
--
activateGateway :: Text -- ^ 'agActivationKey'
                -> Text -- ^ 'agGatewayName'
                -> Text -- ^ 'agGatewayTimezone'
                -> Text -- ^ 'agGatewayRegion'
                -> ActivateGateway
activateGateway p1 p2 p3 p4 = ActivateGateway
    { _agActivationKey     = p1
    , _agGatewayName       = p2
    , _agGatewayTimezone   = p3
    , _agGatewayRegion     = p4
    , _agGatewayType       = Nothing
    , _agTapeDriveType     = Nothing
    , _agMediumChangerType = Nothing
    }

-- | Your gateway activation key. You can obtain the activation key by sending
-- an HTTP GET request with redirects enabled to the gateway IP address
-- (port 80). The redirect URL returned in the response provides you the
-- activation key for your gateway in the query string parameter
-- activationKey. It may also include other activation-related parameters,
-- however, these are merely defaults -- the arguments you pass to the
-- ActivateGateway API call determine the actual configuration of your
-- gateway.
agActivationKey :: Lens' ActivateGateway Text
agActivationKey = lens _agActivationKey (\s a -> s { _agActivationKey = a })

agGatewayName :: Lens' ActivateGateway Text
agGatewayName = lens _agGatewayName (\s a -> s { _agGatewayName = a })

-- | One of the values that indicates the region where you want to store the
-- snapshot backups. The gateway region specified must be the same region as
-- the region in your Host header in the request. For more information about
-- available regions and endpoints for AWS Storage Gateway, see Regions and
-- Endpoints in the Amazon Web Services Glossary. Valid Values: "us-east-1",
-- "us-west-1", "us-west-2", "eu-west-1", "eu-central-1", "ap-northeast-1",
-- "ap-southeast-1", "ap-southeast-2", "sa-east-1".
agGatewayRegion :: Lens' ActivateGateway Text
agGatewayRegion = lens _agGatewayRegion (\s a -> s { _agGatewayRegion = a })

-- | One of the values that indicates the time zone you want to set for the
-- gateway. The time zone is used, for example, for scheduling snapshots and
-- your gateway's maintenance schedule.
agGatewayTimezone :: Lens' ActivateGateway Text
agGatewayTimezone =
    lens _agGatewayTimezone (\s a -> s { _agGatewayTimezone = a })

-- | One of the values that defines the type of gateway to activate. The type
-- specified is critical to all later functions of the gateway and cannot be
-- changed after activation. The default value is STORED.
agGatewayType :: Lens' ActivateGateway (Maybe Text)
agGatewayType = lens _agGatewayType (\s a -> s { _agGatewayType = a })

-- | The value that indicates the type of medium changer to use for
-- gateway-VTL. This field is optional. Valid Values: "STK-L700".
agMediumChangerType :: Lens' ActivateGateway (Maybe Text)
agMediumChangerType =
    lens _agMediumChangerType (\s a -> s { _agMediumChangerType = a })

-- | The value that indicates the type of tape drive to use for gateway-VTL.
-- This field is optional. Valid Values: "IBM-ULT3580-TD5".
agTapeDriveType :: Lens' ActivateGateway (Maybe Text)
agTapeDriveType = lens _agTapeDriveType (\s a -> s { _agTapeDriveType = a })

newtype ActivateGatewayResponse = ActivateGatewayResponse
    { _agrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ActivateGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agrGatewayARN' @::@ 'Maybe' 'Text'
--
activateGatewayResponse :: ActivateGatewayResponse
activateGatewayResponse = ActivateGatewayResponse
    { _agrGatewayARN = Nothing
    }

agrGatewayARN :: Lens' ActivateGatewayResponse (Maybe Text)
agrGatewayARN = lens _agrGatewayARN (\s a -> s { _agrGatewayARN = a })

instance ToPath ActivateGateway where
    toPath = const "/"

instance ToQuery ActivateGateway where
    toQuery = const mempty

instance ToHeaders ActivateGateway
instance ToJSON ActivateGateway where
    toJSON = genericToJSON jsonOptions

instance AWSRequest ActivateGateway where
    type Sv ActivateGateway = StorageGateway
    type Rs ActivateGateway = ActivateGatewayResponse

    request  = post
    response = jsonResponse

instance FromJSON ActivateGatewayResponse where
    parseJSON = genericParseJSON jsonOptions
