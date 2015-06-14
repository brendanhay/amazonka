{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.ActivateGateway
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

-- | This operation activates the gateway you previously deployed on your
-- host. For more information, see
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedActivateGateway-common.html Activate the AWS Storage Gateway>.
-- In the activation process, you specify information such as the region
-- you want to use for storing snapshots, the time zone for scheduled
-- snapshots the gateway snapshot schedule window, an activation key, and a
-- name for your gateway. The activation process also associates your
-- gateway with your account; for more information, see
-- UpdateGatewayInformation.
--
-- You must turn on the gateway VM before you can activate your gateway.
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
    , agGatewayTimezone
    , agGatewayRegion
    , agMediumChangerType
    , agTapeDriveType
    , agGatewayType

    -- * Response
    , ActivateGatewayResponse
    -- ** Response constructor
    , activateGatewayResponse
    -- ** Response lenses
    , agrGatewayARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'activateGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agActivationKey'
--
-- * 'agGatewayName'
--
-- * 'agGatewayTimezone'
--
-- * 'agGatewayRegion'
--
-- * 'agMediumChangerType'
--
-- * 'agTapeDriveType'
--
-- * 'agGatewayType'
data ActivateGateway = ActivateGateway'{_agActivationKey :: Text, _agGatewayName :: Text, _agGatewayTimezone :: Text, _agGatewayRegion :: Text, _agMediumChangerType :: Text, _agTapeDriveType :: Text, _agGatewayType :: Text} deriving (Eq, Read, Show)

-- | 'ActivateGateway' smart constructor.
activateGateway :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> ActivateGateway
activateGateway pActivationKey pGatewayName pGatewayTimezone pGatewayRegion pMediumChangerType pTapeDriveType pGatewayType = ActivateGateway'{_agActivationKey = pActivationKey, _agGatewayName = pGatewayName, _agGatewayTimezone = pGatewayTimezone, _agGatewayRegion = pGatewayRegion, _agMediumChangerType = pMediumChangerType, _agTapeDriveType = pTapeDriveType, _agGatewayType = pGatewayType};

-- | Your gateway activation key. You can obtain the activation key by
-- sending an HTTP GET request with redirects enabled to the gateway IP
-- address (port 80). The redirect URL returned in the response provides
-- you the activation key for your gateway in the query string parameter
-- @activationKey@. It may also include other activation-related
-- parameters, however, these are merely defaults -- the arguments you pass
-- to the @ActivateGateway@ API call determine the actual configuration of
-- your gateway.
agActivationKey :: Lens' ActivateGateway Text
agActivationKey = lens _agActivationKey (\ s a -> s{_agActivationKey = a});

-- | FIXME: Undocumented member.
agGatewayName :: Lens' ActivateGateway Text
agGatewayName = lens _agGatewayName (\ s a -> s{_agGatewayName = a});

-- | One of the values that indicates the time zone you want to set for the
-- gateway. The time zone is used, for example, for scheduling snapshots
-- and your gateway\'s maintenance schedule.
agGatewayTimezone :: Lens' ActivateGateway Text
agGatewayTimezone = lens _agGatewayTimezone (\ s a -> s{_agGatewayTimezone = a});

-- | One of the values that indicates the region where you want to store the
-- snapshot backups. The gateway region specified must be the same region
-- as the region in your @Host@ header in the request. For more information
-- about available regions and endpoints for AWS Storage Gateway, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#sg_region Regions and Endpoints>
-- in the /Amazon Web Services Glossary/.
--
-- /Valid Values/: \"us-east-1\", \"us-west-1\", \"us-west-2\",
-- \"eu-west-1\", \"eu-central-1\", \"ap-northeast-1\", \"ap-southeast-1\",
-- \"ap-southeast-2\", \"sa-east-1\"
agGatewayRegion :: Lens' ActivateGateway Text
agGatewayRegion = lens _agGatewayRegion (\ s a -> s{_agGatewayRegion = a});

-- | The value that indicates the type of medium changer to use for
-- gateway-VTL. This field is optional.
--
-- /Valid Values/: \"STK-L700\", \"AWS-Gateway-VTL\"
agMediumChangerType :: Lens' ActivateGateway Text
agMediumChangerType = lens _agMediumChangerType (\ s a -> s{_agMediumChangerType = a});

-- | The value that indicates the type of tape drive to use for gateway-VTL.
-- This field is optional.
--
-- /Valid Values/: \"IBM-ULT3580-TD5\"
agTapeDriveType :: Lens' ActivateGateway Text
agTapeDriveType = lens _agTapeDriveType (\ s a -> s{_agTapeDriveType = a});

-- | One of the values that defines the type of gateway to activate. The type
-- specified is critical to all later functions of the gateway and cannot
-- be changed after activation. The default value is @STORED@.
agGatewayType :: Lens' ActivateGateway Text
agGatewayType = lens _agGatewayType (\ s a -> s{_agGatewayType = a});

instance AWSRequest ActivateGateway where
        type Sv ActivateGateway = StorageGateway
        type Rs ActivateGateway = ActivateGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ActivateGatewayResponse' <$> x .:> "GatewayARN")

instance ToHeaders ActivateGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ActivateGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ActivateGateway where
        toJSON ActivateGateway'{..}
          = object
              ["ActivationKey" .= _agActivationKey,
               "GatewayName" .= _agGatewayName,
               "GatewayTimezone" .= _agGatewayTimezone,
               "GatewayRegion" .= _agGatewayRegion,
               "MediumChangerType" .= _agMediumChangerType,
               "TapeDriveType" .= _agTapeDriveType,
               "GatewayType" .= _agGatewayType]

instance ToPath ActivateGateway where
        toPath = const "/"

instance ToQuery ActivateGateway where
        toQuery = const mempty

-- | /See:/ 'activateGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agrGatewayARN'
newtype ActivateGatewayResponse = ActivateGatewayResponse'{_agrGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'ActivateGatewayResponse' smart constructor.
activateGatewayResponse :: Text -> ActivateGatewayResponse
activateGatewayResponse pGatewayARN = ActivateGatewayResponse'{_agrGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
agrGatewayARN :: Lens' ActivateGatewayResponse Text
agrGatewayARN = lens _agrGatewayARN (\ s a -> s{_agrGatewayARN = a});
