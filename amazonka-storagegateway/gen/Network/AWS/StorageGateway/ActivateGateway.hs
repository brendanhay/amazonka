{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation activates the gateway you previously deployed on your
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
    , agrqMediumChangerType
    , agrqTapeDriveType
    , agrqGatewayType
    , agrqActivationKey
    , agrqGatewayName
    , agrqGatewayTimezone
    , agrqGatewayRegion

    -- * Response
    , ActivateGatewayResponse
    -- ** Response constructor
    , activateGatewayResponse
    -- ** Response lenses
    , agrsGatewayARN
    , agrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   ActivateGatewayInput$ActivationKey
-- -   GatewayName
-- -   ActivateGatewayInput$GatewayRegion
-- -   ActivateGatewayInput$GatewayTimezone
-- -   ActivateGatewayInput$GatewayType
-- -   ActivateGatewayInput$TapeDriveType
-- -   ActivateGatewayInput$MediumChangerType
--
-- /See:/ 'activateGateway' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agrqMediumChangerType'
--
-- * 'agrqTapeDriveType'
--
-- * 'agrqGatewayType'
--
-- * 'agrqActivationKey'
--
-- * 'agrqGatewayName'
--
-- * 'agrqGatewayTimezone'
--
-- * 'agrqGatewayRegion'
data ActivateGateway = ActivateGateway'
    { _agrqMediumChangerType :: !(Maybe Text)
    , _agrqTapeDriveType     :: !(Maybe Text)
    , _agrqGatewayType       :: !(Maybe Text)
    , _agrqActivationKey     :: !Text
    , _agrqGatewayName       :: !Text
    , _agrqGatewayTimezone   :: !Text
    , _agrqGatewayRegion     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActivateGateway' smart constructor.
activateGateway :: Text -> Text -> Text -> Text -> ActivateGateway
activateGateway pActivationKey pGatewayName pGatewayTimezone pGatewayRegion =
    ActivateGateway'
    { _agrqMediumChangerType = Nothing
    , _agrqTapeDriveType = Nothing
    , _agrqGatewayType = Nothing
    , _agrqActivationKey = pActivationKey
    , _agrqGatewayName = pGatewayName
    , _agrqGatewayTimezone = pGatewayTimezone
    , _agrqGatewayRegion = pGatewayRegion
    }

-- | The value that indicates the type of medium changer to use for
-- gateway-VTL. This field is optional.
--
-- /Valid Values/: \"STK-L700\", \"AWS-Gateway-VTL\"
agrqMediumChangerType :: Lens' ActivateGateway (Maybe Text)
agrqMediumChangerType = lens _agrqMediumChangerType (\ s a -> s{_agrqMediumChangerType = a});

-- | The value that indicates the type of tape drive to use for gateway-VTL.
-- This field is optional.
--
-- /Valid Values/: \"IBM-ULT3580-TD5\"
agrqTapeDriveType :: Lens' ActivateGateway (Maybe Text)
agrqTapeDriveType = lens _agrqTapeDriveType (\ s a -> s{_agrqTapeDriveType = a});

-- | One of the values that defines the type of gateway to activate. The type
-- specified is critical to all later functions of the gateway and cannot
-- be changed after activation. The default value is @STORED@.
agrqGatewayType :: Lens' ActivateGateway (Maybe Text)
agrqGatewayType = lens _agrqGatewayType (\ s a -> s{_agrqGatewayType = a});

-- | Your gateway activation key. You can obtain the activation key by
-- sending an HTTP GET request with redirects enabled to the gateway IP
-- address (port 80). The redirect URL returned in the response provides
-- you the activation key for your gateway in the query string parameter
-- @activationKey@. It may also include other activation-related
-- parameters, however, these are merely defaults -- the arguments you pass
-- to the @ActivateGateway@ API call determine the actual configuration of
-- your gateway.
agrqActivationKey :: Lens' ActivateGateway Text
agrqActivationKey = lens _agrqActivationKey (\ s a -> s{_agrqActivationKey = a});

-- | FIXME: Undocumented member.
agrqGatewayName :: Lens' ActivateGateway Text
agrqGatewayName = lens _agrqGatewayName (\ s a -> s{_agrqGatewayName = a});

-- | One of the values that indicates the time zone you want to set for the
-- gateway. The time zone is used, for example, for scheduling snapshots
-- and your gateway\'s maintenance schedule.
agrqGatewayTimezone :: Lens' ActivateGateway Text
agrqGatewayTimezone = lens _agrqGatewayTimezone (\ s a -> s{_agrqGatewayTimezone = a});

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
agrqGatewayRegion :: Lens' ActivateGateway Text
agrqGatewayRegion = lens _agrqGatewayRegion (\ s a -> s{_agrqGatewayRegion = a});

instance AWSRequest ActivateGateway where
        type Sv ActivateGateway = StorageGateway
        type Rs ActivateGateway = ActivateGatewayResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ActivateGatewayResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

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
              ["MediumChangerType" .= _agrqMediumChangerType,
               "TapeDriveType" .= _agrqTapeDriveType,
               "GatewayType" .= _agrqGatewayType,
               "ActivationKey" .= _agrqActivationKey,
               "GatewayName" .= _agrqGatewayName,
               "GatewayTimezone" .= _agrqGatewayTimezone,
               "GatewayRegion" .= _agrqGatewayRegion]

instance ToPath ActivateGateway where
        toPath = const "/"

instance ToQuery ActivateGateway where
        toQuery = const mempty

-- | AWS Storage Gateway returns the Amazon Resource Name (ARN) of the
-- activated gateway. It is a string made of information such as your
-- account, gateway name, and region. This ARN is used to reference the
-- gateway in other API operations as well as resource-based authorization.
--
-- /See:/ 'activateGatewayResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'agrsGatewayARN'
--
-- * 'agrsStatus'
data ActivateGatewayResponse = ActivateGatewayResponse'
    { _agrsGatewayARN :: !(Maybe Text)
    , _agrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActivateGatewayResponse' smart constructor.
activateGatewayResponse :: Int -> ActivateGatewayResponse
activateGatewayResponse pStatus =
    ActivateGatewayResponse'
    { _agrsGatewayARN = Nothing
    , _agrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
agrsGatewayARN :: Lens' ActivateGatewayResponse (Maybe Text)
agrsGatewayARN = lens _agrsGatewayARN (\ s a -> s{_agrsGatewayARN = a});

-- | FIXME: Undocumented member.
agrsStatus :: Lens' ActivateGatewayResponse Int
agrsStatus = lens _agrsStatus (\ s a -> s{_agrsStatus = a});
