{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.GetSessionToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary credentials for an AWS account or IAM user. The
-- credentials consist of an access key ID, a secret access key, and a
-- security token. Typically, you use GetSessionToken if you want to use MFA
-- to protect programmatic calls to specific AWS APIs like Amazon EC2
-- StopInstances. MFA-enabled IAM users would need to call GetSessionToken and
-- submit an MFA code that is associated with their MFA device. Using the
-- temporary security credentials that are returned from the call, IAM users
-- can then make programmatic calls to APIs that require MFA authentication.
-- The GetSessionToken action must be called by using the long-term AWS
-- security credentials of the AWS account or an IAM user. Credentials that
-- are created by IAM users are valid for the duration that you specify,
-- between 900 seconds (15 minutes) and 129600 seconds (36 hours); credentials
-- that are created by using account credentials have a maximum duration of
-- 3600 seconds (1 hour). We recommend that you do not call GetSessionToken
-- with root account credentials. Instead, follow our best practices by
-- creating one or more IAM users, giving them the necessary permissions, and
-- using IAM users for everyday interaction with AWS. The permissions
-- associated with the temporary security credentials returned by
-- GetSessionToken are based on the permissions associated with account or IAM
-- user whose credentials are used to call the action. If GetSessionToken is
-- called using root account credentials, the temporary credentials have root
-- account permissions. Similarly, if GetSessionToken is called using the
-- credentials of an IAM user, the temporary credentials have the same
-- permissions as the IAM user. For more information about using
-- GetSessionToken to create temporary credentials, go to Creating Temporary
-- Credentials to Enable Access for IAM Users in Using Temporary Security
-- Credentials. https://sts.amazonaws.com/ ?Version=2011-06-15
-- &Action=GetSessionToken &DurationSeconds=3600
-- &SerialNumber=YourMFADeviceSerialNumber &TokenCode=123456 &AUTHPARAMS
-- AQoEXAMPLEH4aoAH0gNCAPyJxz4BlCFFxWNE1OPTgk5TthT+FvwqnKwRcOIfrRh3c/L
-- To6UDdyJwOOvEVPvLXCrrrUtdnniCEXAMPLE/IvU1dYUg2RVAJBanLiHb4IgRmpRV3z
-- rkuWJOgQs8IZZaIv2BXIa2R4OlgkBN9bkUDNCJiBeb/AXlzBBko7b15fjrBs2+cTQtp
-- Z3CYWFXG8C5zqx37wnOE49mRl/+OtkIKGO7fAE
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-11T19:55:29.611Z
-- AKIAIOSFODNN7EXAMPLE 58c5dbae-abef-11e0-8cfe-09039844ac7d.
module Network.AWS.STS.GetSessionToken
    (
    -- * Request
      GetSessionToken
    -- ** Request constructor
    , getSessionToken
    -- ** Request lenses
    , gstDurationSeconds
    , gstSerialNumber
    , gstTokenCode

    -- * Response
    , GetSessionTokenResponse
    -- ** Response constructor
    , getSessionTokenResponse
    -- ** Response lenses
    , gstrCredentials
    ) where

import Network.AWS.Request.Query
import Network.AWS.STS.Types
import Network.AWS.Prelude

data GetSessionToken = GetSessionToken
    { _gstDurationSeconds :: Maybe Integer
    , _gstSerialNumber :: Maybe Text
    , _gstTokenCode :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSessionToken' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DurationSeconds ::@ @Maybe Integer@
--
-- * @SerialNumber ::@ @Maybe Text@
--
-- * @TokenCode ::@ @Maybe Text@
--
getSessionToken :: GetSessionToken
getSessionToken = GetSessionToken
    { _gstDurationSeconds = Nothing
    , _gstSerialNumber = Nothing
    , _gstTokenCode = Nothing
    }

-- | The duration, in seconds, that the credentials should remain valid.
-- Acceptable durations for IAM user sessions range from 900 seconds (15
-- minutes) to 129600 seconds (36 hours), with 43200 seconds (12 hours) as the
-- default. Sessions for AWS account owners are restricted to a maximum of
-- 3600 seconds (one hour). If the duration is longer than one hour, the
-- session for AWS account owners defaults to one hour.
gstDurationSeconds :: Lens' GetSessionToken (Maybe Integer)
gstDurationSeconds =
    lens _gstDurationSeconds (\s a -> s { _gstDurationSeconds = a })

-- | The identification number of the MFA device that is associated with the IAM
-- user who is making the GetSessionToken call. Specify this value if the IAM
-- user has a policy that requires MFA authentication. The value is either the
-- serial number for a hardware device (such as GAHT12345678) or an Amazon
-- Resource Name (ARN) for a virtual device (such as
-- arn:aws:iam::123456789012:mfa/user). You can find the device for an IAM
-- user by going to the AWS Management Console and viewing the user's security
-- credentials.
gstSerialNumber :: Lens' GetSessionToken (Maybe Text)
gstSerialNumber = lens _gstSerialNumber (\s a -> s { _gstSerialNumber = a })

-- | The value provided by the MFA device, if MFA is required. If any policy
-- requires the IAM user to submit an MFA code, specify this value. If MFA
-- authentication is required, and the user does not provide a code when
-- requesting a set of temporary security credentials, the user will receive
-- an "access denied" response when requesting resources that require MFA
-- authentication.
gstTokenCode :: Lens' GetSessionToken (Maybe Text)
gstTokenCode = lens _gstTokenCode (\s a -> s { _gstTokenCode = a })

instance ToQuery GetSessionToken where
    toQuery = genericQuery def

-- | Contains the result of a successful call to the GetSessionToken action,
-- including temporary AWS credentials that can be used to make AWS requests.
newtype GetSessionTokenResponse = GetSessionTokenResponse
    { _gstrCredentials :: Maybe Credentials
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSessionTokenResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Credentials ::@ @Maybe Credentials@
--
getSessionTokenResponse :: GetSessionTokenResponse
getSessionTokenResponse = GetSessionTokenResponse
    { _gstrCredentials = Nothing
    }

-- | The session credentials for API authentication.
gstrCredentials :: Lens' GetSessionTokenResponse (Maybe Credentials)
gstrCredentials = lens _gstrCredentials (\s a -> s { _gstrCredentials = a })

instance FromXML GetSessionTokenResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSessionToken where
    type Sv GetSessionToken = STS
    type Rs GetSessionToken = GetSessionTokenResponse

    request = post "GetSessionToken"
    response _ = xmlResponse
