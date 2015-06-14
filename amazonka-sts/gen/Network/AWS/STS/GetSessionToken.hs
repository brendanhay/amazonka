{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.STS.GetSessionToken
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

-- | Returns a set of temporary credentials for an AWS account or IAM user.
-- The credentials consist of an access key ID, a secret access key, and a
-- security token. Typically, you use @GetSessionToken@ if you want to use
-- MFA to protect programmatic calls to specific AWS APIs like Amazon EC2
-- @StopInstances@. MFA-enabled IAM users would need to call
-- @GetSessionToken@ and submit an MFA code that is associated with their
-- MFA device. Using the temporary security credentials that are returned
-- from the call, IAM users can then make programmatic calls to APIs that
-- require MFA authentication.
--
-- The @GetSessionToken@ action must be called by using the long-term AWS
-- security credentials of the AWS account or an IAM user. Credentials that
-- are created by IAM users are valid for the duration that you specify,
-- between 900 seconds (15 minutes) and 129600 seconds (36 hours);
-- credentials that are created by using account credentials have a maximum
-- duration of 3600 seconds (1 hour).
--
-- We recommend that you do not call @GetSessionToken@ with root account
-- credentials. Instead, follow our
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html#create-iam-users best practices>
-- by creating one or more IAM users, giving them the necessary
-- permissions, and using IAM users for everyday interaction with AWS.
--
-- The permissions associated with the temporary security credentials
-- returned by @GetSessionToken@ are based on the permissions associated
-- with account or IAM user whose credentials are used to call the action.
-- If @GetSessionToken@ is called using root account credentials, the
-- temporary credentials have root account permissions. Similarly, if
-- @GetSessionToken@ is called using the credentials of an IAM user, the
-- temporary credentials have the same permissions as the IAM user.
--
-- For more information about using @GetSessionToken@ to create temporary
-- credentials, go to
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/CreatingSessionTokens.html Creating Temporary Credentials to Enable Access for IAM Users>
-- in /Using Temporary Security Credentials/.
--
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_GetSessionToken.html>
module Network.AWS.STS.GetSessionToken
    (
    -- * Request
      GetSessionToken
    -- ** Request constructor
    , getSessionToken
    -- ** Request lenses
    , gstTokenCode
    , gstDurationSeconds
    , gstSerialNumber

    -- * Response
    , GetSessionTokenResponse
    -- ** Response constructor
    , getSessionTokenResponse
    -- ** Response lenses
    , gstrCredentials
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.STS.Types

-- | /See:/ 'getSessionToken' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gstTokenCode'
--
-- * 'gstDurationSeconds'
--
-- * 'gstSerialNumber'
data GetSessionToken = GetSessionToken'{_gstTokenCode :: Maybe Text, _gstDurationSeconds :: Maybe Nat, _gstSerialNumber :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetSessionToken' smart constructor.
getSessionToken :: GetSessionToken
getSessionToken = GetSessionToken'{_gstTokenCode = Nothing, _gstDurationSeconds = Nothing, _gstSerialNumber = Nothing};

-- | The value provided by the MFA device, if MFA is required. If any policy
-- requires the IAM user to submit an MFA code, specify this value. If MFA
-- authentication is required, and the user does not provide a code when
-- requesting a set of temporary security credentials, the user will
-- receive an \"access denied\" response when requesting resources that
-- require MFA authentication.
gstTokenCode :: Lens' GetSessionToken (Maybe Text)
gstTokenCode = lens _gstTokenCode (\ s a -> s{_gstTokenCode = a});

-- | The duration, in seconds, that the credentials should remain valid.
-- Acceptable durations for IAM user sessions range from 900 seconds (15
-- minutes) to 129600 seconds (36 hours), with 43200 seconds (12 hours) as
-- the default. Sessions for AWS account owners are restricted to a maximum
-- of 3600 seconds (one hour). If the duration is longer than one hour, the
-- session for AWS account owners defaults to one hour.
gstDurationSeconds :: Lens' GetSessionToken (Maybe Natural)
gstDurationSeconds = lens _gstDurationSeconds (\ s a -> s{_gstDurationSeconds = a}) . mapping _Nat;

-- | The identification number of the MFA device that is associated with the
-- IAM user who is making the @GetSessionToken@ call. Specify this value if
-- the IAM user has a policy that requires MFA authentication. The value is
-- either the serial number for a hardware device (such as @GAHT12345678@)
-- or an Amazon Resource Name (ARN) for a virtual device (such as
-- @arn:aws:iam::123456789012:mfa\/user@). You can find the device for an
-- IAM user by going to the AWS Management Console and viewing the user\'s
-- security credentials.
gstSerialNumber :: Lens' GetSessionToken (Maybe Text)
gstSerialNumber = lens _gstSerialNumber (\ s a -> s{_gstSerialNumber = a});

instance AWSRequest GetSessionToken where
        type Sv GetSessionToken = STS
        type Rs GetSessionToken = GetSessionTokenResponse
        request = post
        response
          = receiveXMLWrapper "GetSessionTokenResult"
              (\ s h x ->
                 GetSessionTokenResponse' <$> x .@? "Credentials")

instance ToHeaders GetSessionToken where
        toHeaders = const mempty

instance ToPath GetSessionToken where
        toPath = const "/"

instance ToQuery GetSessionToken where
        toQuery GetSessionToken'{..}
          = mconcat
              ["Action" =: ("GetSessionToken" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "TokenCode" =: _gstTokenCode,
               "DurationSeconds" =: _gstDurationSeconds,
               "SerialNumber" =: _gstSerialNumber]

-- | /See:/ 'getSessionTokenResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gstrCredentials'
newtype GetSessionTokenResponse = GetSessionTokenResponse'{_gstrCredentials :: Maybe Credentials} deriving (Eq, Read, Show)

-- | 'GetSessionTokenResponse' smart constructor.
getSessionTokenResponse :: GetSessionTokenResponse
getSessionTokenResponse = GetSessionTokenResponse'{_gstrCredentials = Nothing};

-- | The session credentials for API authentication.
gstrCredentials :: Lens' GetSessionTokenResponse (Maybe Credentials)
gstrCredentials = lens _gstrCredentials (\ s a -> s{_gstrCredentials = a});
