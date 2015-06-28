{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.STS.AssumeRole
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

-- | Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) that you can use to
-- access AWS resources that you might not normally have access to.
-- Typically, you use @AssumeRole@ for cross-account access or federation.
--
-- __Important:__ You cannot call @AssumeRole@ by using AWS account
-- credentials; access will be denied. You must use IAM user credentials or
-- temporary security credentials to call @AssumeRole@.
--
-- For cross-account access, imagine that you own multiple accounts and
-- need to access resources in each account. You could create long-term
-- credentials in each account to access those resources. However, managing
-- all those credentials and remembering which one can access which account
-- can be time consuming. Instead, you can create one set of long-term
-- credentials in one account and then use temporary security credentials
-- to access all the other accounts by assuming roles in those accounts.
-- For more information about roles, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Roles>
-- in /Using IAM/.
--
-- For federation, you can, for example, grant single sign-on access to the
-- AWS Management Console. If you already have an identity and
-- authentication system in your corporate network, you don\'t have to
-- recreate user identities in AWS in order to grant those user identities
-- access to AWS. Instead, after a user has been authenticated, you call
-- @AssumeRole@ (and specify the role with the appropriate permissions) to
-- get temporary security credentials for that user. With those temporary
-- security credentials, you construct a sign-in URL that users can use to
-- access the console. For more information, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/STSUseCases.html Scenarios for Granting Temporary Access>
-- in /Using Temporary Security Credentials/.
--
-- The temporary security credentials are valid for the duration that you
-- specified when calling @AssumeRole@, which can be from 900 seconds (15
-- minutes) to 3600 seconds (1 hour). The default is 1 hour.
--
-- Optionally, you can pass an IAM access policy to this operation. If you
-- choose not to pass a policy, the temporary security credentials that are
-- returned by the operation have the permissions that are defined in the
-- access policy of the role that is being assumed. If you pass a policy to
-- this operation, the temporary security credentials that are returned by
-- the operation have the permissions that are allowed by both the access
-- policy of the role that is being assumed, /__and__/ the policy that you
-- pass. This gives you a way to further restrict the permissions for the
-- resulting temporary security credentials. You cannot use the passed
-- policy to grant permissions that are in excess of those allowed by the
-- access policy of the role that is being assumed. For more information,
-- see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRole>
-- in /Using Temporary Security Credentials/.
--
-- To assume a role, your AWS account must be trusted by the role. The
-- trust relationship is defined in the role\'s trust policy when the role
-- is created. You must also have a policy that allows you to call
-- @sts:AssumeRole@.
--
-- __Using MFA with AssumeRole__
--
-- You can optionally include multi-factor authentication (MFA) information
-- when you call @AssumeRole@. This is useful for cross-account scenarios
-- in which you want to make sure that the user who is assuming the role
-- has been authenticated using an AWS MFA device. In that scenario, the
-- trust policy of the role being assumed includes a condition that tests
-- for MFA authentication; if the caller does not include valid MFA
-- information, the request to assume the role is denied. The condition in
-- a trust policy that tests for MFA authentication might look like the
-- following example.
--
-- @\"Condition\": {\"Null\": {\"aws:MultiFactorAuthAge\": false}}@
--
-- For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/MFAProtectedAPI.html Configuring MFA-Protected API Access>
-- in the /Using IAM/ guide.
--
-- To use MFA with @AssumeRole@, you pass values for the @SerialNumber@ and
-- @TokenCode@ parameters. The @SerialNumber@ value identifies the user\'s
-- hardware or virtual MFA device. The @TokenCode@ is the time-based
-- one-time password (TOTP) that the MFA devices produces.
--
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html>
module Network.AWS.STS.AssumeRole
    (
    -- * Request
      AssumeRole
    -- ** Request constructor
    , assumeRole
    -- ** Request lenses
    , arTokenCode
    , arDurationSeconds
    , arExternalId
    , arPolicy
    , arSerialNumber
    , arRoleARN
    , arRoleSessionName

    -- * Response
    , AssumeRoleResponse
    -- ** Response constructor
    , assumeRoleResponse
    -- ** Response lenses
    , arrPackedPolicySize
    , arrCredentials
    , arrAssumedRoleUser
    , arrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.STS.Types

-- | /See:/ 'assumeRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arTokenCode'
--
-- * 'arDurationSeconds'
--
-- * 'arExternalId'
--
-- * 'arPolicy'
--
-- * 'arSerialNumber'
--
-- * 'arRoleARN'
--
-- * 'arRoleSessionName'
data AssumeRole = AssumeRole'
    { _arTokenCode       :: !(Maybe Text)
    , _arDurationSeconds :: !(Maybe Nat)
    , _arExternalId      :: !(Maybe Text)
    , _arPolicy          :: !(Maybe Text)
    , _arSerialNumber    :: !(Maybe Text)
    , _arRoleARN         :: !Text
    , _arRoleSessionName :: !Text
    } deriving (Eq,Read,Show)

-- | 'AssumeRole' smart constructor.
assumeRole :: Text -> Text -> AssumeRole
assumeRole pRoleARN pRoleSessionName =
    AssumeRole'
    { _arTokenCode = Nothing
    , _arDurationSeconds = Nothing
    , _arExternalId = Nothing
    , _arPolicy = Nothing
    , _arSerialNumber = Nothing
    , _arRoleARN = pRoleARN
    , _arRoleSessionName = pRoleSessionName
    }

-- | The value provided by the MFA device, if the trust policy of the role
-- being assumed requires MFA (that is, if the policy includes a condition
-- that tests for MFA). If the role being assumed requires MFA and if the
-- @TokenCode@ value is missing or expired, the @AssumeRole@ call returns
-- an \"access denied\" error.
arTokenCode :: Lens' AssumeRole (Maybe Text)
arTokenCode = lens _arTokenCode (\ s a -> s{_arTokenCode = a});

-- | The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) to 3600 seconds (1 hour). By default, the value
-- is set to 3600 seconds.
arDurationSeconds :: Lens' AssumeRole (Maybe Natural)
arDurationSeconds = lens _arDurationSeconds (\ s a -> s{_arDurationSeconds = a}) . mapping _Nat;

-- | A unique identifier that is used by third parties to assume a role in
-- their customers\' accounts. For each role that the third party can
-- assume, they should instruct their customers to create a role with the
-- external ID that the third party generated. Each time the third party
-- assumes the role, they must pass the customer\'s external ID. The
-- external ID is useful in order to help third parties bind a role to the
-- customer who created it. For more information about the external ID, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/sts-delegating-externalid.html About the External ID>
-- in /Using Temporary Security Credentials/.
arExternalId :: Lens' AssumeRole (Maybe Text)
arExternalId = lens _arExternalId (\ s a -> s{_arExternalId = a});

-- | An IAM policy in JSON format.
--
-- The policy parameter is optional. If you pass a policy, the temporary
-- security credentials that are returned by the operation have the
-- permissions that are allowed by both the access policy of the role that
-- is being assumed, /__and__/ the policy that you pass. This gives you a
-- way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant
-- permissions that are in excess of those allowed by the access policy of
-- the role that is being assumed. For more information, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRole>
-- in /Using Temporary Security Credentials/.
arPolicy :: Lens' AssumeRole (Maybe Text)
arPolicy = lens _arPolicy (\ s a -> s{_arPolicy = a});

-- | The identification number of the MFA device that is associated with the
-- user who is making the @AssumeRole@ call. Specify this value if the
-- trust policy of the role being assumed includes a condition that
-- requires MFA authentication. The value is either the serial number for a
-- hardware device (such as @GAHT12345678@) or an Amazon Resource Name
-- (ARN) for a virtual device (such as
-- @arn:aws:iam::123456789012:mfa\/user@).
arSerialNumber :: Lens' AssumeRole (Maybe Text)
arSerialNumber = lens _arSerialNumber (\ s a -> s{_arSerialNumber = a});

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arRoleARN :: Lens' AssumeRole Text
arRoleARN = lens _arRoleARN (\ s a -> s{_arRoleARN = a});

-- | An identifier for the assumed role session. The session name is included
-- as part of the @AssumedRoleUser@.
arRoleSessionName :: Lens' AssumeRole Text
arRoleSessionName = lens _arRoleSessionName (\ s a -> s{_arRoleSessionName = a});

instance AWSRequest AssumeRole where
        type Sv AssumeRole = STS
        type Rs AssumeRole = AssumeRoleResponse
        request = post
        response
          = receiveXMLWrapper "AssumeRoleResult"
              (\ s h x ->
                 AssumeRoleResponse' <$>
                   (x .@? "PackedPolicySize") <*> (x .@? "Credentials")
                     <*> (x .@? "AssumedRoleUser")
                     <*> (pure s))

instance ToHeaders AssumeRole where
        toHeaders = const mempty

instance ToPath AssumeRole where
        toPath = const "/"

instance ToQuery AssumeRole where
        toQuery AssumeRole'{..}
          = mconcat
              ["Action" =: ("AssumeRole" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "TokenCode" =: _arTokenCode,
               "DurationSeconds" =: _arDurationSeconds,
               "ExternalId" =: _arExternalId, "Policy" =: _arPolicy,
               "SerialNumber" =: _arSerialNumber,
               "RoleArn" =: _arRoleARN,
               "RoleSessionName" =: _arRoleSessionName]

-- | Contains the response to a successful AssumeRole request, including
-- temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'assumeRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arrPackedPolicySize'
--
-- * 'arrCredentials'
--
-- * 'arrAssumedRoleUser'
--
-- * 'arrStatus'
data AssumeRoleResponse = AssumeRoleResponse'
    { _arrPackedPolicySize :: !(Maybe Nat)
    , _arrCredentials      :: !(Maybe Credentials)
    , _arrAssumedRoleUser  :: !(Maybe AssumedRoleUser)
    , _arrStatus           :: !Status
    } deriving (Eq,Read,Show)

-- | 'AssumeRoleResponse' smart constructor.
assumeRoleResponse :: Status -> AssumeRoleResponse
assumeRoleResponse pStatus =
    AssumeRoleResponse'
    { _arrPackedPolicySize = Nothing
    , _arrCredentials = Nothing
    , _arrAssumedRoleUser = Nothing
    , _arrStatus = pStatus
    }

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arrPackedPolicySize :: Lens' AssumeRoleResponse (Maybe Natural)
arrPackedPolicySize = lens _arrPackedPolicySize (\ s a -> s{_arrPackedPolicySize = a}) . mapping _Nat;

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
arrCredentials :: Lens' AssumeRoleResponse (Maybe Credentials)
arrCredentials = lens _arrCredentials (\ s a -> s{_arrCredentials = a});

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
arrAssumedRoleUser :: Lens' AssumeRoleResponse (Maybe AssumedRoleUser)
arrAssumedRoleUser = lens _arrAssumedRoleUser (\ s a -> s{_arrAssumedRoleUser = a});

-- | FIXME: Undocumented member.
arrStatus :: Lens' AssumeRoleResponse Status
arrStatus = lens _arrStatus (\ s a -> s{_arrStatus = a});
