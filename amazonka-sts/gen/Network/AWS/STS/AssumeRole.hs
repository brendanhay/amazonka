{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRole
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials (consisting of an access
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
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html IAM Roles (Delegation and Federation)>
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
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity>
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
-- @\"Condition\": {\"Bool\": {\"aws:MultiFactorAuthPresent\": true}}@
--
-- For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/MFAProtectedAPI.html Configuring MFA-Protected API Access>
-- in /Using IAM/ guide.
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
    , arrsPackedPolicySize
    , arrsCredentials
    , arrsAssumedRoleUser
    , arrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssumeRole' smart constructor.
assumeRole :: Text -> Text -> AssumeRole
assumeRole pRoleARN_ pRoleSessionName_ =
    AssumeRole'
    { _arTokenCode = Nothing
    , _arDurationSeconds = Nothing
    , _arExternalId = Nothing
    , _arPolicy = Nothing
    , _arSerialNumber = Nothing
    , _arRoleARN = pRoleARN_
    , _arRoleSessionName = pRoleSessionName_
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

-- | A unique identifier that is used by third parties when assuming roles in
-- their customers\' accounts. For each role that the third party can
-- assume, they should instruct their customers to ensure the role\'s trust
-- policy checks for the external ID that the third party generated. Each
-- time the third party assumes the role, they should pass the customer\'s
-- external ID. The external ID is useful in order to help third parties
-- bind a role to the customer who created it. For more information about
-- the external ID, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/sts-delegating-externalid.html How to Use External ID When Granting Access to Your AWS Resources>
-- in /Using Temporary Security Credentials/.
arExternalId :: Lens' AssumeRole (Maybe Text)
arExternalId = lens _arExternalId (\ s a -> s{_arExternalId = a});

-- | An IAM policy in JSON format.
--
-- This parameter is optional. If you pass a policy, the temporary security
-- credentials that are returned by the operation have the permissions that
-- are allowed by both (the intersection of) the access policy of the role
-- that is being assumed, /and/ the policy that you pass. This gives you a
-- way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant
-- permissions that are in excess of those allowed by the access policy of
-- the role that is being assumed. For more information, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity>
-- in /Using Temporary Security Credentials/.
--
-- The policy plain text must be 2048 bytes or shorter. However, an
-- internal conversion compresses it into a packed binary format with a
-- separate limit. The PackedPolicySize response element indicates by
-- percentage how close to the upper size limit the policy is, with 100%
-- equaling the maximum allowed size.
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

-- | The Amazon Resource Name (ARN) of the role to assume.
arRoleARN :: Lens' AssumeRole Text
arRoleARN = lens _arRoleARN (\ s a -> s{_arRoleARN = a});

-- | An identifier for the assumed role session.
--
-- Use the role session name to uniquely identity a session when the same
-- role is assumed by different principals or for different reasons. In
-- cross-account scenarios, the role session name is visible to, and can be
-- logged by the account that owns the role. The role session name is also
-- used in the ARN of the assumed role principal. This means that
-- subsequent cross-account API requests using the temporary security
-- credentials will expose the role session name to the external account in
-- their CloudTrail logs.
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
                     <*> (pure (fromEnum s)))

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
-- * 'arrsPackedPolicySize'
--
-- * 'arrsCredentials'
--
-- * 'arrsAssumedRoleUser'
--
-- * 'arrsStatus'
data AssumeRoleResponse = AssumeRoleResponse'
    { _arrsPackedPolicySize :: !(Maybe Nat)
    , _arrsCredentials      :: !(Maybe Credentials)
    , _arrsAssumedRoleUser  :: !(Maybe AssumedRoleUser)
    , _arrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssumeRoleResponse' smart constructor.
assumeRoleResponse :: Int -> AssumeRoleResponse
assumeRoleResponse pStatus_ =
    AssumeRoleResponse'
    { _arrsPackedPolicySize = Nothing
    , _arrsCredentials = Nothing
    , _arrsAssumedRoleUser = Nothing
    , _arrsStatus = pStatus_
    }

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arrsPackedPolicySize :: Lens' AssumeRoleResponse (Maybe Natural)
arrsPackedPolicySize = lens _arrsPackedPolicySize (\ s a -> s{_arrsPackedPolicySize = a}) . mapping _Nat;

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
arrsCredentials :: Lens' AssumeRoleResponse (Maybe Credentials)
arrsCredentials = lens _arrsCredentials (\ s a -> s{_arrsCredentials = a});

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
arrsAssumedRoleUser :: Lens' AssumeRoleResponse (Maybe AssumedRoleUser)
arrsAssumedRoleUser = lens _arrsAssumedRoleUser (\ s a -> s{_arrsAssumedRoleUser = a});

-- | FIXME: Undocumented member.
arrsStatus :: Lens' AssumeRoleResponse Int
arrsStatus = lens _arrsStatus (\ s a -> s{_arrsStatus = a});
