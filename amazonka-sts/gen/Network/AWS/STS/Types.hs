{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.STS.Types
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

module Network.AWS.STS.Types
    (
    -- * Service
      STS

    -- * Errors
    , _MalformedPolicyDocumentException
    , _PackedPolicyTooLargeException
    , _InvalidAuthorizationMessageException
    , _IDPCommunicationErrorException
    , _ExpiredTokenException
    , _InvalidIdentityTokenException
    , _IDPRejectedClaimException

    -- * AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruAssumedRoleId
    , aruARN

    -- * Credentials
    , Credentials
    , credentials
    , creAccessKeyId
    , creSecretAccessKey
    , creSessionToken
    , creExpiration

    -- * FederatedUser
    , FederatedUser
    , federatedUser
    , fuFederatedUserId
    , fuARN
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2011-06-15@ of the Amazon Security Token Service SDK.
data STS

instance AWSService STS where
    type Sg STS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "STS"
            , _svcPrefix = "sts"
            , _svcVersion = "2011-06-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: AWSError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | The request was rejected because the policy document was too large. The
-- error message describes how big the policy document is, in packed form,
-- as a percentage of what the API allows.
_PackedPolicyTooLargeException :: AWSError a => Getting (First ServiceError) a ServiceError
_PackedPolicyTooLargeException =
    _ServiceError . hasStatus 400 . hasCode "PackedPolicyTooLarge"

-- | The error returned if the message passed to @DecodeAuthorizationMessage@
-- was invalid. This can happen if the token contains invalid characters,
-- such as linebreaks.
_InvalidAuthorizationMessageException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidAuthorizationMessageException =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidAuthorizationMessageException"

-- | The request could not be fulfilled because the non-AWS identity provider
-- (IDP) that was asked to verify the incoming identity token could not be
-- reached. This is often a transient error caused by network conditions.
-- Retry the request a limited number of times so that you don\'t exceed
-- the request rate. If the error persists, the non-AWS identity provider
-- might be down or not responding.
_IDPCommunicationErrorException :: AWSError a => Getting (First ServiceError) a ServiceError
_IDPCommunicationErrorException =
    _ServiceError . hasStatus 400 . hasCode "IDPCommunicationError"

-- | The web identity token that was passed is expired or is not valid. Get a
-- new identity token from the identity provider and then retry the
-- request.
_ExpiredTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_ExpiredTokenException =
    _ServiceError . hasStatus 400 . hasCode "ExpiredTokenException"

-- | The web identity token that was passed could not be validated by AWS.
-- Get a new identity token from the identity provider and then retry the
-- request.
_InvalidIdentityTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidIdentityTokenException =
    _ServiceError . hasStatus 400 . hasCode "InvalidIdentityToken"

-- | The identity provider (IdP) reported that authentication failed. This
-- might be because the claim is invalid.
--
-- If this error is returned for the @AssumeRoleWithWebIdentity@ operation,
-- it can also mean that the claim has expired or has been explicitly
-- revoked.
_IDPRejectedClaimException :: AWSError a => Getting (First ServiceError) a ServiceError
_IDPRejectedClaimException =
    _ServiceError . hasStatus 403 . hasCode "IDPRejectedClaim"

-- | The identifiers for the temporary security credentials that the
-- operation returns.
--
-- /See:/ 'assumedRoleUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aruAssumedRoleId'
--
-- * 'aruARN'
data AssumedRoleUser = AssumedRoleUser'
    { _aruAssumedRoleId :: !Text
    , _aruARN           :: !Text
    } deriving (Eq,Read,Show)

-- | 'AssumedRoleUser' smart constructor.
assumedRoleUser :: Text -> Text -> AssumedRoleUser
assumedRoleUser pAssumedRoleId pARN =
    AssumedRoleUser'
    { _aruAssumedRoleId = pAssumedRoleId
    , _aruARN = pARN
    }

-- | A unique identifier that contains the role ID and the role session name
-- of the role that is being assumed. The role ID is generated by AWS when
-- the role is created.
aruAssumedRoleId :: Lens' AssumedRoleUser Text
aruAssumedRoleId = lens _aruAssumedRoleId (\ s a -> s{_aruAssumedRoleId = a});

-- | The ARN of the temporary security credentials that are returned from the
-- AssumeRole action. For more information about ARNs and how to use them
-- in policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Identifiers for IAM Entities>
-- in /Using IAM/.
aruARN :: Lens' AssumedRoleUser Text
aruARN = lens _aruARN (\ s a -> s{_aruARN = a});

instance FromXML AssumedRoleUser where
        parseXML x
          = AssumedRoleUser' <$>
              (x .@ "AssumedRoleId") <*> (x .@ "Arn")

-- | AWS credentials for API authentication.
--
-- /See:/ 'credentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creAccessKeyId'
--
-- * 'creSecretAccessKey'
--
-- * 'creSessionToken'
--
-- * 'creExpiration'
data Credentials = Credentials'
    { _creAccessKeyId     :: !Text
    , _creSecretAccessKey :: !Text
    , _creSessionToken    :: !Text
    , _creExpiration      :: !ISO8601
    } deriving (Eq,Read,Show)

-- | 'Credentials' smart constructor.
credentials :: Text -> Text -> Text -> UTCTime -> Credentials
credentials pAccessKeyId pSecretAccessKey pSessionToken pExpiration =
    Credentials'
    { _creAccessKeyId = pAccessKeyId
    , _creSecretAccessKey = pSecretAccessKey
    , _creSessionToken = pSessionToken
    , _creExpiration = _Time # pExpiration
    }

-- | The access key ID that identifies the temporary security credentials.
creAccessKeyId :: Lens' Credentials Text
creAccessKeyId = lens _creAccessKeyId (\ s a -> s{_creAccessKeyId = a});

-- | The secret access key that can be used to sign requests.
creSecretAccessKey :: Lens' Credentials Text
creSecretAccessKey = lens _creSecretAccessKey (\ s a -> s{_creSecretAccessKey = a});

-- | The token that users must pass to the service API to use the temporary
-- credentials.
creSessionToken :: Lens' Credentials Text
creSessionToken = lens _creSessionToken (\ s a -> s{_creSessionToken = a});

-- | The date on which the current credentials expire.
creExpiration :: Lens' Credentials UTCTime
creExpiration = lens _creExpiration (\ s a -> s{_creExpiration = a}) . _Time;

instance FromXML Credentials where
        parseXML x
          = Credentials' <$>
              (x .@ "AccessKeyId") <*> (x .@ "SecretAccessKey") <*>
                (x .@ "SessionToken")
                <*> (x .@ "Expiration")

-- | Identifiers for the federated user that is associated with the
-- credentials.
--
-- /See:/ 'federatedUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fuFederatedUserId'
--
-- * 'fuARN'
data FederatedUser = FederatedUser'
    { _fuFederatedUserId :: !Text
    , _fuARN             :: !Text
    } deriving (Eq,Read,Show)

-- | 'FederatedUser' smart constructor.
federatedUser :: Text -> Text -> FederatedUser
federatedUser pFederatedUserId pARN =
    FederatedUser'
    { _fuFederatedUserId = pFederatedUserId
    , _fuARN = pARN
    }

-- | The string that identifies the federated user associated with the
-- credentials, similar to the unique ID of an IAM user.
fuFederatedUserId :: Lens' FederatedUser Text
fuFederatedUserId = lens _fuFederatedUserId (\ s a -> s{_fuFederatedUserId = a});

-- | The ARN that specifies the federated user that is associated with the
-- credentials. For more information about ARNs and how to use them in
-- policies, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Identifiers for IAM Entities>
-- in /Using IAM/.
fuARN :: Lens' FederatedUser Text
fuARN = lens _fuARN (\ s a -> s{_fuARN = a});

instance FromXML FederatedUser where
        parseXML x
          = FederatedUser' <$>
              (x .@ "FederatedUserId") <*> (x .@ "Arn")
