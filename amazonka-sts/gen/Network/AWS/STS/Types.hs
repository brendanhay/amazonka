{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AWS Security Token Service (STS) is a web service that enables you to
-- request temporary, limited-privilege credentials for AWS Identity and
-- Access Management (IAM) users or for users that you authenticate (federated
-- users).
module Network.AWS.STS.Types
    (
    -- * Service
      STS
    -- ** Errors
    , STSError (..)
    , _ExpiredTokenException
    , _IDPCommunicationErrorException
    , _IDPRejectedClaimException
    , _InvalidAuthorizationMessageException
    , _InvalidIdentityTokenException
    , _MalformedPolicyDocumentException
    , _PackedPolicyTooLargeException
    , _STSClient
    , _STSSerializer
    , _STSService
    -- ** XML
    , xmlOptions

    -- * AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruAssumedRoleId
    , aruArn

    -- * Credentials
    , Credentials
    , credentials
    , cAccessKeyId
    , cSecretAccessKey
    , cSessionToken
    , cExpiration

    -- * FederatedUser
    , FederatedUser
    , federatedUser
    , fuFederatedUserId
    , fuArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2011-06-15@) of the
-- @AWS Security Token Service@ service.
data STS deriving (Typeable)

instance AWSService STS where
    type Sg STS = V4
    type Er STS = STSError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "sts"
        , _svcVersion  = "2011-06-15"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'STS' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data STSError
      -- | The web identity token that was passed is expired or is not
      -- valid. Get a new identity token from the identity provider and
      -- then retry the request.
    = ExpiredTokenException
        { _eteMessage :: Maybe Text
        }
      -- | The request could not be fulfilled because the non-AWS identity
      -- provider (IDP) that was asked to verify the incoming identity
      -- token could not be reached. This is often a transient error
      -- caused by network conditions. Retry the request a limited number
      -- of times so that you don't exceed the request rate. If the error
      -- persists, the non-AWS identity provider might be down or not
      -- responding.
    | IDPCommunicationErrorException
        { _idpceeMessage :: Maybe Text
        }
      -- | The identity provider (IdP) reported that authentication failed.
      -- This might be because the claim is invalid. If this error is
      -- returned for the AssumeRoleWithWebIdentity operation, it can also
      -- mean that the claim has expired or has been explicitly revoked.
    | IDPRejectedClaimException
        { _idprceMessage :: Maybe Text
        }
      -- | The error returned if the message passed to
      -- DecodeAuthorizationMessage was invalid. This can happen if the
      -- token contains invalid characters, such as linebreaks.
    | InvalidAuthorizationMessageException
        { _iameMessage :: Maybe Text
        }
      -- | The web identity token that was passed could not be validated by
      -- AWS. Get a new identity token from the identity provider and then
      -- retry the request.
    | InvalidIdentityTokenException
        { _iiteMessage :: Maybe Text
        }
      -- | The request was rejected because the policy document was
      -- malformed. The error message describes the specific error.
    | MalformedPolicyDocumentException
        { _mpdeMessage :: Maybe Text
        }
      -- | The request was rejected because the policy document was too
      -- large. The error message describes how big the policy document
      -- is, in packed form, as a percentage of what the API allows.
    | PackedPolicyTooLargeException
        { _pptleMessage :: Maybe Text
        }
    | STSClient HttpException
    | STSSerializer String
    | STSService String
      deriving (Show, Typeable, Generic)

instance AWSError STSError where
    awsError = const "STSError"

instance AWSServiceError STSError where
    serviceError    = STSService
    clientError     = STSClient
    serializerError = STSSerializer

instance Exception STSError

-- | The web identity token that was passed is expired or is not valid. Get a
-- new identity token from the identity provider and then retry the request.
--
-- See: 'ExpiredTokenException'
_ExpiredTokenException :: Prism' STSError (Maybe Text)
_ExpiredTokenException = prism
    ExpiredTokenException
    (\case
        ExpiredTokenException p1 -> Right p1
        x -> Left x)

-- | The request could not be fulfilled because the non-AWS identity provider
-- (IDP) that was asked to verify the incoming identity token could not be
-- reached. This is often a transient error caused by network conditions.
-- Retry the request a limited number of times so that you don't exceed the
-- request rate. If the error persists, the non-AWS identity provider might be
-- down or not responding.
--
-- See: 'IDPCommunicationErrorException'
_IDPCommunicationErrorException :: Prism' STSError (Maybe Text)
_IDPCommunicationErrorException = prism
    IDPCommunicationErrorException
    (\case
        IDPCommunicationErrorException p1 -> Right p1
        x -> Left x)

-- | The identity provider (IdP) reported that authentication failed. This might
-- be because the claim is invalid. If this error is returned for the
-- AssumeRoleWithWebIdentity operation, it can also mean that the claim has
-- expired or has been explicitly revoked.
--
-- See: 'IDPRejectedClaimException'
_IDPRejectedClaimException :: Prism' STSError (Maybe Text)
_IDPRejectedClaimException = prism
    IDPRejectedClaimException
    (\case
        IDPRejectedClaimException p1 -> Right p1
        x -> Left x)

-- | The error returned if the message passed to DecodeAuthorizationMessage was
-- invalid. This can happen if the token contains invalid characters, such as
-- linebreaks.
--
-- See: 'InvalidAuthorizationMessageException'
_InvalidAuthorizationMessageException :: Prism' STSError (Maybe Text)
_InvalidAuthorizationMessageException = prism
    InvalidAuthorizationMessageException
    (\case
        InvalidAuthorizationMessageException p1 -> Right p1
        x -> Left x)

-- | The web identity token that was passed could not be validated by AWS. Get a
-- new identity token from the identity provider and then retry the request.
--
-- See: 'InvalidIdentityTokenException'
_InvalidIdentityTokenException :: Prism' STSError (Maybe Text)
_InvalidIdentityTokenException = prism
    InvalidIdentityTokenException
    (\case
        InvalidIdentityTokenException p1 -> Right p1
        x -> Left x)

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
--
-- See: 'MalformedPolicyDocumentException'
_MalformedPolicyDocumentException :: Prism' STSError (Maybe Text)
_MalformedPolicyDocumentException = prism
    MalformedPolicyDocumentException
    (\case
        MalformedPolicyDocumentException p1 -> Right p1
        x -> Left x)

-- | The request was rejected because the policy document was too large. The
-- error message describes how big the policy document is, in packed form, as
-- a percentage of what the API allows.
--
-- See: 'PackedPolicyTooLargeException'
_PackedPolicyTooLargeException :: Prism' STSError (Maybe Text)
_PackedPolicyTooLargeException = prism
    PackedPolicyTooLargeException
    (\case
        PackedPolicyTooLargeException p1 -> Right p1
        x -> Left x)

-- | See: 'STSClient'
_STSClient :: Prism' STSError HttpException
_STSClient = prism
    STSClient
    (\case
        STSClient p1 -> Right p1
        x -> Left x)

-- | See: 'STSSerializer'
_STSSerializer :: Prism' STSError String
_STSSerializer = prism
    STSSerializer
    (\case
        STSSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'STSService'
_STSService :: Prism' STSError String
_STSService = prism
    STSService
    (\case
        STSService p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary security
-- credentials. For example, you can reference these credentials as a
-- principal in a resource-based policy by using the ARN or assumed role ID.
-- The ARN and ID include the RoleSessionName that you specified when you
-- called AssumeRole.
data AssumedRoleUser = AssumedRoleUser
    { _aruAssumedRoleId :: Text
    , _aruArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AssumedRoleUser' data type.
--
-- 'AssumedRoleUser' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AssumedRoleId ::@ @Text@
--
-- * @Arn ::@ @Text@
--
assumedRoleUser :: Text -- ^ 'aruAssumedRoleId'
                  -> Text -- ^ 'aruArn'
                  -> AssumedRoleUser
assumedRoleUser p1 p2 = AssumedRoleUser
    { _aruAssumedRoleId = p1
    , _aruArn = p2
    }

-- | A unique identifier that contains the role ID and the role session name of
-- the role that is being assumed. The role ID is generated by AWS when the
-- role is created.
aruAssumedRoleId :: Lens' AssumedRoleUser Text
aruAssumedRoleId =
    lens _aruAssumedRoleId (\s a -> s { _aruAssumedRoleId = a })

-- | The ARN of the temporary security credentials that are returned from the
-- AssumeRole action. For more information about ARNs and how to use them in
-- policies, see Identifiers for IAM Entities in Using IAM.
aruArn :: Lens' AssumedRoleUser Text
aruArn = lens _aruArn (\s a -> s { _aruArn = a })

instance FromXML AssumedRoleUser where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssumedRoleUser"

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
data Credentials = Credentials
    { _cAccessKeyId :: Text
    , _cSecretAccessKey :: Text
    , _cSessionToken :: Text
    , _cExpiration :: ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Credentials' data type.
--
-- 'Credentials' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccessKeyId ::@ @Text@
--
-- * @SecretAccessKey ::@ @Text@
--
-- * @SessionToken ::@ @Text@
--
-- * @Expiration ::@ @ISO8601@
--
credentials :: Text -- ^ 'cAccessKeyId'
              -> Text -- ^ 'cSecretAccessKey'
              -> Text -- ^ 'cSessionToken'
              -> ISO8601 -- ^ 'cExpiration'
              -> Credentials
credentials p1 p2 p3 p4 = Credentials
    { _cAccessKeyId = p1
    , _cSecretAccessKey = p2
    , _cSessionToken = p3
    , _cExpiration = p4
    }

-- | The access key ID that identifies the temporary security credentials.
cAccessKeyId :: Lens' Credentials Text
cAccessKeyId = lens _cAccessKeyId (\s a -> s { _cAccessKeyId = a })

-- | The secret access key that can be used to sign requests.
cSecretAccessKey :: Lens' Credentials Text
cSecretAccessKey =
    lens _cSecretAccessKey (\s a -> s { _cSecretAccessKey = a })

-- | The token that users must pass to the service API to use the temporary
-- credentials.
cSessionToken :: Lens' Credentials Text
cSessionToken = lens _cSessionToken (\s a -> s { _cSessionToken = a })

-- | The date on which the current credentials expire.
cExpiration :: Lens' Credentials ISO8601
cExpiration = lens _cExpiration (\s a -> s { _cExpiration = a })

instance FromXML Credentials where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Credentials"

-- | Identifiers for the federated user associated with the credentials (such as
-- arn:aws:sts::123456789012:federated-user/Bob or 123456789012:Bob). You can
-- use the federated user's ARN in your resource-based policies, such as an
-- Amazon S3 bucket policy.
data FederatedUser = FederatedUser
    { _fuFederatedUserId :: Text
    , _fuArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'FederatedUser' data type.
--
-- 'FederatedUser' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @FederatedUserId ::@ @Text@
--
-- * @Arn ::@ @Text@
--
federatedUser :: Text -- ^ 'fuFederatedUserId'
                -> Text -- ^ 'fuArn'
                -> FederatedUser
federatedUser p1 p2 = FederatedUser
    { _fuFederatedUserId = p1
    , _fuArn = p2
    }

-- | The string that identifies the federated user associated with the
-- credentials, similar to the unique ID of an IAM user.
fuFederatedUserId :: Lens' FederatedUser Text
fuFederatedUserId =
    lens _fuFederatedUserId (\s a -> s { _fuFederatedUserId = a })

-- | The ARN that specifies the federated user that is associated with the
-- credentials. For more information about ARNs and how to use them in
-- policies, see Identifiers for IAM Entities in Using IAM.
fuArn :: Lens' FederatedUser Text
fuArn = lens _fuArn (\s a -> s { _fuArn = a })

instance FromXML FederatedUser where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "FederatedUser"
