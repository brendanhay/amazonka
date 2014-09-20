{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Cognito is a web service that facilitates the delivery of scoped,
-- temporary credentials to mobile devices or other untrusted environments.
-- Amazon Cognito uniquely identifies a device or user and supplies the user
-- with a consistent identity throughout the lifetime of an application.
-- Amazon Cognito lets users authenticate with third-party identity providers
-- (Facebook, Google, or Login with Amazon). As a developer, you decide which
-- identity providers to trust. You can also choose to support unauthenticated
-- access from your application. Your users are provided with Cognito tokens
-- that uniquely identify their device and any information provided about
-- third-party logins.
module Network.AWS.CognitoIdentity.Types
    (
    -- * Service
      CognitoIdentity
    -- ** Errors
    , CognitoIdentityError (..)
    , _CognitoIdentityClient
    , _CognitoIdentitySerializer
    , _CognitoIdentityService
    , _InternalErrorException
    , _InvalidParameterException
    , _LimitExceededException
    , _NotAuthorizedException
    , _ResourceConflictException
    , _ResourceNotFoundException
    , _TooManyRequestsException

    -- * IdentityDescription
    , IdentityDescription
    , identityDescription
    , idIdentityId
    , idLogins

    -- * IdentityPoolShortDescription
    , IdentityPoolShortDescription
    , identityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-06-30@) of the
-- @Amazon Cognito Identity@ service.
data CognitoIdentity deriving (Typeable)

instance AWSService CognitoIdentity where
    type Sg CognitoIdentity = V4
    type Er CognitoIdentity = CognitoIdentityError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "cognito-identity"
        , _svcVersion  = "2014-06-30"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'CognitoIdentity' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data CognitoIdentityError
    = CognitoIdentityClient HttpException
    | CognitoIdentitySerializer String
    | CognitoIdentityService String
      -- | Thrown when the service encounters an error during processing the
      -- request.
    | InternalErrorException
        { _ieeMessage :: Maybe Text
        }
      -- | Thrown for missing or bad input parameter(s).
    | InvalidParameterException
        { _ipeMessage :: Maybe Text
        }
      -- | Thrown when the total number of user pools has exceeded a preset
      -- limit.
    | LimitExceededException
        { _leeMessage :: Maybe Text
        }
      -- | Thrown when a user is not authorized to access the requested
      -- resource.
    | NotAuthorizedException
        { _naeMessage :: Maybe Text
        }
      -- | Thrown when a user tries to use a login which is already linked
      -- to another account.
    | ResourceConflictException
        { _rceMessage :: Maybe Text
        }
      -- | Thrown when the requested resource (for example, a dataset or
      -- record) does not exist.
    | ResourceNotFoundException
        { _rnfeMessage :: Maybe Text
        }
      -- | Thrown when a request is throttled.
    | TooManyRequestsException
        { _tmreMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError CognitoIdentityError where
    awsError = const "CognitoIdentityError"

instance AWSServiceError CognitoIdentityError where
    serviceError    = CognitoIdentityService
    clientError     = CognitoIdentityClient
    serializerError = CognitoIdentitySerializer

instance Exception CognitoIdentityError

-- | See: 'CognitoIdentityClient'
_CognitoIdentityClient :: Prism' CognitoIdentityError HttpException
_CognitoIdentityClient = prism
    CognitoIdentityClient
    (\case
        CognitoIdentityClient p1 -> Right p1
        x -> Left x)

-- | See: 'CognitoIdentitySerializer'
_CognitoIdentitySerializer :: Prism' CognitoIdentityError String
_CognitoIdentitySerializer = prism
    CognitoIdentitySerializer
    (\case
        CognitoIdentitySerializer p1 -> Right p1
        x -> Left x)

-- | See: 'CognitoIdentityService'
_CognitoIdentityService :: Prism' CognitoIdentityError String
_CognitoIdentityService = prism
    CognitoIdentityService
    (\case
        CognitoIdentityService p1 -> Right p1
        x -> Left x)

-- | Thrown when the service encounters an error during processing the request.
--
-- See: 'InternalErrorException'
_InternalErrorException :: Prism' CognitoIdentityError (Maybe Text)
_InternalErrorException = prism
    InternalErrorException
    (\case
        InternalErrorException p1 -> Right p1
        x -> Left x)

-- | Thrown for missing or bad input parameter(s).
--
-- See: 'InvalidParameterException'
_InvalidParameterException :: Prism' CognitoIdentityError (Maybe Text)
_InvalidParameterException = prism
    InvalidParameterException
    (\case
        InvalidParameterException p1 -> Right p1
        x -> Left x)

-- | Thrown when the total number of user pools has exceeded a preset limit.
--
-- See: 'LimitExceededException'
_LimitExceededException :: Prism' CognitoIdentityError (Maybe Text)
_LimitExceededException = prism
    LimitExceededException
    (\case
        LimitExceededException p1 -> Right p1
        x -> Left x)

-- | Thrown when a user is not authorized to access the requested resource.
--
-- See: 'NotAuthorizedException'
_NotAuthorizedException :: Prism' CognitoIdentityError (Maybe Text)
_NotAuthorizedException = prism
    NotAuthorizedException
    (\case
        NotAuthorizedException p1 -> Right p1
        x -> Left x)

-- | Thrown when a user tries to use a login which is already linked to another
-- account.
--
-- See: 'ResourceConflictException'
_ResourceConflictException :: Prism' CognitoIdentityError (Maybe Text)
_ResourceConflictException = prism
    ResourceConflictException
    (\case
        ResourceConflictException p1 -> Right p1
        x -> Left x)

-- | Thrown when the requested resource (for example, a dataset or record) does
-- not exist.
--
-- See: 'ResourceNotFoundException'
_ResourceNotFoundException :: Prism' CognitoIdentityError (Maybe Text)
_ResourceNotFoundException = prism
    ResourceNotFoundException
    (\case
        ResourceNotFoundException p1 -> Right p1
        x -> Left x)

-- | Thrown when a request is throttled.
--
-- See: 'TooManyRequestsException'
_TooManyRequestsException :: Prism' CognitoIdentityError (Maybe Text)
_TooManyRequestsException = prism
    TooManyRequestsException
    (\case
        TooManyRequestsException p1 -> Right p1
        x -> Left x)

-- | A description of the identity.
data IdentityDescription = IdentityDescription
    { _idIdentityId :: Maybe Text
    , _idLogins :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IdentityDescription' data type.
--
-- 'IdentityDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityId ::@ @Maybe Text@
--
-- * @Logins ::@ @[Text]@
--
identityDescription :: IdentityDescription
identityDescription = IdentityDescription
    { _idIdentityId = Nothing
    , _idLogins = mempty
    }

-- | A unique identifier in the format REGION:GUID.
idIdentityId :: Lens' IdentityDescription (Maybe Text)
idIdentityId = lens _idIdentityId (\s a -> s { _idIdentityId = a })

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
idLogins :: Lens' IdentityDescription [Text]
idLogins = lens _idLogins (\s a -> s { _idLogins = a })

instance FromJSON IdentityDescription

-- | A description of the identity pool.
data IdentityPoolShortDescription = IdentityPoolShortDescription
    { _ipsdIdentityPoolId :: Maybe Text
    , _ipsdIdentityPoolName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'IdentityPoolShortDescription' data type.
--
-- 'IdentityPoolShortDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Maybe Text@
--
-- * @IdentityPoolName ::@ @Maybe Text@
--
identityPoolShortDescription :: IdentityPoolShortDescription
identityPoolShortDescription = IdentityPoolShortDescription
    { _ipsdIdentityPoolId = Nothing
    , _ipsdIdentityPoolName = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
ipsdIdentityPoolId :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolId =
    lens _ipsdIdentityPoolId (\s a -> s { _ipsdIdentityPoolId = a })

-- | A string that you provide.
ipsdIdentityPoolName :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolName =
    lens _ipsdIdentityPoolName (\s a -> s { _ipsdIdentityPoolName = a })

instance FromJSON IdentityPoolShortDescription
