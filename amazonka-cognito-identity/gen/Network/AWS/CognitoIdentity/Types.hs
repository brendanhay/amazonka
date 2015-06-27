{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.Types
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

module Network.AWS.CognitoIdentity.Types
    (
    -- * Service
      CognitoIdentity

    -- * Errors
    , _InvalidIdentityPoolConfigurationException
    , _InvalidParameterException
    , _NotAuthorizedException
    , _InternalErrorException
    , _ExternalServiceException
    , _ConcurrentModificationException
    , _TooManyRequestsException
    , _ResourceConflictException
    , _DeveloperUserAlreadyRegisteredException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * CognitoErrorCode
    , CognitoErrorCode (..)

    -- * Credentials
    , Credentials
    , credentials
    , creSessionToken
    , creExpiration
    , creSecretKey
    , creAccessKeyId

    -- * IdentityDescription
    , IdentityDescription
    , identityDescription
    , idLastModifiedDate
    , idCreationDate
    , idLogins
    , idIdentityId

    -- * IdentityPool
    , IdentityPool
    , identityPool
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities

    -- * IdentityPoolShortDescription
    , IdentityPoolShortDescription
    , identityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName

    -- * UnprocessedIdentityId
    , UnprocessedIdentityId
    , unprocessedIdentityId
    , uiiErrorCode
    , uiiIdentityId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-06-30@ of the Amazon Cognito Identity SDK.
data CognitoIdentity

instance AWSService CognitoIdentity where
    type Sg CognitoIdentity = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CognitoIdentity"
            , _svcPrefix = "cognito-identity"
            , _svcVersion = "2014-06-30"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
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

-- | Thrown if the identity pool has no role associated for the given auth
-- type (auth\/unauth) or if the AssumeRole fails.
_InvalidIdentityPoolConfigurationException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidIdentityPoolConfigurationException =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidIdentityPoolConfigurationException"

-- | Thrown for missing or bad input parameter(s).
_InvalidParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterException"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: AWSError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
    _ServiceError . hasStatus 403 . hasCode "NotAuthorizedException"

-- | Thrown when the service encounters an error during processing the
-- request.
_InternalErrorException :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _ServiceError . hasCode "InternalErrorException"

-- | An exception thrown when a dependent service such as Facebook or Twitter
-- is not responding
_ExternalServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_ExternalServiceException =
    _ServiceError . hasStatus 400 . hasCode "ExternalServiceException"

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: AWSError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasStatus 400 . hasCode "ConcurrentModificationException"

-- | Thrown when a request is throttled.
_TooManyRequestsException :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | Thrown when a user tries to use a login which is already linked to
-- another account.
_ResourceConflictException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflictException"

-- | The provided developer user identifier is already registered with
-- Cognito under a different identity ID.
_DeveloperUserAlreadyRegisteredException :: AWSError a => Getting (First ServiceError) a ServiceError
_DeveloperUserAlreadyRegisteredException =
    _ServiceError .
    hasStatus 400 . hasCode "DeveloperUserAlreadyRegisteredException"

-- | Thrown when the requested resource (for example, a dataset or record)
-- does not exist.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | Thrown when the total number of user pools has exceeded a preset limit.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededException"

data CognitoErrorCode
    = InternalServerError
    | AccessDenied
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText CognitoErrorCode where
    parser = takeLowerText >>= \case
        "AccessDenied" -> pure AccessDenied
        "InternalServerError" -> pure InternalServerError
        e -> fail ("Failure parsing CognitoErrorCode from " ++ show e)

instance ToText CognitoErrorCode where
    toText = \case
        AccessDenied -> "AccessDenied"
        InternalServerError -> "InternalServerError"

instance Hashable CognitoErrorCode
instance ToQuery CognitoErrorCode
instance ToHeader CognitoErrorCode

instance FromJSON CognitoErrorCode where
    parseJSON = parseJSONText "CognitoErrorCode"

-- | Credentials for the the provided identity ID.
--
-- /See:/ 'credentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creSessionToken'
--
-- * 'creExpiration'
--
-- * 'creSecretKey'
--
-- * 'creAccessKeyId'
data Credentials = Credentials'
    { _creSessionToken :: !(Maybe Text)
    , _creExpiration   :: !(Maybe POSIX)
    , _creSecretKey    :: !(Maybe Text)
    , _creAccessKeyId  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Credentials' smart constructor.
credentials :: Credentials
credentials =
    Credentials'
    { _creSessionToken = Nothing
    , _creExpiration = Nothing
    , _creSecretKey = Nothing
    , _creAccessKeyId = Nothing
    }

-- | The Session Token portion of the credentials
creSessionToken :: Lens' Credentials (Maybe Text)
creSessionToken = lens _creSessionToken (\ s a -> s{_creSessionToken = a});

-- | The date at which these credentials will expire.
creExpiration :: Lens' Credentials (Maybe UTCTime)
creExpiration = lens _creExpiration (\ s a -> s{_creExpiration = a}) . mapping _Time;

-- | The Secret Access Key portion of the credentials
creSecretKey :: Lens' Credentials (Maybe Text)
creSecretKey = lens _creSecretKey (\ s a -> s{_creSecretKey = a});

-- | The Access Key portion of the credentials.
creAccessKeyId :: Lens' Credentials (Maybe Text)
creAccessKeyId = lens _creAccessKeyId (\ s a -> s{_creAccessKeyId = a});

instance FromJSON Credentials where
        parseJSON
          = withObject "Credentials"
              (\ x ->
                 Credentials' <$>
                   (x .:? "SessionToken") <*> (x .:? "Expiration") <*>
                     (x .:? "SecretKey")
                     <*> (x .:? "AccessKeyId"))

-- | A description of the identity.
--
-- /See:/ 'identityDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idLastModifiedDate'
--
-- * 'idCreationDate'
--
-- * 'idLogins'
--
-- * 'idIdentityId'
data IdentityDescription = IdentityDescription'
    { _idLastModifiedDate :: !(Maybe POSIX)
    , _idCreationDate     :: !(Maybe POSIX)
    , _idLogins           :: !(Maybe [Text])
    , _idIdentityId       :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'IdentityDescription' smart constructor.
identityDescription :: IdentityDescription
identityDescription =
    IdentityDescription'
    { _idLastModifiedDate = Nothing
    , _idCreationDate = Nothing
    , _idLogins = Nothing
    , _idIdentityId = Nothing
    }

-- | Date on which the identity was last modified.
idLastModifiedDate :: Lens' IdentityDescription (Maybe UTCTime)
idLastModifiedDate = lens _idLastModifiedDate (\ s a -> s{_idLastModifiedDate = a}) . mapping _Time;

-- | Date on which the identity was created.
idCreationDate :: Lens' IdentityDescription (Maybe UTCTime)
idCreationDate = lens _idCreationDate (\ s a -> s{_idCreationDate = a}) . mapping _Time;

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
idLogins :: Lens' IdentityDescription [Text]
idLogins = lens _idLogins (\ s a -> s{_idLogins = a}) . _Default;

-- | A unique identifier in the format REGION:GUID.
idIdentityId :: Lens' IdentityDescription (Maybe Text)
idIdentityId = lens _idIdentityId (\ s a -> s{_idIdentityId = a});

instance FromJSON IdentityDescription where
        parseJSON
          = withObject "IdentityDescription"
              (\ x ->
                 IdentityDescription' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "CreationDate")
                     <*> (x .:? "Logins" .!= mempty)
                     <*> (x .:? "IdentityId"))

-- | An object representing a Cognito identity pool.
--
-- /See:/ 'identityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipSupportedLoginProviders'
--
-- * 'ipDeveloperProviderName'
--
-- * 'ipOpenIdConnectProviderARNs'
--
-- * 'ipIdentityPoolId'
--
-- * 'ipIdentityPoolName'
--
-- * 'ipAllowUnauthenticatedIdentities'
data IdentityPool = IdentityPool'
    { _ipSupportedLoginProviders        :: !(Maybe (Map Text Text))
    , _ipDeveloperProviderName          :: !(Maybe Text)
    , _ipOpenIdConnectProviderARNs      :: !(Maybe [Text])
    , _ipIdentityPoolId                 :: !Text
    , _ipIdentityPoolName               :: !Text
    , _ipAllowUnauthenticatedIdentities :: !Bool
    } deriving (Eq,Read,Show)

-- | 'IdentityPool' smart constructor.
identityPool :: Text -> Text -> Bool -> IdentityPool
identityPool pIdentityPoolId pIdentityPoolName pAllowUnauthenticatedIdentities =
    IdentityPool'
    { _ipSupportedLoginProviders = Nothing
    , _ipDeveloperProviderName = Nothing
    , _ipOpenIdConnectProviderARNs = Nothing
    , _ipIdentityPoolId = pIdentityPoolId
    , _ipIdentityPoolName = pIdentityPoolName
    , _ipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities
    }

-- | Optional key:value pairs mapping provider names to provider app IDs.
ipSupportedLoginProviders :: Lens' IdentityPool (HashMap Text Text)
ipSupportedLoginProviders = lens _ipSupportedLoginProviders (\ s a -> s{_ipSupportedLoginProviders = a}) . _Default . _Map;

-- | The \"domain\" by which Cognito will refer to your users.
ipDeveloperProviderName :: Lens' IdentityPool (Maybe Text)
ipDeveloperProviderName = lens _ipDeveloperProviderName (\ s a -> s{_ipDeveloperProviderName = a});

-- | A list of OpendID Connect provider ARNs.
ipOpenIdConnectProviderARNs :: Lens' IdentityPool [Text]
ipOpenIdConnectProviderARNs = lens _ipOpenIdConnectProviderARNs (\ s a -> s{_ipOpenIdConnectProviderARNs = a}) . _Default;

-- | An identity pool ID in the format REGION:GUID.
ipIdentityPoolId :: Lens' IdentityPool Text
ipIdentityPoolId = lens _ipIdentityPoolId (\ s a -> s{_ipIdentityPoolId = a});

-- | A string that you provide.
ipIdentityPoolName :: Lens' IdentityPool Text
ipIdentityPoolName = lens _ipIdentityPoolName (\ s a -> s{_ipIdentityPoolName = a});

-- | TRUE if the identity pool supports unauthenticated logins.
ipAllowUnauthenticatedIdentities :: Lens' IdentityPool Bool
ipAllowUnauthenticatedIdentities = lens _ipAllowUnauthenticatedIdentities (\ s a -> s{_ipAllowUnauthenticatedIdentities = a});

instance FromJSON IdentityPool where
        parseJSON
          = withObject "IdentityPool"
              (\ x ->
                 IdentityPool' <$>
                   (x .:? "SupportedLoginProviders" .!= mempty) <*>
                     (x .:? "DeveloperProviderName")
                     <*> (x .:? "OpenIdConnectProviderARNs" .!= mempty)
                     <*> (x .: "IdentityPoolId")
                     <*> (x .: "IdentityPoolName")
                     <*> (x .: "AllowUnauthenticatedIdentities"))

instance ToJSON IdentityPool where
        toJSON IdentityPool'{..}
          = object
              ["SupportedLoginProviders" .=
                 _ipSupportedLoginProviders,
               "DeveloperProviderName" .= _ipDeveloperProviderName,
               "OpenIdConnectProviderARNs" .=
                 _ipOpenIdConnectProviderARNs,
               "IdentityPoolId" .= _ipIdentityPoolId,
               "IdentityPoolName" .= _ipIdentityPoolName,
               "AllowUnauthenticatedIdentities" .=
                 _ipAllowUnauthenticatedIdentities]

-- | A description of the identity pool.
--
-- /See:/ 'identityPoolShortDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipsdIdentityPoolId'
--
-- * 'ipsdIdentityPoolName'
data IdentityPoolShortDescription = IdentityPoolShortDescription'
    { _ipsdIdentityPoolId   :: !(Maybe Text)
    , _ipsdIdentityPoolName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'IdentityPoolShortDescription' smart constructor.
identityPoolShortDescription :: IdentityPoolShortDescription
identityPoolShortDescription =
    IdentityPoolShortDescription'
    { _ipsdIdentityPoolId = Nothing
    , _ipsdIdentityPoolName = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
ipsdIdentityPoolId :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolId = lens _ipsdIdentityPoolId (\ s a -> s{_ipsdIdentityPoolId = a});

-- | A string that you provide.
ipsdIdentityPoolName :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolName = lens _ipsdIdentityPoolName (\ s a -> s{_ipsdIdentityPoolName = a});

instance FromJSON IdentityPoolShortDescription where
        parseJSON
          = withObject "IdentityPoolShortDescription"
              (\ x ->
                 IdentityPoolShortDescription' <$>
                   (x .:? "IdentityPoolId") <*>
                     (x .:? "IdentityPoolName"))

-- | An array of UnprocessedIdentityId objects, each of which contains an
-- ErrorCode and IdentityId.
--
-- /See:/ 'unprocessedIdentityId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiiErrorCode'
--
-- * 'uiiIdentityId'
data UnprocessedIdentityId = UnprocessedIdentityId'
    { _uiiErrorCode  :: !(Maybe CognitoErrorCode)
    , _uiiIdentityId :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'UnprocessedIdentityId' smart constructor.
unprocessedIdentityId :: UnprocessedIdentityId
unprocessedIdentityId =
    UnprocessedIdentityId'
    { _uiiErrorCode = Nothing
    , _uiiIdentityId = Nothing
    }

-- | The error code indicating the type of error that occurred.
uiiErrorCode :: Lens' UnprocessedIdentityId (Maybe CognitoErrorCode)
uiiErrorCode = lens _uiiErrorCode (\ s a -> s{_uiiErrorCode = a});

-- | A unique identifier in the format REGION:GUID.
uiiIdentityId :: Lens' UnprocessedIdentityId (Maybe Text)
uiiIdentityId = lens _uiiIdentityId (\ s a -> s{_uiiIdentityId = a});

instance FromJSON UnprocessedIdentityId where
        parseJSON
          = withObject "UnprocessedIdentityId"
              (\ x ->
                 UnprocessedIdentityId' <$>
                   (x .:? "ErrorCode") <*> (x .:? "IdentityId"))
