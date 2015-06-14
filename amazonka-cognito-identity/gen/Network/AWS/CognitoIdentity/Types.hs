{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

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
    -- ** Errors
    , JSONError

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
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    , ipDeveloperProviderName

    -- * IdentityPoolShortDescription
    , IdentityPoolShortDescription
    , identityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName

    -- * UnprocessedIdentityId
    , UnprocessedIdentityId
    , unprocessedIdentityId
    , uiiCognitoErrorCode
    , uiiIdentityId
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-06-30@ of the Amazon Cognito Identity SDK.
data CognitoIdentity

instance AWSService CognitoIdentity where
    type Sg CognitoIdentity = V4
    type Er CognitoIdentity = JSONError

    service = service'
      where
        service' :: Service CognitoIdentity
        service' = Service
            { _svcAbbrev  = "CognitoIdentity"
            , _svcPrefix  = "cognito-identity"
            , _svcVersion = "2014-06-30"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CognitoIdentity
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

data CognitoErrorCode = InternalServerError | AccessDenied deriving (Eq, Ord, Read, Show, Enum, Generic)

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

-- | /See:/ 'credentials' smart constructor.
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
data Credentials = Credentials'{_creSessionToken :: Maybe Text, _creExpiration :: Maybe POSIX, _creSecretKey :: Maybe Text, _creAccessKeyId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Credentials' smart constructor.
credentials :: Credentials
credentials = Credentials'{_creSessionToken = Nothing, _creExpiration = Nothing, _creSecretKey = Nothing, _creAccessKeyId = Nothing};

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
                   x .:? "SessionToken" <*> x .:? "Expiration" <*>
                     x .:? "SecretKey"
                     <*> x .:? "AccessKeyId")

-- | /See:/ 'identityDescription' smart constructor.
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
data IdentityDescription = IdentityDescription'{_idLastModifiedDate :: Maybe POSIX, _idCreationDate :: Maybe POSIX, _idLogins :: [Text], _idIdentityId :: Text} deriving (Eq, Read, Show)

-- | 'IdentityDescription' smart constructor.
identityDescription :: Text -> IdentityDescription
identityDescription pIdentityId = IdentityDescription'{_idLastModifiedDate = Nothing, _idCreationDate = Nothing, _idLogins = mempty, _idIdentityId = pIdentityId};

-- | Date on which the identity was last modified.
idLastModifiedDate :: Lens' IdentityDescription (Maybe UTCTime)
idLastModifiedDate = lens _idLastModifiedDate (\ s a -> s{_idLastModifiedDate = a}) . mapping _Time;

-- | Date on which the identity was created.
idCreationDate :: Lens' IdentityDescription (Maybe UTCTime)
idCreationDate = lens _idCreationDate (\ s a -> s{_idCreationDate = a}) . mapping _Time;

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
idLogins :: Lens' IdentityDescription [Text]
idLogins = lens _idLogins (\ s a -> s{_idLogins = a});

-- | A unique identifier in the format REGION:GUID.
idIdentityId :: Lens' IdentityDescription Text
idIdentityId = lens _idIdentityId (\ s a -> s{_idIdentityId = a});

instance FromJSON IdentityDescription where
        parseJSON
          = withObject "IdentityDescription"
              (\ x ->
                 IdentityDescription' <$>
                   x .:? "LastModifiedDate" <*> x .:? "CreationDate" <*>
                     x .:? "Logins" .!= mempty
                     <*> x .: "IdentityId")

-- | /See:/ 'identityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipSupportedLoginProviders'
--
-- * 'ipOpenIdConnectProviderARNs'
--
-- * 'ipIdentityPoolId'
--
-- * 'ipIdentityPoolName'
--
-- * 'ipAllowUnauthenticatedIdentities'
--
-- * 'ipDeveloperProviderName'
data IdentityPool = IdentityPool'{_ipSupportedLoginProviders :: HashMap Text Text, _ipOpenIdConnectProviderARNs :: [Text], _ipIdentityPoolId :: Text, _ipIdentityPoolName :: Text, _ipAllowUnauthenticatedIdentities :: Bool, _ipDeveloperProviderName :: Text} deriving (Eq, Read, Show)

-- | 'IdentityPool' smart constructor.
identityPool :: Text -> Text -> Bool -> Text -> IdentityPool
identityPool pIdentityPoolId pIdentityPoolName pAllowUnauthenticatedIdentities pDeveloperProviderName = IdentityPool'{_ipSupportedLoginProviders = mempty, _ipOpenIdConnectProviderARNs = mempty, _ipIdentityPoolId = pIdentityPoolId, _ipIdentityPoolName = pIdentityPoolName, _ipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities, _ipDeveloperProviderName = pDeveloperProviderName};

-- | Optional key:value pairs mapping provider names to provider app IDs.
ipSupportedLoginProviders :: Lens' IdentityPool (HashMap Text Text)
ipSupportedLoginProviders = lens _ipSupportedLoginProviders (\ s a -> s{_ipSupportedLoginProviders = a}) . _Coerce;

-- | A list of OpendID Connect provider ARNs.
ipOpenIdConnectProviderARNs :: Lens' IdentityPool [Text]
ipOpenIdConnectProviderARNs = lens _ipOpenIdConnectProviderARNs (\ s a -> s{_ipOpenIdConnectProviderARNs = a});

-- | An identity pool ID in the format REGION:GUID.
ipIdentityPoolId :: Lens' IdentityPool Text
ipIdentityPoolId = lens _ipIdentityPoolId (\ s a -> s{_ipIdentityPoolId = a});

-- | A string that you provide.
ipIdentityPoolName :: Lens' IdentityPool Text
ipIdentityPoolName = lens _ipIdentityPoolName (\ s a -> s{_ipIdentityPoolName = a});

-- | TRUE if the identity pool supports unauthenticated logins.
ipAllowUnauthenticatedIdentities :: Lens' IdentityPool Bool
ipAllowUnauthenticatedIdentities = lens _ipAllowUnauthenticatedIdentities (\ s a -> s{_ipAllowUnauthenticatedIdentities = a});

-- | The \"domain\" by which Cognito will refer to your users.
ipDeveloperProviderName :: Lens' IdentityPool Text
ipDeveloperProviderName = lens _ipDeveloperProviderName (\ s a -> s{_ipDeveloperProviderName = a});

instance FromJSON IdentityPool where
        parseJSON
          = withObject "IdentityPool"
              (\ x ->
                 IdentityPool' <$>
                   x .:? "SupportedLoginProviders" .!= mempty <*>
                     x .:? "OpenIdConnectProviderARNs" .!= mempty
                     <*> x .: "IdentityPoolId"
                     <*> x .: "IdentityPoolName"
                     <*> x .: "AllowUnauthenticatedIdentities"
                     <*> x .: "DeveloperProviderName")

instance ToJSON IdentityPool where
        toJSON IdentityPool'{..}
          = object
              ["SupportedLoginProviders" .=
                 _ipSupportedLoginProviders,
               "OpenIdConnectProviderARNs" .=
                 _ipOpenIdConnectProviderARNs,
               "IdentityPoolId" .= _ipIdentityPoolId,
               "IdentityPoolName" .= _ipIdentityPoolName,
               "AllowUnauthenticatedIdentities" .=
                 _ipAllowUnauthenticatedIdentities,
               "DeveloperProviderName" .= _ipDeveloperProviderName]

-- | /See:/ 'identityPoolShortDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipsdIdentityPoolId'
--
-- * 'ipsdIdentityPoolName'
data IdentityPoolShortDescription = IdentityPoolShortDescription'{_ipsdIdentityPoolId :: Text, _ipsdIdentityPoolName :: Text} deriving (Eq, Read, Show)

-- | 'IdentityPoolShortDescription' smart constructor.
identityPoolShortDescription :: Text -> Text -> IdentityPoolShortDescription
identityPoolShortDescription pIdentityPoolId pIdentityPoolName = IdentityPoolShortDescription'{_ipsdIdentityPoolId = pIdentityPoolId, _ipsdIdentityPoolName = pIdentityPoolName};

-- | An identity pool ID in the format REGION:GUID.
ipsdIdentityPoolId :: Lens' IdentityPoolShortDescription Text
ipsdIdentityPoolId = lens _ipsdIdentityPoolId (\ s a -> s{_ipsdIdentityPoolId = a});

-- | A string that you provide.
ipsdIdentityPoolName :: Lens' IdentityPoolShortDescription Text
ipsdIdentityPoolName = lens _ipsdIdentityPoolName (\ s a -> s{_ipsdIdentityPoolName = a});

instance FromJSON IdentityPoolShortDescription where
        parseJSON
          = withObject "IdentityPoolShortDescription"
              (\ x ->
                 IdentityPoolShortDescription' <$>
                   x .: "IdentityPoolId" <*> x .: "IdentityPoolName")

-- | /See:/ 'unprocessedIdentityId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiiCognitoErrorCode'
--
-- * 'uiiIdentityId'
data UnprocessedIdentityId = UnprocessedIdentityId'{_uiiCognitoErrorCode :: Maybe CognitoErrorCode, _uiiIdentityId :: Text} deriving (Eq, Read, Show)

-- | 'UnprocessedIdentityId' smart constructor.
unprocessedIdentityId :: Text -> UnprocessedIdentityId
unprocessedIdentityId pIdentityId = UnprocessedIdentityId'{_uiiCognitoErrorCode = Nothing, _uiiIdentityId = pIdentityId};

-- | The error code indicating the type of error that occurred.
uiiCognitoErrorCode :: Lens' UnprocessedIdentityId (Maybe CognitoErrorCode)
uiiCognitoErrorCode = lens _uiiCognitoErrorCode (\ s a -> s{_uiiCognitoErrorCode = a});

-- | A unique identifier in the format REGION:GUID.
uiiIdentityId :: Lens' UnprocessedIdentityId Text
uiiIdentityId = lens _uiiIdentityId (\ s a -> s{_uiiIdentityId = a});

instance FromJSON UnprocessedIdentityId where
        parseJSON
          = withObject "UnprocessedIdentityId"
              (\ x ->
                 UnprocessedIdentityId' <$>
                   x .:? "CognitoErrorCode" <*> x .: "IdentityId")
