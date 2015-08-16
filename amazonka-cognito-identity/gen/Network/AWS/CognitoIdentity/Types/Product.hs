{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Product where

import           Network.AWS.CognitoIdentity.Types.Sum
import           Network.AWS.Prelude

-- | Credentials for the the provided identity ID.
--
-- /See:/ 'credentials' smart constructor.
data Credentials = Credentials'
    { _cSessionToken :: !(Maybe Text)
    , _cExpiration   :: !(Maybe POSIX)
    , _cSecretKey    :: !(Maybe Text)
    , _cAccessKeyId  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cSessionToken'
--
-- * 'cExpiration'
--
-- * 'cSecretKey'
--
-- * 'cAccessKeyId'
credentials
    :: Credentials
credentials =
    Credentials'
    { _cSessionToken = Nothing
    , _cExpiration = Nothing
    , _cSecretKey = Nothing
    , _cAccessKeyId = Nothing
    }

-- | The Session Token portion of the credentials
cSessionToken :: Lens' Credentials (Maybe Text)
cSessionToken = lens _cSessionToken (\ s a -> s{_cSessionToken = a});

-- | The date at which these credentials will expire.
cExpiration :: Lens' Credentials (Maybe UTCTime)
cExpiration = lens _cExpiration (\ s a -> s{_cExpiration = a}) . mapping _Time;

-- | The Secret Access Key portion of the credentials
cSecretKey :: Lens' Credentials (Maybe Text)
cSecretKey = lens _cSecretKey (\ s a -> s{_cSecretKey = a});

-- | The Access Key portion of the credentials.
cAccessKeyId :: Lens' Credentials (Maybe Text)
cAccessKeyId = lens _cAccessKeyId (\ s a -> s{_cAccessKeyId = a});

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
data IdentityDescription = IdentityDescription'
    { _idLastModifiedDate :: !(Maybe POSIX)
    , _idCreationDate     :: !(Maybe POSIX)
    , _idLogins           :: !(Maybe [Text])
    , _idIdentityId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IdentityDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idLastModifiedDate'
--
-- * 'idCreationDate'
--
-- * 'idLogins'
--
-- * 'idIdentityId'
identityDescription
    :: IdentityDescription
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
idLogins = lens _idLogins (\ s a -> s{_idLogins = a}) . _Default . _Coerce;

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
data IdentityPool = IdentityPool'
    { _ipSupportedLoginProviders        :: !(Maybe (Map Text Text))
    , _ipDeveloperProviderName          :: !(Maybe Text)
    , _ipOpenIdConnectProviderARNs      :: !(Maybe [Text])
    , _ipIdentityPoolId                 :: !Text
    , _ipIdentityPoolName               :: !Text
    , _ipAllowUnauthenticatedIdentities :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IdentityPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
identityPool
    :: Text -- ^ 'ipIdentityPoolId'
    -> Text -- ^ 'ipIdentityPoolName'
    -> Bool -- ^ 'ipAllowUnauthenticatedIdentities'
    -> IdentityPool
identityPool pIdentityPoolId_ pIdentityPoolName_ pAllowUnauthenticatedIdentities_ =
    IdentityPool'
    { _ipSupportedLoginProviders = Nothing
    , _ipDeveloperProviderName = Nothing
    , _ipOpenIdConnectProviderARNs = Nothing
    , _ipIdentityPoolId = pIdentityPoolId_
    , _ipIdentityPoolName = pIdentityPoolName_
    , _ipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities_
    }

-- | Optional key:value pairs mapping provider names to provider app IDs.
ipSupportedLoginProviders :: Lens' IdentityPool (HashMap Text Text)
ipSupportedLoginProviders = lens _ipSupportedLoginProviders (\ s a -> s{_ipSupportedLoginProviders = a}) . _Default . _Map;

-- | The \"domain\" by which Cognito will refer to your users.
ipDeveloperProviderName :: Lens' IdentityPool (Maybe Text)
ipDeveloperProviderName = lens _ipDeveloperProviderName (\ s a -> s{_ipDeveloperProviderName = a});

-- | A list of OpendID Connect provider ARNs.
ipOpenIdConnectProviderARNs :: Lens' IdentityPool [Text]
ipOpenIdConnectProviderARNs = lens _ipOpenIdConnectProviderARNs (\ s a -> s{_ipOpenIdConnectProviderARNs = a}) . _Default . _Coerce;

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
data IdentityPoolShortDescription = IdentityPoolShortDescription'
    { _ipsdIdentityPoolId   :: !(Maybe Text)
    , _ipsdIdentityPoolName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IdentityPoolShortDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsdIdentityPoolId'
--
-- * 'ipsdIdentityPoolName'
identityPoolShortDescription
    :: IdentityPoolShortDescription
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
data UnprocessedIdentityId = UnprocessedIdentityId'
    { _uiiErrorCode  :: !(Maybe CognitoErrorCode)
    , _uiiIdentityId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UnprocessedIdentityId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiiErrorCode'
--
-- * 'uiiIdentityId'
unprocessedIdentityId
    :: UnprocessedIdentityId
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
