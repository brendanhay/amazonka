{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Product where

import Network.AWS.CognitoIdentity.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A provider representing an Amazon Cognito Identity User Pool and its client ID.
--
--
--
-- /See:/ 'cognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { _cipClientId             :: !(Maybe Text)
  , _cipServerSideTokenCheck :: !(Maybe Bool)
  , _cipProviderName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CognitoIdentityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipClientId' - The client ID for the Amazon Cognito Identity User Pool.
--
-- * 'cipServerSideTokenCheck' - TRUE if server-side token validation is enabled for the identity provider’s token.
--
-- * 'cipProviderName' - The provider name for an Amazon Cognito Identity User Pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
cognitoIdentityProvider
    :: CognitoIdentityProvider
cognitoIdentityProvider =
  CognitoIdentityProvider'
    { _cipClientId = Nothing
    , _cipServerSideTokenCheck = Nothing
    , _cipProviderName = Nothing
    }


-- | The client ID for the Amazon Cognito Identity User Pool.
cipClientId :: Lens' CognitoIdentityProvider (Maybe Text)
cipClientId = lens _cipClientId (\ s a -> s{_cipClientId = a})

-- | TRUE if server-side token validation is enabled for the identity provider’s token.
cipServerSideTokenCheck :: Lens' CognitoIdentityProvider (Maybe Bool)
cipServerSideTokenCheck = lens _cipServerSideTokenCheck (\ s a -> s{_cipServerSideTokenCheck = a})

-- | The provider name for an Amazon Cognito Identity User Pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
cipProviderName :: Lens' CognitoIdentityProvider (Maybe Text)
cipProviderName = lens _cipProviderName (\ s a -> s{_cipProviderName = a})

instance FromJSON CognitoIdentityProvider where
        parseJSON
          = withObject "CognitoIdentityProvider"
              (\ x ->
                 CognitoIdentityProvider' <$>
                   (x .:? "ClientId") <*> (x .:? "ServerSideTokenCheck")
                     <*> (x .:? "ProviderName"))

instance Hashable CognitoIdentityProvider where

instance NFData CognitoIdentityProvider where

instance ToJSON CognitoIdentityProvider where
        toJSON CognitoIdentityProvider'{..}
          = object
              (catMaybes
                 [("ClientId" .=) <$> _cipClientId,
                  ("ServerSideTokenCheck" .=) <$>
                    _cipServerSideTokenCheck,
                  ("ProviderName" .=) <$> _cipProviderName])

-- | Credentials for the provided identity ID.
--
--
--
-- /See:/ 'credentials' smart constructor.
data Credentials = Credentials'
  { _cSessionToken :: !(Maybe Text)
  , _cExpiration   :: !(Maybe POSIX)
  , _cSecretKey    :: !(Maybe Text)
  , _cAccessKeyId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cSessionToken' - The Session Token portion of the credentials
--
-- * 'cExpiration' - The date at which these credentials will expire.
--
-- * 'cSecretKey' - The Secret Access Key portion of the credentials
--
-- * 'cAccessKeyId' - The Access Key portion of the credentials.
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
cSessionToken = lens _cSessionToken (\ s a -> s{_cSessionToken = a})

-- | The date at which these credentials will expire.
cExpiration :: Lens' Credentials (Maybe UTCTime)
cExpiration = lens _cExpiration (\ s a -> s{_cExpiration = a}) . mapping _Time

-- | The Secret Access Key portion of the credentials
cSecretKey :: Lens' Credentials (Maybe Text)
cSecretKey = lens _cSecretKey (\ s a -> s{_cSecretKey = a})

-- | The Access Key portion of the credentials.
cAccessKeyId :: Lens' Credentials (Maybe Text)
cAccessKeyId = lens _cAccessKeyId (\ s a -> s{_cAccessKeyId = a})

instance FromJSON Credentials where
        parseJSON
          = withObject "Credentials"
              (\ x ->
                 Credentials' <$>
                   (x .:? "SessionToken") <*> (x .:? "Expiration") <*>
                     (x .:? "SecretKey")
                     <*> (x .:? "AccessKeyId"))

instance Hashable Credentials where

instance NFData Credentials where

-- | A description of the identity.
--
--
--
-- /See:/ 'identityDescription' smart constructor.
data IdentityDescription = IdentityDescription'
  { _idLastModifiedDate :: !(Maybe POSIX)
  , _idCreationDate     :: !(Maybe POSIX)
  , _idLogins           :: !(Maybe [Text])
  , _idIdentityId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idLastModifiedDate' - Date on which the identity was last modified.
--
-- * 'idCreationDate' - Date on which the identity was created.
--
-- * 'idLogins' - A set of optional name-value pairs that map provider names to provider tokens.
--
-- * 'idIdentityId' - A unique identifier in the format REGION:GUID.
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
idLastModifiedDate = lens _idLastModifiedDate (\ s a -> s{_idLastModifiedDate = a}) . mapping _Time

-- | Date on which the identity was created.
idCreationDate :: Lens' IdentityDescription (Maybe UTCTime)
idCreationDate = lens _idCreationDate (\ s a -> s{_idCreationDate = a}) . mapping _Time

-- | A set of optional name-value pairs that map provider names to provider tokens.
idLogins :: Lens' IdentityDescription [Text]
idLogins = lens _idLogins (\ s a -> s{_idLogins = a}) . _Default . _Coerce

-- | A unique identifier in the format REGION:GUID.
idIdentityId :: Lens' IdentityDescription (Maybe Text)
idIdentityId = lens _idIdentityId (\ s a -> s{_idIdentityId = a})

instance FromJSON IdentityDescription where
        parseJSON
          = withObject "IdentityDescription"
              (\ x ->
                 IdentityDescription' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "CreationDate")
                     <*> (x .:? "Logins" .!= mempty)
                     <*> (x .:? "IdentityId"))

instance Hashable IdentityDescription where

instance NFData IdentityDescription where

-- | An object representing an Amazon Cognito identity pool.
--
--
--
-- /See:/ 'identityPool' smart constructor.
data IdentityPool = IdentityPool'
  { _ipSamlProviderARNs               :: !(Maybe [Text])
  , _ipSupportedLoginProviders        :: !(Maybe (Map Text Text))
  , _ipDeveloperProviderName          :: !(Maybe Text)
  , _ipOpenIdConnectProviderARNs      :: !(Maybe [Text])
  , _ipCognitoIdentityProviders       :: !(Maybe [CognitoIdentityProvider])
  , _ipIdentityPoolId                 :: !Text
  , _ipIdentityPoolName               :: !Text
  , _ipAllowUnauthenticatedIdentities :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipSamlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
--
-- * 'ipSupportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
--
-- * 'ipDeveloperProviderName' - The "domain" by which Cognito will refer to your users.
--
-- * 'ipOpenIdConnectProviderARNs' - A list of OpendID Connect provider ARNs.
--
-- * 'ipCognitoIdentityProviders' - A list representing an Amazon Cognito Identity User Pool and its client ID.
--
-- * 'ipIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'ipIdentityPoolName' - A string that you provide.
--
-- * 'ipAllowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
identityPool
    :: Text -- ^ 'ipIdentityPoolId'
    -> Text -- ^ 'ipIdentityPoolName'
    -> Bool -- ^ 'ipAllowUnauthenticatedIdentities'
    -> IdentityPool
identityPool pIdentityPoolId_ pIdentityPoolName_ pAllowUnauthenticatedIdentities_ =
  IdentityPool'
    { _ipSamlProviderARNs = Nothing
    , _ipSupportedLoginProviders = Nothing
    , _ipDeveloperProviderName = Nothing
    , _ipOpenIdConnectProviderARNs = Nothing
    , _ipCognitoIdentityProviders = Nothing
    , _ipIdentityPoolId = pIdentityPoolId_
    , _ipIdentityPoolName = pIdentityPoolName_
    , _ipAllowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities_
    }


-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
ipSamlProviderARNs :: Lens' IdentityPool [Text]
ipSamlProviderARNs = lens _ipSamlProviderARNs (\ s a -> s{_ipSamlProviderARNs = a}) . _Default . _Coerce

-- | Optional key:value pairs mapping provider names to provider app IDs.
ipSupportedLoginProviders :: Lens' IdentityPool (HashMap Text Text)
ipSupportedLoginProviders = lens _ipSupportedLoginProviders (\ s a -> s{_ipSupportedLoginProviders = a}) . _Default . _Map

-- | The "domain" by which Cognito will refer to your users.
ipDeveloperProviderName :: Lens' IdentityPool (Maybe Text)
ipDeveloperProviderName = lens _ipDeveloperProviderName (\ s a -> s{_ipDeveloperProviderName = a})

-- | A list of OpendID Connect provider ARNs.
ipOpenIdConnectProviderARNs :: Lens' IdentityPool [Text]
ipOpenIdConnectProviderARNs = lens _ipOpenIdConnectProviderARNs (\ s a -> s{_ipOpenIdConnectProviderARNs = a}) . _Default . _Coerce

-- | A list representing an Amazon Cognito Identity User Pool and its client ID.
ipCognitoIdentityProviders :: Lens' IdentityPool [CognitoIdentityProvider]
ipCognitoIdentityProviders = lens _ipCognitoIdentityProviders (\ s a -> s{_ipCognitoIdentityProviders = a}) . _Default . _Coerce

-- | An identity pool ID in the format REGION:GUID.
ipIdentityPoolId :: Lens' IdentityPool Text
ipIdentityPoolId = lens _ipIdentityPoolId (\ s a -> s{_ipIdentityPoolId = a})

-- | A string that you provide.
ipIdentityPoolName :: Lens' IdentityPool Text
ipIdentityPoolName = lens _ipIdentityPoolName (\ s a -> s{_ipIdentityPoolName = a})

-- | TRUE if the identity pool supports unauthenticated logins.
ipAllowUnauthenticatedIdentities :: Lens' IdentityPool Bool
ipAllowUnauthenticatedIdentities = lens _ipAllowUnauthenticatedIdentities (\ s a -> s{_ipAllowUnauthenticatedIdentities = a})

instance FromJSON IdentityPool where
        parseJSON
          = withObject "IdentityPool"
              (\ x ->
                 IdentityPool' <$>
                   (x .:? "SamlProviderARNs" .!= mempty) <*>
                     (x .:? "SupportedLoginProviders" .!= mempty)
                     <*> (x .:? "DeveloperProviderName")
                     <*> (x .:? "OpenIdConnectProviderARNs" .!= mempty)
                     <*> (x .:? "CognitoIdentityProviders" .!= mempty)
                     <*> (x .: "IdentityPoolId")
                     <*> (x .: "IdentityPoolName")
                     <*> (x .: "AllowUnauthenticatedIdentities"))

instance Hashable IdentityPool where

instance NFData IdentityPool where

instance ToJSON IdentityPool where
        toJSON IdentityPool'{..}
          = object
              (catMaybes
                 [("SamlProviderARNs" .=) <$> _ipSamlProviderARNs,
                  ("SupportedLoginProviders" .=) <$>
                    _ipSupportedLoginProviders,
                  ("DeveloperProviderName" .=) <$>
                    _ipDeveloperProviderName,
                  ("OpenIdConnectProviderARNs" .=) <$>
                    _ipOpenIdConnectProviderARNs,
                  ("CognitoIdentityProviders" .=) <$>
                    _ipCognitoIdentityProviders,
                  Just ("IdentityPoolId" .= _ipIdentityPoolId),
                  Just ("IdentityPoolName" .= _ipIdentityPoolName),
                  Just
                    ("AllowUnauthenticatedIdentities" .=
                       _ipAllowUnauthenticatedIdentities)])

-- | A description of the identity pool.
--
--
--
-- /See:/ 'identityPoolShortDescription' smart constructor.
data IdentityPoolShortDescription = IdentityPoolShortDescription'
  { _ipsdIdentityPoolId   :: !(Maybe Text)
  , _ipsdIdentityPoolName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityPoolShortDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsdIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'ipsdIdentityPoolName' - A string that you provide.
identityPoolShortDescription
    :: IdentityPoolShortDescription
identityPoolShortDescription =
  IdentityPoolShortDescription'
    {_ipsdIdentityPoolId = Nothing, _ipsdIdentityPoolName = Nothing}


-- | An identity pool ID in the format REGION:GUID.
ipsdIdentityPoolId :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolId = lens _ipsdIdentityPoolId (\ s a -> s{_ipsdIdentityPoolId = a})

-- | A string that you provide.
ipsdIdentityPoolName :: Lens' IdentityPoolShortDescription (Maybe Text)
ipsdIdentityPoolName = lens _ipsdIdentityPoolName (\ s a -> s{_ipsdIdentityPoolName = a})

instance FromJSON IdentityPoolShortDescription where
        parseJSON
          = withObject "IdentityPoolShortDescription"
              (\ x ->
                 IdentityPoolShortDescription' <$>
                   (x .:? "IdentityPoolId") <*>
                     (x .:? "IdentityPoolName"))

instance Hashable IdentityPoolShortDescription where

instance NFData IdentityPoolShortDescription where

-- | A rule that maps a claim name, a claim value, and a match type to a role ARN.
--
--
--
-- /See:/ 'mappingRule' smart constructor.
data MappingRule = MappingRule'
  { _mrClaim     :: !Text
  , _mrMatchType :: !MappingRuleMatchType
  , _mrValue     :: !Text
  , _mrRoleARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MappingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrClaim' - The claim name that must be present in the token, for example, "isAdmin" or "paid".
--
-- * 'mrMatchType' - The match condition that specifies how closely the claim value in the IdP token must match @Value@ .
--
-- * 'mrValue' - A brief string that the claim must match, for example, "paid" or "yes".
--
-- * 'mrRoleARN' - The role ARN.
mappingRule
    :: Text -- ^ 'mrClaim'
    -> MappingRuleMatchType -- ^ 'mrMatchType'
    -> Text -- ^ 'mrValue'
    -> Text -- ^ 'mrRoleARN'
    -> MappingRule
mappingRule pClaim_ pMatchType_ pValue_ pRoleARN_ =
  MappingRule'
    { _mrClaim = pClaim_
    , _mrMatchType = pMatchType_
    , _mrValue = pValue_
    , _mrRoleARN = pRoleARN_
    }


-- | The claim name that must be present in the token, for example, "isAdmin" or "paid".
mrClaim :: Lens' MappingRule Text
mrClaim = lens _mrClaim (\ s a -> s{_mrClaim = a})

-- | The match condition that specifies how closely the claim value in the IdP token must match @Value@ .
mrMatchType :: Lens' MappingRule MappingRuleMatchType
mrMatchType = lens _mrMatchType (\ s a -> s{_mrMatchType = a})

-- | A brief string that the claim must match, for example, "paid" or "yes".
mrValue :: Lens' MappingRule Text
mrValue = lens _mrValue (\ s a -> s{_mrValue = a})

-- | The role ARN.
mrRoleARN :: Lens' MappingRule Text
mrRoleARN = lens _mrRoleARN (\ s a -> s{_mrRoleARN = a})

instance FromJSON MappingRule where
        parseJSON
          = withObject "MappingRule"
              (\ x ->
                 MappingRule' <$>
                   (x .: "Claim") <*> (x .: "MatchType") <*>
                     (x .: "Value")
                     <*> (x .: "RoleARN"))

instance Hashable MappingRule where

instance NFData MappingRule where

instance ToJSON MappingRule where
        toJSON MappingRule'{..}
          = object
              (catMaybes
                 [Just ("Claim" .= _mrClaim),
                  Just ("MatchType" .= _mrMatchType),
                  Just ("Value" .= _mrValue),
                  Just ("RoleARN" .= _mrRoleARN)])

-- | A role mapping.
--
--
--
-- /See:/ 'roleMapping' smart constructor.
data RoleMapping = RoleMapping'
  { _rmRulesConfiguration      :: !(Maybe RulesConfigurationType)
  , _rmAmbiguousRoleResolution :: !(Maybe AmbiguousRoleResolutionType)
  , _rmType                    :: !RoleMappingType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoleMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmRulesConfiguration' - The rules to be used for mapping users to roles. If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
--
-- * 'rmAmbiguousRoleResolution' - If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required. Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
--
-- * 'rmType' - The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
roleMapping
    :: RoleMappingType -- ^ 'rmType'
    -> RoleMapping
roleMapping pType_ =
  RoleMapping'
    { _rmRulesConfiguration = Nothing
    , _rmAmbiguousRoleResolution = Nothing
    , _rmType = pType_
    }


-- | The rules to be used for mapping users to roles. If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
rmRulesConfiguration :: Lens' RoleMapping (Maybe RulesConfigurationType)
rmRulesConfiguration = lens _rmRulesConfiguration (\ s a -> s{_rmRulesConfiguration = a})

-- | If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required. Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
rmAmbiguousRoleResolution :: Lens' RoleMapping (Maybe AmbiguousRoleResolutionType)
rmAmbiguousRoleResolution = lens _rmAmbiguousRoleResolution (\ s a -> s{_rmAmbiguousRoleResolution = a})

-- | The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
rmType :: Lens' RoleMapping RoleMappingType
rmType = lens _rmType (\ s a -> s{_rmType = a})

instance FromJSON RoleMapping where
        parseJSON
          = withObject "RoleMapping"
              (\ x ->
                 RoleMapping' <$>
                   (x .:? "RulesConfiguration") <*>
                     (x .:? "AmbiguousRoleResolution")
                     <*> (x .: "Type"))

instance Hashable RoleMapping where

instance NFData RoleMapping where

instance ToJSON RoleMapping where
        toJSON RoleMapping'{..}
          = object
              (catMaybes
                 [("RulesConfiguration" .=) <$> _rmRulesConfiguration,
                  ("AmbiguousRoleResolution" .=) <$>
                    _rmAmbiguousRoleResolution,
                  Just ("Type" .= _rmType)])

-- | A container for rules.
--
--
--
-- /See:/ 'rulesConfigurationType' smart constructor.
newtype RulesConfigurationType = RulesConfigurationType'
  { _rctRules :: List1 MappingRule
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RulesConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rctRules' - An array of rules. You can specify up to 25 rules per identity provider. Rules are evaluated in order. The first one to match specifies the role.
rulesConfigurationType
    :: NonEmpty MappingRule -- ^ 'rctRules'
    -> RulesConfigurationType
rulesConfigurationType pRules_ =
  RulesConfigurationType' {_rctRules = _List1 # pRules_}


-- | An array of rules. You can specify up to 25 rules per identity provider. Rules are evaluated in order. The first one to match specifies the role.
rctRules :: Lens' RulesConfigurationType (NonEmpty MappingRule)
rctRules = lens _rctRules (\ s a -> s{_rctRules = a}) . _List1

instance FromJSON RulesConfigurationType where
        parseJSON
          = withObject "RulesConfigurationType"
              (\ x -> RulesConfigurationType' <$> (x .: "Rules"))

instance Hashable RulesConfigurationType where

instance NFData RulesConfigurationType where

instance ToJSON RulesConfigurationType where
        toJSON RulesConfigurationType'{..}
          = object (catMaybes [Just ("Rules" .= _rctRules)])

-- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
--
--
--
-- /See:/ 'unprocessedIdentityId' smart constructor.
data UnprocessedIdentityId = UnprocessedIdentityId'
  { _uiiErrorCode  :: !(Maybe CognitoErrorCode)
  , _uiiIdentityId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnprocessedIdentityId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiiErrorCode' - The error code indicating the type of error that occurred.
--
-- * 'uiiIdentityId' - A unique identifier in the format REGION:GUID.
unprocessedIdentityId
    :: UnprocessedIdentityId
unprocessedIdentityId =
  UnprocessedIdentityId' {_uiiErrorCode = Nothing, _uiiIdentityId = Nothing}


-- | The error code indicating the type of error that occurred.
uiiErrorCode :: Lens' UnprocessedIdentityId (Maybe CognitoErrorCode)
uiiErrorCode = lens _uiiErrorCode (\ s a -> s{_uiiErrorCode = a})

-- | A unique identifier in the format REGION:GUID.
uiiIdentityId :: Lens' UnprocessedIdentityId (Maybe Text)
uiiIdentityId = lens _uiiIdentityId (\ s a -> s{_uiiIdentityId = a})

instance FromJSON UnprocessedIdentityId where
        parseJSON
          = withObject "UnprocessedIdentityId"
              (\ x ->
                 UnprocessedIdentityId' <$>
                   (x .:? "ErrorCode") <*> (x .:? "IdentityId"))

instance Hashable UnprocessedIdentityId where

instance NFData UnprocessedIdentityId where
