{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Product where

import Network.AWS.ELBv2.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an action.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aFixedResponseConfig       :: !(Maybe FixedResponseActionConfig)
  , _aTargetGroupARN            :: !(Maybe Text)
  , _aRedirectConfig            :: !(Maybe RedirectActionConfig)
  , _aAuthenticateCognitoConfig :: !(Maybe AuthenticateCognitoActionConfig)
  , _aOrder                     :: !(Maybe Nat)
  , _aAuthenticateOidcConfig    :: !(Maybe AuthenticateOidcActionConfig)
  , _aType                      :: !ActionTypeEnum
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aFixedResponseConfig' - [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
--
-- * 'aTargetGroupARN' - The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ .
--
-- * 'aRedirectConfig' - [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
--
-- * 'aAuthenticateCognitoConfig' - [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
--
-- * 'aOrder' - The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first. The final action to be performed must be a @forward@ or a @fixed-response@ action.
--
-- * 'aAuthenticateOidcConfig' - [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
--
-- * 'aType' - The type of action. Each rule must include exactly one of the following types of actions: @forward@ , @fixed-response@ , or @redirect@ .
action
    :: ActionTypeEnum -- ^ 'aType'
    -> Action
action pType_ =
  Action'
    { _aFixedResponseConfig = Nothing
    , _aTargetGroupARN = Nothing
    , _aRedirectConfig = Nothing
    , _aAuthenticateCognitoConfig = Nothing
    , _aOrder = Nothing
    , _aAuthenticateOidcConfig = Nothing
    , _aType = pType_
    }


-- | [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
aFixedResponseConfig :: Lens' Action (Maybe FixedResponseActionConfig)
aFixedResponseConfig = lens _aFixedResponseConfig (\ s a -> s{_aFixedResponseConfig = a})

-- | The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ .
aTargetGroupARN :: Lens' Action (Maybe Text)
aTargetGroupARN = lens _aTargetGroupARN (\ s a -> s{_aTargetGroupARN = a})

-- | [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
aRedirectConfig :: Lens' Action (Maybe RedirectActionConfig)
aRedirectConfig = lens _aRedirectConfig (\ s a -> s{_aRedirectConfig = a})

-- | [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
aAuthenticateCognitoConfig :: Lens' Action (Maybe AuthenticateCognitoActionConfig)
aAuthenticateCognitoConfig = lens _aAuthenticateCognitoConfig (\ s a -> s{_aAuthenticateCognitoConfig = a})

-- | The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first. The final action to be performed must be a @forward@ or a @fixed-response@ action.
aOrder :: Lens' Action (Maybe Natural)
aOrder = lens _aOrder (\ s a -> s{_aOrder = a}) . mapping _Nat

-- | [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
aAuthenticateOidcConfig :: Lens' Action (Maybe AuthenticateOidcActionConfig)
aAuthenticateOidcConfig = lens _aAuthenticateOidcConfig (\ s a -> s{_aAuthenticateOidcConfig = a})

-- | The type of action. Each rule must include exactly one of the following types of actions: @forward@ , @fixed-response@ , or @redirect@ .
aType :: Lens' Action ActionTypeEnum
aType = lens _aType (\ s a -> s{_aType = a})

instance FromXML Action where
        parseXML x
          = Action' <$>
              (x .@? "FixedResponseConfig") <*>
                (x .@? "TargetGroupArn")
                <*> (x .@? "RedirectConfig")
                <*> (x .@? "AuthenticateCognitoConfig")
                <*> (x .@? "Order")
                <*> (x .@? "AuthenticateOidcConfig")
                <*> (x .@ "Type")

instance Hashable Action where

instance NFData Action where

instance ToQuery Action where
        toQuery Action'{..}
          = mconcat
              ["FixedResponseConfig" =: _aFixedResponseConfig,
               "TargetGroupArn" =: _aTargetGroupARN,
               "RedirectConfig" =: _aRedirectConfig,
               "AuthenticateCognitoConfig" =:
                 _aAuthenticateCognitoConfig,
               "Order" =: _aOrder,
               "AuthenticateOidcConfig" =: _aAuthenticateOidcConfig,
               "Type" =: _aType]

-- | Request parameters to use when integrating with Amazon Cognito to authenticate users.
--
--
--
-- /See:/ 'authenticateCognitoActionConfig' smart constructor.
data AuthenticateCognitoActionConfig = AuthenticateCognitoActionConfig'
  { _acacAuthenticationRequestExtraParams :: !(Maybe (Map Text Text))
  , _acacScope :: !(Maybe Text)
  , _acacOnUnauthenticatedRequest :: !(Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
  , _acacSessionCookieName :: !(Maybe Text)
  , _acacSessionTimeout :: !(Maybe Integer)
  , _acacUserPoolARN :: !Text
  , _acacUserPoolClientId :: !Text
  , _acacUserPoolDomain :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthenticateCognitoActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acacAuthenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- * 'acacScope' - The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- * 'acacOnUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
-- * 'acacSessionCookieName' - The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- * 'acacSessionTimeout' - The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- * 'acacUserPoolARN' - The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
--
-- * 'acacUserPoolClientId' - The ID of the Amazon Cognito user pool client.
--
-- * 'acacUserPoolDomain' - The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
authenticateCognitoActionConfig
    :: Text -- ^ 'acacUserPoolARN'
    -> Text -- ^ 'acacUserPoolClientId'
    -> Text -- ^ 'acacUserPoolDomain'
    -> AuthenticateCognitoActionConfig
authenticateCognitoActionConfig pUserPoolARN_ pUserPoolClientId_ pUserPoolDomain_ =
  AuthenticateCognitoActionConfig'
    { _acacAuthenticationRequestExtraParams = Nothing
    , _acacScope = Nothing
    , _acacOnUnauthenticatedRequest = Nothing
    , _acacSessionCookieName = Nothing
    , _acacSessionTimeout = Nothing
    , _acacUserPoolARN = pUserPoolARN_
    , _acacUserPoolClientId = pUserPoolClientId_
    , _acacUserPoolDomain = pUserPoolDomain_
    }


-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
acacAuthenticationRequestExtraParams :: Lens' AuthenticateCognitoActionConfig (HashMap Text Text)
acacAuthenticationRequestExtraParams = lens _acacAuthenticationRequestExtraParams (\ s a -> s{_acacAuthenticationRequestExtraParams = a}) . _Default . _Map

-- | The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
acacScope :: Lens' AuthenticateCognitoActionConfig (Maybe Text)
acacScope = lens _acacScope (\ s a -> s{_acacScope = a})

-- | The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
acacOnUnauthenticatedRequest :: Lens' AuthenticateCognitoActionConfig (Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
acacOnUnauthenticatedRequest = lens _acacOnUnauthenticatedRequest (\ s a -> s{_acacOnUnauthenticatedRequest = a})

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
acacSessionCookieName :: Lens' AuthenticateCognitoActionConfig (Maybe Text)
acacSessionCookieName = lens _acacSessionCookieName (\ s a -> s{_acacSessionCookieName = a})

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
acacSessionTimeout :: Lens' AuthenticateCognitoActionConfig (Maybe Integer)
acacSessionTimeout = lens _acacSessionTimeout (\ s a -> s{_acacSessionTimeout = a})

-- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
acacUserPoolARN :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolARN = lens _acacUserPoolARN (\ s a -> s{_acacUserPoolARN = a})

-- | The ID of the Amazon Cognito user pool client.
acacUserPoolClientId :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolClientId = lens _acacUserPoolClientId (\ s a -> s{_acacUserPoolClientId = a})

-- | The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
acacUserPoolDomain :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolDomain = lens _acacUserPoolDomain (\ s a -> s{_acacUserPoolDomain = a})

instance FromXML AuthenticateCognitoActionConfig
         where
        parseXML x
          = AuthenticateCognitoActionConfig' <$>
              (x .@? "AuthenticationRequestExtraParams" .!@ mempty
                 >>= may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "Scope")
                <*> (x .@? "OnUnauthenticatedRequest")
                <*> (x .@? "SessionCookieName")
                <*> (x .@? "SessionTimeout")
                <*> (x .@ "UserPoolArn")
                <*> (x .@ "UserPoolClientId")
                <*> (x .@ "UserPoolDomain")

instance Hashable AuthenticateCognitoActionConfig
         where

instance NFData AuthenticateCognitoActionConfig where

instance ToQuery AuthenticateCognitoActionConfig
         where
        toQuery AuthenticateCognitoActionConfig'{..}
          = mconcat
              ["AuthenticationRequestExtraParams" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _acacAuthenticationRequestExtraParams),
               "Scope" =: _acacScope,
               "OnUnauthenticatedRequest" =:
                 _acacOnUnauthenticatedRequest,
               "SessionCookieName" =: _acacSessionCookieName,
               "SessionTimeout" =: _acacSessionTimeout,
               "UserPoolArn" =: _acacUserPoolARN,
               "UserPoolClientId" =: _acacUserPoolClientId,
               "UserPoolDomain" =: _acacUserPoolDomain]

-- | Request parameters when using an identity provider (IdP) that is compliant with OpenID Connect (OIDC) to authenticate users.
--
--
--
-- /See:/ 'authenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { _aoacClientSecret :: !(Maybe Text)
  , _aoacUseExistingClientSecret :: !(Maybe Bool)
  , _aoacAuthenticationRequestExtraParams :: !(Maybe (Map Text Text))
  , _aoacScope :: !(Maybe Text)
  , _aoacOnUnauthenticatedRequest :: !(Maybe AuthenticateOidcActionConditionalBehaviorEnum)
  , _aoacSessionCookieName :: !(Maybe Text)
  , _aoacSessionTimeout :: !(Maybe Integer)
  , _aoacIssuer :: !Text
  , _aoacAuthorizationEndpoint :: !Text
  , _aoacTokenEndpoint :: !Text
  , _aoacUserInfoEndpoint :: !Text
  , _aoacClientId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthenticateOidcActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoacClientSecret' - The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
--
-- * 'aoacUseExistingClientSecret' - Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
--
-- * 'aoacAuthenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- * 'aoacScope' - The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- * 'aoacOnUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
-- * 'aoacSessionCookieName' - The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- * 'aoacSessionTimeout' - The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- * 'aoacIssuer' - The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacAuthorizationEndpoint' - The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacTokenEndpoint' - The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacUserInfoEndpoint' - The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacClientId' - The OAuth 2.0 client identifier.
authenticateOidcActionConfig
    :: Text -- ^ 'aoacIssuer'
    -> Text -- ^ 'aoacAuthorizationEndpoint'
    -> Text -- ^ 'aoacTokenEndpoint'
    -> Text -- ^ 'aoacUserInfoEndpoint'
    -> Text -- ^ 'aoacClientId'
    -> AuthenticateOidcActionConfig
authenticateOidcActionConfig pIssuer_ pAuthorizationEndpoint_ pTokenEndpoint_ pUserInfoEndpoint_ pClientId_ =
  AuthenticateOidcActionConfig'
    { _aoacClientSecret = Nothing
    , _aoacUseExistingClientSecret = Nothing
    , _aoacAuthenticationRequestExtraParams = Nothing
    , _aoacScope = Nothing
    , _aoacOnUnauthenticatedRequest = Nothing
    , _aoacSessionCookieName = Nothing
    , _aoacSessionTimeout = Nothing
    , _aoacIssuer = pIssuer_
    , _aoacAuthorizationEndpoint = pAuthorizationEndpoint_
    , _aoacTokenEndpoint = pTokenEndpoint_
    , _aoacUserInfoEndpoint = pUserInfoEndpoint_
    , _aoacClientId = pClientId_
    }


-- | The OAuth 2.0 client secret. This parameter is required if you are creating a rule. If you are modifying a rule, you can omit this parameter if you set @UseExistingClientSecret@ to true.
aoacClientSecret :: Lens' AuthenticateOidcActionConfig (Maybe Text)
aoacClientSecret = lens _aoacClientSecret (\ s a -> s{_aoacClientSecret = a})

-- | Indicates whether to use the existing client secret when modifying a rule. If you are creating a rule, you can omit this parameter or set it to false.
aoacUseExistingClientSecret :: Lens' AuthenticateOidcActionConfig (Maybe Bool)
aoacUseExistingClientSecret = lens _aoacUseExistingClientSecret (\ s a -> s{_aoacUseExistingClientSecret = a})

-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
aoacAuthenticationRequestExtraParams :: Lens' AuthenticateOidcActionConfig (HashMap Text Text)
aoacAuthenticationRequestExtraParams = lens _aoacAuthenticationRequestExtraParams (\ s a -> s{_aoacAuthenticationRequestExtraParams = a}) . _Default . _Map

-- | The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
aoacScope :: Lens' AuthenticateOidcActionConfig (Maybe Text)
aoacScope = lens _aoacScope (\ s a -> s{_aoacScope = a})

-- | The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
aoacOnUnauthenticatedRequest :: Lens' AuthenticateOidcActionConfig (Maybe AuthenticateOidcActionConditionalBehaviorEnum)
aoacOnUnauthenticatedRequest = lens _aoacOnUnauthenticatedRequest (\ s a -> s{_aoacOnUnauthenticatedRequest = a})

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
aoacSessionCookieName :: Lens' AuthenticateOidcActionConfig (Maybe Text)
aoacSessionCookieName = lens _aoacSessionCookieName (\ s a -> s{_aoacSessionCookieName = a})

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
aoacSessionTimeout :: Lens' AuthenticateOidcActionConfig (Maybe Integer)
aoacSessionTimeout = lens _aoacSessionTimeout (\ s a -> s{_aoacSessionTimeout = a})

-- | The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacIssuer :: Lens' AuthenticateOidcActionConfig Text
aoacIssuer = lens _aoacIssuer (\ s a -> s{_aoacIssuer = a})

-- | The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacAuthorizationEndpoint :: Lens' AuthenticateOidcActionConfig Text
aoacAuthorizationEndpoint = lens _aoacAuthorizationEndpoint (\ s a -> s{_aoacAuthorizationEndpoint = a})

-- | The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacTokenEndpoint :: Lens' AuthenticateOidcActionConfig Text
aoacTokenEndpoint = lens _aoacTokenEndpoint (\ s a -> s{_aoacTokenEndpoint = a})

-- | The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacUserInfoEndpoint :: Lens' AuthenticateOidcActionConfig Text
aoacUserInfoEndpoint = lens _aoacUserInfoEndpoint (\ s a -> s{_aoacUserInfoEndpoint = a})

-- | The OAuth 2.0 client identifier.
aoacClientId :: Lens' AuthenticateOidcActionConfig Text
aoacClientId = lens _aoacClientId (\ s a -> s{_aoacClientId = a})

instance FromXML AuthenticateOidcActionConfig where
        parseXML x
          = AuthenticateOidcActionConfig' <$>
              (x .@? "ClientSecret") <*>
                (x .@? "UseExistingClientSecret")
                <*>
                (x .@? "AuthenticationRequestExtraParams" .!@ mempty
                   >>= may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "Scope")
                <*> (x .@? "OnUnauthenticatedRequest")
                <*> (x .@? "SessionCookieName")
                <*> (x .@? "SessionTimeout")
                <*> (x .@ "Issuer")
                <*> (x .@ "AuthorizationEndpoint")
                <*> (x .@ "TokenEndpoint")
                <*> (x .@ "UserInfoEndpoint")
                <*> (x .@ "ClientId")

instance Hashable AuthenticateOidcActionConfig where

instance NFData AuthenticateOidcActionConfig where

instance ToQuery AuthenticateOidcActionConfig where
        toQuery AuthenticateOidcActionConfig'{..}
          = mconcat
              ["ClientSecret" =: _aoacClientSecret,
               "UseExistingClientSecret" =:
                 _aoacUseExistingClientSecret,
               "AuthenticationRequestExtraParams" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _aoacAuthenticationRequestExtraParams),
               "Scope" =: _aoacScope,
               "OnUnauthenticatedRequest" =:
                 _aoacOnUnauthenticatedRequest,
               "SessionCookieName" =: _aoacSessionCookieName,
               "SessionTimeout" =: _aoacSessionTimeout,
               "Issuer" =: _aoacIssuer,
               "AuthorizationEndpoint" =:
                 _aoacAuthorizationEndpoint,
               "TokenEndpoint" =: _aoacTokenEndpoint,
               "UserInfoEndpoint" =: _aoacUserInfoEndpoint,
               "ClientId" =: _aoacClientId]

-- | Information about an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azSubnetId              :: !(Maybe Text)
  , _azZoneName              :: !(Maybe Text)
  , _azLoadBalancerAddresses :: !(Maybe [LoadBalancerAddress])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azSubnetId' - The ID of the subnet.
--
-- * 'azZoneName' - The name of the Availability Zone.
--
-- * 'azLoadBalancerAddresses' - [Network Load Balancers] The static IP address.
availabilityZone
    :: AvailabilityZone
availabilityZone =
  AvailabilityZone'
    { _azSubnetId = Nothing
    , _azZoneName = Nothing
    , _azLoadBalancerAddresses = Nothing
    }


-- | The ID of the subnet.
azSubnetId :: Lens' AvailabilityZone (Maybe Text)
azSubnetId = lens _azSubnetId (\ s a -> s{_azSubnetId = a})

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a})

-- | [Network Load Balancers] The static IP address.
azLoadBalancerAddresses :: Lens' AvailabilityZone [LoadBalancerAddress]
azLoadBalancerAddresses = lens _azLoadBalancerAddresses (\ s a -> s{_azLoadBalancerAddresses = a}) . _Default . _Coerce

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "SubnetId") <*> (x .@? "ZoneName") <*>
                (x .@? "LoadBalancerAddresses" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | Information about an SSL server certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cCertificateARN :: !(Maybe Text)
  , _cIsDefault      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) of the certificate.
--
-- * 'cIsDefault' - Indicates whether the certificate is the default certificate. Do not set @IsDefault@ when specifying a certificate as an input parameter.
certificate
    :: Certificate
certificate = Certificate' {_cCertificateARN = Nothing, _cIsDefault = Nothing}


-- | The Amazon Resource Name (ARN) of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | Indicates whether the certificate is the default certificate. Do not set @IsDefault@ when specifying a certificate as an input parameter.
cIsDefault :: Lens' Certificate (Maybe Bool)
cIsDefault = lens _cIsDefault (\ s a -> s{_cIsDefault = a})

instance FromXML Certificate where
        parseXML x
          = Certificate' <$>
              (x .@? "CertificateArn") <*> (x .@? "IsDefault")

instance Hashable Certificate where

instance NFData Certificate where

instance ToQuery Certificate where
        toQuery Certificate'{..}
          = mconcat
              ["CertificateArn" =: _cCertificateARN,
               "IsDefault" =: _cIsDefault]

-- | Information about a cipher used in a policy.
--
--
--
-- /See:/ 'cipher' smart constructor.
data Cipher = Cipher'
  { _cPriority :: !(Maybe Int)
  , _cName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cipher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPriority' - The priority of the cipher.
--
-- * 'cName' - The name of the cipher.
cipher
    :: Cipher
cipher = Cipher' {_cPriority = Nothing, _cName = Nothing}


-- | The priority of the cipher.
cPriority :: Lens' Cipher (Maybe Int)
cPriority = lens _cPriority (\ s a -> s{_cPriority = a})

-- | The name of the cipher.
cName :: Lens' Cipher (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance FromXML Cipher where
        parseXML x
          = Cipher' <$> (x .@? "Priority") <*> (x .@? "Name")

instance Hashable Cipher where

instance NFData Cipher where

-- | Information about an action that returns a custom HTTP response.
--
--
--
-- /See:/ 'fixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { _fracMessageBody :: !(Maybe Text)
  , _fracContentType :: !(Maybe Text)
  , _fracStatusCode  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FixedResponseActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fracMessageBody' - The message.
--
-- * 'fracContentType' - The content type. Valid Values: text/plain | text/css | text/html | application/javascript | application/json
--
-- * 'fracStatusCode' - The HTTP response code (2XX, 4XX, or 5XX).
fixedResponseActionConfig
    :: Text -- ^ 'fracStatusCode'
    -> FixedResponseActionConfig
fixedResponseActionConfig pStatusCode_ =
  FixedResponseActionConfig'
    { _fracMessageBody = Nothing
    , _fracContentType = Nothing
    , _fracStatusCode = pStatusCode_
    }


-- | The message.
fracMessageBody :: Lens' FixedResponseActionConfig (Maybe Text)
fracMessageBody = lens _fracMessageBody (\ s a -> s{_fracMessageBody = a})

-- | The content type. Valid Values: text/plain | text/css | text/html | application/javascript | application/json
fracContentType :: Lens' FixedResponseActionConfig (Maybe Text)
fracContentType = lens _fracContentType (\ s a -> s{_fracContentType = a})

-- | The HTTP response code (2XX, 4XX, or 5XX).
fracStatusCode :: Lens' FixedResponseActionConfig Text
fracStatusCode = lens _fracStatusCode (\ s a -> s{_fracStatusCode = a})

instance FromXML FixedResponseActionConfig where
        parseXML x
          = FixedResponseActionConfig' <$>
              (x .@? "MessageBody") <*> (x .@? "ContentType") <*>
                (x .@ "StatusCode")

instance Hashable FixedResponseActionConfig where

instance NFData FixedResponseActionConfig where

instance ToQuery FixedResponseActionConfig where
        toQuery FixedResponseActionConfig'{..}
          = mconcat
              ["MessageBody" =: _fracMessageBody,
               "ContentType" =: _fracContentType,
               "StatusCode" =: _fracStatusCode]

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
--
--
-- /See:/ 'limit' smart constructor.
data Limit = Limit'
  { _lMax  :: !(Maybe Text)
  , _lName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Limit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMax' - The maximum value of the limit.
--
-- * 'lName' - The name of the limit. The possible values are:     * application-load-balancers     * listeners-per-application-load-balancer     * listeners-per-network-load-balancer     * network-load-balancers     * rules-per-application-load-balancer     * target-groups     * targets-per-application-load-balancer     * targets-per-availability-zone-per-network-load-balancer     * targets-per-network-load-balancer
limit
    :: Limit
limit = Limit' {_lMax = Nothing, _lName = Nothing}


-- | The maximum value of the limit.
lMax :: Lens' Limit (Maybe Text)
lMax = lens _lMax (\ s a -> s{_lMax = a})

-- | The name of the limit. The possible values are:     * application-load-balancers     * listeners-per-application-load-balancer     * listeners-per-network-load-balancer     * network-load-balancers     * rules-per-application-load-balancer     * target-groups     * targets-per-application-load-balancer     * targets-per-availability-zone-per-network-load-balancer     * targets-per-network-load-balancer
lName :: Lens' Limit (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a})

instance FromXML Limit where
        parseXML x
          = Limit' <$> (x .@? "Max") <*> (x .@? "Name")

instance Hashable Limit where

instance NFData Limit where

-- | Information about a listener.
--
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lSSLPolicy       :: !(Maybe Text)
  , _lListenerARN     :: !(Maybe Text)
  , _lProtocol        :: !(Maybe ProtocolEnum)
  , _lDefaultActions  :: !(Maybe [Action])
  , _lCertificates    :: !(Maybe [Certificate])
  , _lLoadBalancerARN :: !(Maybe Text)
  , _lPort            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSSLPolicy' - The security policy that defines which ciphers and protocols are supported. The default is the current predefined security policy.
--
-- * 'lListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'lProtocol' - The protocol for connections from clients to the load balancer.
--
-- * 'lDefaultActions' - The default actions for the listener.
--
-- * 'lCertificates' - The SSL server certificate. You must provide a certificate if the protocol is HTTPS or TLS.
--
-- * 'lLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lPort' - The port on which the load balancer is listening.
listener
    :: Listener
listener =
  Listener'
    { _lSSLPolicy = Nothing
    , _lListenerARN = Nothing
    , _lProtocol = Nothing
    , _lDefaultActions = Nothing
    , _lCertificates = Nothing
    , _lLoadBalancerARN = Nothing
    , _lPort = Nothing
    }


-- | The security policy that defines which ciphers and protocols are supported. The default is the current predefined security policy.
lSSLPolicy :: Lens' Listener (Maybe Text)
lSSLPolicy = lens _lSSLPolicy (\ s a -> s{_lSSLPolicy = a})

-- | The Amazon Resource Name (ARN) of the listener.
lListenerARN :: Lens' Listener (Maybe Text)
lListenerARN = lens _lListenerARN (\ s a -> s{_lListenerARN = a})

-- | The protocol for connections from clients to the load balancer.
lProtocol :: Lens' Listener (Maybe ProtocolEnum)
lProtocol = lens _lProtocol (\ s a -> s{_lProtocol = a})

-- | The default actions for the listener.
lDefaultActions :: Lens' Listener [Action]
lDefaultActions = lens _lDefaultActions (\ s a -> s{_lDefaultActions = a}) . _Default . _Coerce

-- | The SSL server certificate. You must provide a certificate if the protocol is HTTPS or TLS.
lCertificates :: Lens' Listener [Certificate]
lCertificates = lens _lCertificates (\ s a -> s{_lCertificates = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lLoadBalancerARN :: Lens' Listener (Maybe Text)
lLoadBalancerARN = lens _lLoadBalancerARN (\ s a -> s{_lLoadBalancerARN = a})

-- | The port on which the load balancer is listening.
lPort :: Lens' Listener (Maybe Natural)
lPort = lens _lPort (\ s a -> s{_lPort = a}) . mapping _Nat

instance FromXML Listener where
        parseXML x
          = Listener' <$>
              (x .@? "SslPolicy") <*> (x .@? "ListenerArn") <*>
                (x .@? "Protocol")
                <*>
                (x .@? "DefaultActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Certificates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerArn")
                <*> (x .@? "Port")

instance Hashable Listener where

instance NFData Listener where

-- | Information about a load balancer.
--
--
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { _lbState                 :: !(Maybe LoadBalancerState)
  , _lbSecurityGroups        :: !(Maybe [Text])
  , _lbLoadBalancerName      :: !(Maybe Text)
  , _lbCreatedTime           :: !(Maybe ISO8601)
  , _lbVPCId                 :: !(Maybe Text)
  , _lbCanonicalHostedZoneId :: !(Maybe Text)
  , _lbAvailabilityZones     :: !(Maybe [AvailabilityZone])
  , _lbLoadBalancerARN       :: !(Maybe Text)
  , _lbIPAddressType         :: !(Maybe IPAddressType)
  , _lbScheme                :: !(Maybe LoadBalancerSchemeEnum)
  , _lbType                  :: !(Maybe LoadBalancerTypeEnum)
  , _lbDNSName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbState' - The state of the load balancer.
--
-- * 'lbSecurityGroups' - The IDs of the security groups for the load balancer.
--
-- * 'lbLoadBalancerName' - The name of the load balancer.
--
-- * 'lbCreatedTime' - The date and time the load balancer was created.
--
-- * 'lbVPCId' - The ID of the VPC for the load balancer.
--
-- * 'lbCanonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load balancer.
--
-- * 'lbAvailabilityZones' - The Availability Zones for the load balancer.
--
-- * 'lbLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lbIPAddressType' - The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
--
-- * 'lbScheme' - The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can only route requests from clients with access to the VPC for the load balancer.
--
-- * 'lbType' - The type of load balancer.
--
-- * 'lbDNSName' - The public DNS name of the load balancer.
loadBalancer
    :: LoadBalancer
loadBalancer =
  LoadBalancer'
    { _lbState = Nothing
    , _lbSecurityGroups = Nothing
    , _lbLoadBalancerName = Nothing
    , _lbCreatedTime = Nothing
    , _lbVPCId = Nothing
    , _lbCanonicalHostedZoneId = Nothing
    , _lbAvailabilityZones = Nothing
    , _lbLoadBalancerARN = Nothing
    , _lbIPAddressType = Nothing
    , _lbScheme = Nothing
    , _lbType = Nothing
    , _lbDNSName = Nothing
    }


-- | The state of the load balancer.
lbState :: Lens' LoadBalancer (Maybe LoadBalancerState)
lbState = lens _lbState (\ s a -> s{_lbState = a})

-- | The IDs of the security groups for the load balancer.
lbSecurityGroups :: Lens' LoadBalancer [Text]
lbSecurityGroups = lens _lbSecurityGroups (\ s a -> s{_lbSecurityGroups = a}) . _Default . _Coerce

-- | The name of the load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a})

-- | The date and time the load balancer was created.
lbCreatedTime :: Lens' LoadBalancer (Maybe UTCTime)
lbCreatedTime = lens _lbCreatedTime (\ s a -> s{_lbCreatedTime = a}) . mapping _Time

-- | The ID of the VPC for the load balancer.
lbVPCId :: Lens' LoadBalancer (Maybe Text)
lbVPCId = lens _lbVPCId (\ s a -> s{_lbVPCId = a})

-- | The ID of the Amazon Route 53 hosted zone associated with the load balancer.
lbCanonicalHostedZoneId :: Lens' LoadBalancer (Maybe Text)
lbCanonicalHostedZoneId = lens _lbCanonicalHostedZoneId (\ s a -> s{_lbCanonicalHostedZoneId = a})

-- | The Availability Zones for the load balancer.
lbAvailabilityZones :: Lens' LoadBalancer [AvailabilityZone]
lbAvailabilityZones = lens _lbAvailabilityZones (\ s a -> s{_lbAvailabilityZones = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lbLoadBalancerARN :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerARN = lens _lbLoadBalancerARN (\ s a -> s{_lbLoadBalancerARN = a})

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
lbIPAddressType :: Lens' LoadBalancer (Maybe IPAddressType)
lbIPAddressType = lens _lbIPAddressType (\ s a -> s{_lbIPAddressType = a})

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can only route requests from clients with access to the VPC for the load balancer.
lbScheme :: Lens' LoadBalancer (Maybe LoadBalancerSchemeEnum)
lbScheme = lens _lbScheme (\ s a -> s{_lbScheme = a})

-- | The type of load balancer.
lbType :: Lens' LoadBalancer (Maybe LoadBalancerTypeEnum)
lbType = lens _lbType (\ s a -> s{_lbType = a})

-- | The public DNS name of the load balancer.
lbDNSName :: Lens' LoadBalancer (Maybe Text)
lbDNSName = lens _lbDNSName (\ s a -> s{_lbDNSName = a})

instance FromXML LoadBalancer where
        parseXML x
          = LoadBalancer' <$>
              (x .@? "State") <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerName")
                <*> (x .@? "CreatedTime")
                <*> (x .@? "VpcId")
                <*> (x .@? "CanonicalHostedZoneId")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerArn")
                <*> (x .@? "IpAddressType")
                <*> (x .@? "Scheme")
                <*> (x .@? "Type")
                <*> (x .@? "DNSName")

instance Hashable LoadBalancer where

instance NFData LoadBalancer where

-- | Information about a static IP address for a load balancer.
--
--
--
-- /See:/ 'loadBalancerAddress' smart constructor.
data LoadBalancerAddress = LoadBalancerAddress'
  { _lbaIPAddress    :: !(Maybe Text)
  , _lbaAllocationId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaIPAddress' - The static IP address.
--
-- * 'lbaAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address.
loadBalancerAddress
    :: LoadBalancerAddress
loadBalancerAddress =
  LoadBalancerAddress' {_lbaIPAddress = Nothing, _lbaAllocationId = Nothing}


-- | The static IP address.
lbaIPAddress :: Lens' LoadBalancerAddress (Maybe Text)
lbaIPAddress = lens _lbaIPAddress (\ s a -> s{_lbaIPAddress = a})

-- | [Network Load Balancers] The allocation ID of the Elastic IP address.
lbaAllocationId :: Lens' LoadBalancerAddress (Maybe Text)
lbaAllocationId = lens _lbaAllocationId (\ s a -> s{_lbaAllocationId = a})

instance FromXML LoadBalancerAddress where
        parseXML x
          = LoadBalancerAddress' <$>
              (x .@? "IpAddress") <*> (x .@? "AllocationId")

instance Hashable LoadBalancerAddress where

instance NFData LoadBalancerAddress where

-- | Information about a load balancer attribute.
--
--
--
-- /See:/ 'loadBalancerAttribute' smart constructor.
data LoadBalancerAttribute = LoadBalancerAttribute'
  { _lbaValue :: !(Maybe Text)
  , _lbaKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaValue' - The value of the attribute.
--
-- * 'lbaKey' - The name of the attribute. The following attributes are supported by both Application Load Balancers and Network Load Balancers:     * @access_logs.s3.enabled@ - Indicates whether access logs are enabled. The value is @true@ or @false@ . The default is @false@ .     * @access_logs.s3.bucket@ - The name of the S3 bucket for the access logs. This attribute is required if access logs are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permissions to write to the bucket.     * @access_logs.s3.prefix@ - The prefix for the location in the S3 bucket for the access logs.     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ . The default is @false@ . The following attributes are supported by only Application Load Balancers:     * @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds. The valid range is 1-4000 seconds. The default is 60 seconds.     * @routing.http2.enabled@ - Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ . The following attributes are supported by only Network Load Balancers:     * @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .
loadBalancerAttribute
    :: LoadBalancerAttribute
loadBalancerAttribute =
  LoadBalancerAttribute' {_lbaValue = Nothing, _lbaKey = Nothing}


-- | The value of the attribute.
lbaValue :: Lens' LoadBalancerAttribute (Maybe Text)
lbaValue = lens _lbaValue (\ s a -> s{_lbaValue = a})

-- | The name of the attribute. The following attributes are supported by both Application Load Balancers and Network Load Balancers:     * @access_logs.s3.enabled@ - Indicates whether access logs are enabled. The value is @true@ or @false@ . The default is @false@ .     * @access_logs.s3.bucket@ - The name of the S3 bucket for the access logs. This attribute is required if access logs are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permissions to write to the bucket.     * @access_logs.s3.prefix@ - The prefix for the location in the S3 bucket for the access logs.     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ . The default is @false@ . The following attributes are supported by only Application Load Balancers:     * @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds. The valid range is 1-4000 seconds. The default is 60 seconds.     * @routing.http2.enabled@ - Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ . The following attributes are supported by only Network Load Balancers:     * @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .
lbaKey :: Lens' LoadBalancerAttribute (Maybe Text)
lbaKey = lens _lbaKey (\ s a -> s{_lbaKey = a})

instance FromXML LoadBalancerAttribute where
        parseXML x
          = LoadBalancerAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance Hashable LoadBalancerAttribute where

instance NFData LoadBalancerAttribute where

instance ToQuery LoadBalancerAttribute where
        toQuery LoadBalancerAttribute'{..}
          = mconcat ["Value" =: _lbaValue, "Key" =: _lbaKey]

-- | Information about the state of the load balancer.
--
--
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { _lbsReason :: !(Maybe Text)
  , _lbsCode   :: !(Maybe LoadBalancerStateEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsReason' - A description of the state.
--
-- * 'lbsCode' - The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
loadBalancerState
    :: LoadBalancerState
loadBalancerState =
  LoadBalancerState' {_lbsReason = Nothing, _lbsCode = Nothing}


-- | A description of the state.
lbsReason :: Lens' LoadBalancerState (Maybe Text)
lbsReason = lens _lbsReason (\ s a -> s{_lbsReason = a})

-- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
lbsCode :: Lens' LoadBalancerState (Maybe LoadBalancerStateEnum)
lbsCode = lens _lbsCode (\ s a -> s{_lbsCode = a})

instance FromXML LoadBalancerState where
        parseXML x
          = LoadBalancerState' <$>
              (x .@? "Reason") <*> (x .@? "Code")

instance Hashable LoadBalancerState where

instance NFData LoadBalancerState where

-- | Information to use when checking for a successful response from a target.
--
--
--
-- /See:/ 'matcher' smart constructor.
newtype Matcher = Matcher'
  { _mHTTPCode :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Matcher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mHTTPCode' - The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200
