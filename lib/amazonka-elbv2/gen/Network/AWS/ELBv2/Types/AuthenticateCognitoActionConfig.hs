{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig where

import Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Request parameters to use when integrating with Amazon Cognito to authenticate users.
--
--
--
-- /See:/ 'authenticateCognitoActionConfig' smart constructor.
data AuthenticateCognitoActionConfig = AuthenticateCognitoActionConfig'
  { _acacAuthenticationRequestExtraParams ::
      !(Maybe (Map Text (Text))),
    _acacScope :: !(Maybe Text),
    _acacOnUnauthenticatedRequest ::
      !( Maybe
           AuthenticateCognitoActionConditionalBehaviorEnum
       ),
    _acacSessionCookieName ::
      !(Maybe Text),
    _acacSessionTimeout ::
      !(Maybe Integer),
    _acacUserPoolARN :: !Text,
    _acacUserPoolClientId ::
      !Text,
    _acacUserPoolDomain ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
authenticateCognitoActionConfig ::
  -- | 'acacUserPoolARN'
  Text ->
  -- | 'acacUserPoolClientId'
  Text ->
  -- | 'acacUserPoolDomain'
  Text ->
  AuthenticateCognitoActionConfig
authenticateCognitoActionConfig
  pUserPoolARN_
  pUserPoolClientId_
  pUserPoolDomain_ =
    AuthenticateCognitoActionConfig'
      { _acacAuthenticationRequestExtraParams =
          Nothing,
        _acacScope = Nothing,
        _acacOnUnauthenticatedRequest = Nothing,
        _acacSessionCookieName = Nothing,
        _acacSessionTimeout = Nothing,
        _acacUserPoolARN = pUserPoolARN_,
        _acacUserPoolClientId = pUserPoolClientId_,
        _acacUserPoolDomain = pUserPoolDomain_
      }

-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
acacAuthenticationRequestExtraParams :: Lens' AuthenticateCognitoActionConfig (HashMap Text (Text))
acacAuthenticationRequestExtraParams = lens _acacAuthenticationRequestExtraParams (\s a -> s {_acacAuthenticationRequestExtraParams = a}) . _Default . _Map

-- | The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
acacScope :: Lens' AuthenticateCognitoActionConfig (Maybe Text)
acacScope = lens _acacScope (\s a -> s {_acacScope = a})

-- | The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
acacOnUnauthenticatedRequest :: Lens' AuthenticateCognitoActionConfig (Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
acacOnUnauthenticatedRequest = lens _acacOnUnauthenticatedRequest (\s a -> s {_acacOnUnauthenticatedRequest = a})

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
acacSessionCookieName :: Lens' AuthenticateCognitoActionConfig (Maybe Text)
acacSessionCookieName = lens _acacSessionCookieName (\s a -> s {_acacSessionCookieName = a})

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
acacSessionTimeout :: Lens' AuthenticateCognitoActionConfig (Maybe Integer)
acacSessionTimeout = lens _acacSessionTimeout (\s a -> s {_acacSessionTimeout = a})

-- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
acacUserPoolARN :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolARN = lens _acacUserPoolARN (\s a -> s {_acacUserPoolARN = a})

-- | The ID of the Amazon Cognito user pool client.
acacUserPoolClientId :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolClientId = lens _acacUserPoolClientId (\s a -> s {_acacUserPoolClientId = a})

-- | The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
acacUserPoolDomain :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolDomain = lens _acacUserPoolDomain (\s a -> s {_acacUserPoolDomain = a})

instance FromXML AuthenticateCognitoActionConfig where
  parseXML x =
    AuthenticateCognitoActionConfig'
      <$> ( x .@? "AuthenticationRequestExtraParams" .!@ mempty
              >>= may (parseXMLMap "entry" "key" "value")
          )
      <*> (x .@? "Scope")
      <*> (x .@? "OnUnauthenticatedRequest")
      <*> (x .@? "SessionCookieName")
      <*> (x .@? "SessionTimeout")
      <*> (x .@ "UserPoolArn")
      <*> (x .@ "UserPoolClientId")
      <*> (x .@ "UserPoolDomain")

instance Hashable AuthenticateCognitoActionConfig

instance NFData AuthenticateCognitoActionConfig

instance ToQuery AuthenticateCognitoActionConfig where
  toQuery AuthenticateCognitoActionConfig' {..} =
    mconcat
      [ "AuthenticationRequestExtraParams"
          =: toQuery
            ( toQueryMap "entry" "key" "value"
                <$> _acacAuthenticationRequestExtraParams
            ),
        "Scope" =: _acacScope,
        "OnUnauthenticatedRequest" =: _acacOnUnauthenticatedRequest,
        "SessionCookieName" =: _acacSessionCookieName,
        "SessionTimeout" =: _acacSessionTimeout,
        "UserPoolArn" =: _acacUserPoolARN,
        "UserPoolClientId" =: _acacUserPoolClientId,
        "UserPoolDomain" =: _acacUserPoolDomain
      ]
