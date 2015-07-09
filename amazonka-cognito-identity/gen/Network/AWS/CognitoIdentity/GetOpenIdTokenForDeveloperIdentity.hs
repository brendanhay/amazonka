{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers (or retrieves) a Cognito @IdentityId@ and an OpenID Connect
-- token for a user authenticated by your backend authentication process.
-- Supplying multiple logins will create an implicit linked account. You
-- can only specify one developer provider as part of the @Logins@ map,
-- which is linked to the identity pool. The developer provider is the
-- \"domain\" by which Cognito will refer to your users.
--
-- You can use @GetOpenIdTokenForDeveloperIdentity@ to create a new
-- identity and to link new logins (that is, user credentials issued by a
-- public provider or developer provider) to an existing identity. When you
-- want to create a new identity, the @IdentityId@ should be null. When you
-- want to associate a new login with an existing
-- authenticated\/unauthenticated identity, you can do so by providing the
-- existing @IdentityId@. This API will create the identity in the
-- specified @IdentityPoolId@.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetOpenIdTokenForDeveloperIdentity.html>
module Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
    (
    -- * Request
      GetOpenIdTokenForDeveloperIdentity
    -- ** Request constructor
    , getOpenIdTokenForDeveloperIdentity
    -- ** Request lenses
    , goitfdiTokenDuration
    , goitfdiIdentityId
    , goitfdiIdentityPoolId
    , goitfdiLogins

    -- * Response
    , GetOpenIdTokenForDeveloperIdentityResponse
    -- ** Response constructor
    , getOpenIdTokenForDeveloperIdentityResponse
    -- ** Response lenses
    , goitfdirToken
    , goitfdirIdentityId
    , goitfdirStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @GetOpenIdTokenForDeveloperIdentity@ action.
--
-- /See:/ 'getOpenIdTokenForDeveloperIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitfdiTokenDuration'
--
-- * 'goitfdiIdentityId'
--
-- * 'goitfdiIdentityPoolId'
--
-- * 'goitfdiLogins'
data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity'
    { _goitfdiTokenDuration  :: !(Maybe Nat)
    , _goitfdiIdentityId     :: !(Maybe Text)
    , _goitfdiIdentityPoolId :: !Text
    , _goitfdiLogins         :: !(Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdTokenForDeveloperIdentity' smart constructor.
getOpenIdTokenForDeveloperIdentity :: Text -> GetOpenIdTokenForDeveloperIdentity
getOpenIdTokenForDeveloperIdentity pIdentityPoolId =
    GetOpenIdTokenForDeveloperIdentity'
    { _goitfdiTokenDuration = Nothing
    , _goitfdiIdentityId = Nothing
    , _goitfdiIdentityPoolId = pIdentityPoolId
    , _goitfdiLogins = mempty
    }

-- | The expiration time of the token, in seconds. You can specify a custom
-- expiration time for the token so that you can cache it. If you don\'t
-- provide an expiration time, the token is valid for 15 minutes. You can
-- exchange the token with Amazon STS for temporary AWS credentials, which
-- are valid for a maximum of one hour. The maximum token duration you can
-- set is 24 hours. You should take care in setting the expiration time for
-- a token, as there are significant security implications: an attacker
-- could use a leaked token to access your AWS resources for the token\'s
-- duration.
goitfdiTokenDuration :: Lens' GetOpenIdTokenForDeveloperIdentity (Maybe Natural)
goitfdiTokenDuration = lens _goitfdiTokenDuration (\ s a -> s{_goitfdiTokenDuration = a}) . mapping _Nat;

-- | A unique identifier in the format REGION:GUID.
goitfdiIdentityId :: Lens' GetOpenIdTokenForDeveloperIdentity (Maybe Text)
goitfdiIdentityId = lens _goitfdiIdentityId (\ s a -> s{_goitfdiIdentityId = a});

-- | An identity pool ID in the format REGION:GUID.
goitfdiIdentityPoolId :: Lens' GetOpenIdTokenForDeveloperIdentity Text
goitfdiIdentityPoolId = lens _goitfdiIdentityPoolId (\ s a -> s{_goitfdiIdentityPoolId = a});

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. Each name-value pair represents a user from a public provider or
-- developer provider. If the user is from a developer provider, the
-- name-value pair will follow the syntax
-- @\"developer_provider_name\": \"developer_user_identifier\"@. The
-- developer provider is the \"domain\" by which Cognito will refer to your
-- users; you provided this domain while creating\/updating the identity
-- pool. The developer user identifier is an identifier from your backend
-- that uniquely identifies a user. When you create an identity pool, you
-- can specify the supported logins.
goitfdiLogins :: Lens' GetOpenIdTokenForDeveloperIdentity (HashMap Text Text)
goitfdiLogins = lens _goitfdiLogins (\ s a -> s{_goitfdiLogins = a}) . _Map;

instance AWSRequest
         GetOpenIdTokenForDeveloperIdentity where
        type Sv GetOpenIdTokenForDeveloperIdentity =
             CognitoIdentity
        type Rs GetOpenIdTokenForDeveloperIdentity =
             GetOpenIdTokenForDeveloperIdentityResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetOpenIdTokenForDeveloperIdentityResponse' <$>
                   (x .?> "Token") <*> (x .?> "IdentityId") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetOpenIdTokenForDeveloperIdentity
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetOpenIdTokenForDeveloperIdentity"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOpenIdTokenForDeveloperIdentity
         where
        toJSON GetOpenIdTokenForDeveloperIdentity'{..}
          = object
              ["TokenDuration" .= _goitfdiTokenDuration,
               "IdentityId" .= _goitfdiIdentityId,
               "IdentityPoolId" .= _goitfdiIdentityPoolId,
               "Logins" .= _goitfdiLogins]

instance ToPath GetOpenIdTokenForDeveloperIdentity
         where
        toPath = const "/"

instance ToQuery GetOpenIdTokenForDeveloperIdentity
         where
        toQuery = const mempty

-- | Returned in response to a successful
-- @GetOpenIdTokenForDeveloperIdentity@ request.
--
-- /See:/ 'getOpenIdTokenForDeveloperIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitfdirToken'
--
-- * 'goitfdirIdentityId'
--
-- * 'goitfdirStatus'
data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse'
    { _goitfdirToken      :: !(Maybe Text)
    , _goitfdirIdentityId :: !(Maybe Text)
    , _goitfdirStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdTokenForDeveloperIdentityResponse' smart constructor.
getOpenIdTokenForDeveloperIdentityResponse :: Int -> GetOpenIdTokenForDeveloperIdentityResponse
getOpenIdTokenForDeveloperIdentityResponse pStatus =
    GetOpenIdTokenForDeveloperIdentityResponse'
    { _goitfdirToken = Nothing
    , _goitfdirIdentityId = Nothing
    , _goitfdirStatus = pStatus
    }

-- | An OpenID token.
goitfdirToken :: Lens' GetOpenIdTokenForDeveloperIdentityResponse (Maybe Text)
goitfdirToken = lens _goitfdirToken (\ s a -> s{_goitfdirToken = a});

-- | A unique identifier in the format REGION:GUID.
goitfdirIdentityId :: Lens' GetOpenIdTokenForDeveloperIdentityResponse (Maybe Text)
goitfdirIdentityId = lens _goitfdirIdentityId (\ s a -> s{_goitfdirIdentityId = a});

-- | FIXME: Undocumented member.
goitfdirStatus :: Lens' GetOpenIdTokenForDeveloperIdentityResponse Int
goitfdirStatus = lens _goitfdirStatus (\ s a -> s{_goitfdirStatus = a});
