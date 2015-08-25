{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateOpenIdConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM entity to describe an identity provider (IdP) that
-- supports <http://openid.net/connect/ OpenID Connect (OIDC)>.
--
-- The OIDC provider that you create with this operation can be used as a
-- principal in a role\'s trust policy to establish a trust relationship
-- between AWS and the OIDC provider.
--
-- When you create the IAM OIDC provider, you specify the URL of the OIDC
-- identity provider (IdP) to trust, a list of client IDs (also known as
-- audiences) that identify the application or applications that are
-- allowed to authenticate using the OIDC provider, and a list of
-- thumbprints of the server certificate(s) that the IdP uses. You get all
-- of this information from the OIDC IdP that you want to use for access to
-- AWS.
--
-- Because trust for the OIDC provider is ultimately derived from the IAM
-- provider that this action creates, it is a best practice to limit access
-- to the CreateOpenIDConnectProvider action to highly-privileged users.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateOpenIdConnectProvider.html AWS API Reference> for CreateOpenIdConnectProvider.
module Network.AWS.IAM.CreateOpenIdConnectProvider
    (
    -- * Creating a Request
      createOpenIdConnectProvider
    , CreateOpenIdConnectProvider
    -- * Request Lenses
    , coicpClientIdList
    , coicpURL
    , coicpThumbprintList

    -- * Destructuring the Response
    , createOpenIdConnectProviderResponse
    , CreateOpenIdConnectProviderResponse
    -- * Response Lenses
    , coicprsOpenIdConnectProviderARN
    , coicprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createOpenIdConnectProvider' smart constructor.
data CreateOpenIdConnectProvider = CreateOpenIdConnectProvider'
    { _coicpClientIdList   :: !(Maybe [Text])
    , _coicpURL            :: !Text
    , _coicpThumbprintList :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coicpClientIdList'
--
-- * 'coicpURL'
--
-- * 'coicpThumbprintList'
createOpenIdConnectProvider
    :: Text -- ^ 'coicpURL'
    -> CreateOpenIdConnectProvider
createOpenIdConnectProvider pURL_ =
    CreateOpenIdConnectProvider'
    { _coicpClientIdList = Nothing
    , _coicpURL = pURL_
    , _coicpThumbprintList = mempty
    }

-- | A list of client IDs (also known as audiences). When a mobile or web app
-- registers with an OpenID Connect provider, they establish a value that
-- identifies the application. (This is the value that\'s sent as the
-- 'client_id' parameter on OAuth requests.)
--
-- You can register multiple client IDs with the same provider. For
-- example, you might have multiple applications that use the same OIDC
-- provider. You cannot register more than 100 client IDs with a single IAM
-- OIDC provider.
--
-- There is no defined format for a client ID. The
-- 'CreateOpenIDConnectProviderRequest' action accepts client IDs up to 255
-- characters long.
coicpClientIdList :: Lens' CreateOpenIdConnectProvider [Text]
coicpClientIdList = lens _coicpClientIdList (\ s a -> s{_coicpClientIdList = a}) . _Default . _Coerce;

-- | The URL of the identity provider. The URL must begin with \"https:\/\/\"
-- and should correspond to the 'iss' claim in the provider\'s OpenID
-- Connect ID tokens. Per the OIDC standard, path components are allowed
-- but query parameters are not. Typically the URL consists of only a host
-- name, like \"https:\/\/server.example.org\" or
-- \"https:\/\/example.com\".
--
-- You cannot register the same provider multiple times in a single AWS
-- account. If you try to submit a URL that has already been used for an
-- OpenID Connect provider in the AWS account, you will get an error.
coicpURL :: Lens' CreateOpenIdConnectProvider Text
coicpURL = lens _coicpURL (\ s a -> s{_coicpURL = a});

-- | A list of server certificate thumbprints for the OpenID Connect (OIDC)
-- identity provider\'s server certificate(s). Typically this list includes
-- only one entry. However, IAM lets you have up to five thumbprints for an
-- OIDC provider. This lets you maintain multiple thumbprints if the
-- identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of
-- the X.509 certificate used by the domain where the OpenID Connect
-- provider makes its keys available. It is always a 40-character string.
--
-- You must provide at least one thumbprint when creating an IAM OIDC
-- provider. For example, if the OIDC provider is 'server.example.com' and
-- the provider stores its keys at
-- \"https:\/\/keys.server.example.com\/openid-connect\", the thumbprint
-- string would be the hex-encoded SHA-1 hash value of the certificate used
-- by https:\/\/keys.server.example.com.
--
-- For more information about obtaining the OIDC provider\'s thumbprint,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the Thumbprint for an OpenID Connect Provider>
-- in the /Using IAM/ guide.
coicpThumbprintList :: Lens' CreateOpenIdConnectProvider [Text]
coicpThumbprintList = lens _coicpThumbprintList (\ s a -> s{_coicpThumbprintList = a}) . _Coerce;

instance AWSRequest CreateOpenIdConnectProvider where
        type Rs CreateOpenIdConnectProvider =
             CreateOpenIdConnectProviderResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper
              "CreateOpenIDConnectProviderResult"
              (\ s h x ->
                 CreateOpenIdConnectProviderResponse' <$>
                   (x .@? "OpenIDConnectProviderArn") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateOpenIdConnectProvider where
        toHeaders = const mempty

instance ToPath CreateOpenIdConnectProvider where
        toPath = const "/"

instance ToQuery CreateOpenIdConnectProvider where
        toQuery CreateOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("CreateOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ClientIDList" =:
                 toQuery
                   (toQueryList "member" <$> _coicpClientIdList),
               "Url" =: _coicpURL,
               "ThumbprintList" =:
                 toQueryList "member" _coicpThumbprintList]

-- | Contains the response to a successful CreateOpenIDConnectProvider
-- request.
--
-- /See:/ 'createOpenIdConnectProviderResponse' smart constructor.
data CreateOpenIdConnectProviderResponse = CreateOpenIdConnectProviderResponse'
    { _coicprsOpenIdConnectProviderARN :: !(Maybe Text)
    , _coicprsStatus                   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coicprsOpenIdConnectProviderARN'
--
-- * 'coicprsStatus'
createOpenIdConnectProviderResponse
    :: Int -- ^ 'coicprsStatus'
    -> CreateOpenIdConnectProviderResponse
createOpenIdConnectProviderResponse pStatus_ =
    CreateOpenIdConnectProviderResponse'
    { _coicprsOpenIdConnectProviderARN = Nothing
    , _coicprsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect provider that
-- was created. For more information, see OpenIDConnectProviderListEntry.
coicprsOpenIdConnectProviderARN :: Lens' CreateOpenIdConnectProviderResponse (Maybe Text)
coicprsOpenIdConnectProviderARN = lens _coicprsOpenIdConnectProviderARN (\ s a -> s{_coicprsOpenIdConnectProviderARN = a});

-- | The response status code.
coicprsStatus :: Lens' CreateOpenIdConnectProviderResponse Int
coicprsStatus = lens _coicprsStatus (\ s a -> s{_coicprsStatus = a});
