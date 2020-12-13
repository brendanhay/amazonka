{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateOpenIdConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM entity to describe an identity provider (IdP) that supports <http://openid.net/connect/ OpenID Connect (OIDC)> .
--
-- The OIDC provider that you create with this operation can be used as a principal in a role's trust policy. Such a policy establishes a trust relationship between AWS and the OIDC provider.
-- When you create the IAM OIDC provider, you specify the following:
--
--     * The URL of the OIDC identity provider (IdP) to trust
--
--
--     * A list of client IDs (also known as audiences) that identify the application or applications that are allowed to authenticate using the OIDC provider
--
--
--     * A list of thumbprints of one or more server certificates that the IdP uses
--
--
-- You get all of this information from the OIDC IdP that you want to use to access AWS.
module Network.AWS.IAM.CreateOpenIdConnectProvider
  ( -- * Creating a request
    CreateOpenIdConnectProvider (..),
    mkCreateOpenIdConnectProvider,

    -- ** Request lenses
    coicpURL,
    coicpThumbprintList,
    coicpClientIdList,

    -- * Destructuring the response
    CreateOpenIdConnectProviderResponse (..),
    mkCreateOpenIdConnectProviderResponse,

    -- ** Response lenses
    coicprsOpenIdConnectProviderARN,
    coicprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateOpenIdConnectProvider' smart constructor.
data CreateOpenIdConnectProvider = CreateOpenIdConnectProvider'
  { -- | The URL of the identity provider. The URL must begin with @https://@ and should correspond to the @iss@ claim in the provider's OpenID Connect ID tokens. Per the OIDC standard, path components are allowed but query parameters are not. Typically the URL consists of only a hostname, like @https://server.example.org@ or @https://example.com@ .
    --
    -- You cannot register the same provider multiple times in a single AWS account. If you try to submit a URL that has already been used for an OpenID Connect provider in the AWS account, you will get an error.
    url :: Lude.Text,
    -- | A list of server certificate thumbprints for the OpenID Connect (OIDC) identity provider's server certificates. Typically this list includes only one entry. However, IAM lets you have up to five thumbprints for an OIDC provider. This lets you maintain multiple thumbprints if the identity provider is rotating certificates.
    --
    -- The server certificate thumbprint is the hex-encoded SHA-1 hash value of the X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.
    -- You must provide at least one thumbprint when creating an IAM OIDC provider. For example, assume that the OIDC provider is @server.example.com@ and the provider stores its keys at https://keys.server.example.com/openid-connect. In that case, the thumbprint string would be the hex-encoded SHA-1 hash value of the certificate used by https://keys.server.example.com.
    -- For more information about obtaining the OIDC provider's thumbprint, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the Thumbprint for an OpenID Connect Provider> in the /IAM User Guide/ .
    thumbprintList :: [Lude.Text],
    -- | A list of client IDs (also known as audiences). When a mobile or web app registers with an OpenID Connect provider, they establish a value that identifies the application. (This is the value that's sent as the @client_id@ parameter on OAuth requests.)
    --
    -- You can register multiple client IDs with the same provider. For example, you might have multiple applications that use the same OIDC provider. You cannot register more than 100 client IDs with a single IAM OIDC provider.
    -- There is no defined format for a client ID. The @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to 255 characters long.
    clientIdList :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- * 'url' - The URL of the identity provider. The URL must begin with @https://@ and should correspond to the @iss@ claim in the provider's OpenID Connect ID tokens. Per the OIDC standard, path components are allowed but query parameters are not. Typically the URL consists of only a hostname, like @https://server.example.org@ or @https://example.com@ .
--
-- You cannot register the same provider multiple times in a single AWS account. If you try to submit a URL that has already been used for an OpenID Connect provider in the AWS account, you will get an error.
-- * 'thumbprintList' - A list of server certificate thumbprints for the OpenID Connect (OIDC) identity provider's server certificates. Typically this list includes only one entry. However, IAM lets you have up to five thumbprints for an OIDC provider. This lets you maintain multiple thumbprints if the identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of the X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.
-- You must provide at least one thumbprint when creating an IAM OIDC provider. For example, assume that the OIDC provider is @server.example.com@ and the provider stores its keys at https://keys.server.example.com/openid-connect. In that case, the thumbprint string would be the hex-encoded SHA-1 hash value of the certificate used by https://keys.server.example.com.
-- For more information about obtaining the OIDC provider's thumbprint, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the Thumbprint for an OpenID Connect Provider> in the /IAM User Guide/ .
-- * 'clientIdList' - A list of client IDs (also known as audiences). When a mobile or web app registers with an OpenID Connect provider, they establish a value that identifies the application. (This is the value that's sent as the @client_id@ parameter on OAuth requests.)
--
-- You can register multiple client IDs with the same provider. For example, you might have multiple applications that use the same OIDC provider. You cannot register more than 100 client IDs with a single IAM OIDC provider.
-- There is no defined format for a client ID. The @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to 255 characters long.
mkCreateOpenIdConnectProvider ::
  -- | 'url'
  Lude.Text ->
  CreateOpenIdConnectProvider
mkCreateOpenIdConnectProvider pURL_ =
  CreateOpenIdConnectProvider'
    { url = pURL_,
      thumbprintList = Lude.mempty,
      clientIdList = Lude.Nothing
    }

-- | The URL of the identity provider. The URL must begin with @https://@ and should correspond to the @iss@ claim in the provider's OpenID Connect ID tokens. Per the OIDC standard, path components are allowed but query parameters are not. Typically the URL consists of only a hostname, like @https://server.example.org@ or @https://example.com@ .
--
-- You cannot register the same provider multiple times in a single AWS account. If you try to submit a URL that has already been used for an OpenID Connect provider in the AWS account, you will get an error.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coicpURL :: Lens.Lens' CreateOpenIdConnectProvider Lude.Text
coicpURL = Lens.lens (url :: CreateOpenIdConnectProvider -> Lude.Text) (\s a -> s {url = a} :: CreateOpenIdConnectProvider)
{-# DEPRECATED coicpURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | A list of server certificate thumbprints for the OpenID Connect (OIDC) identity provider's server certificates. Typically this list includes only one entry. However, IAM lets you have up to five thumbprints for an OIDC provider. This lets you maintain multiple thumbprints if the identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of the X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.
-- You must provide at least one thumbprint when creating an IAM OIDC provider. For example, assume that the OIDC provider is @server.example.com@ and the provider stores its keys at https://keys.server.example.com/openid-connect. In that case, the thumbprint string would be the hex-encoded SHA-1 hash value of the certificate used by https://keys.server.example.com.
-- For more information about obtaining the OIDC provider's thumbprint, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the Thumbprint for an OpenID Connect Provider> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'thumbprintList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coicpThumbprintList :: Lens.Lens' CreateOpenIdConnectProvider [Lude.Text]
coicpThumbprintList = Lens.lens (thumbprintList :: CreateOpenIdConnectProvider -> [Lude.Text]) (\s a -> s {thumbprintList = a} :: CreateOpenIdConnectProvider)
{-# DEPRECATED coicpThumbprintList "Use generic-lens or generic-optics with 'thumbprintList' instead." #-}

-- | A list of client IDs (also known as audiences). When a mobile or web app registers with an OpenID Connect provider, they establish a value that identifies the application. (This is the value that's sent as the @client_id@ parameter on OAuth requests.)
--
-- You can register multiple client IDs with the same provider. For example, you might have multiple applications that use the same OIDC provider. You cannot register more than 100 client IDs with a single IAM OIDC provider.
-- There is no defined format for a client ID. The @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to 255 characters long.
--
-- /Note:/ Consider using 'clientIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coicpClientIdList :: Lens.Lens' CreateOpenIdConnectProvider (Lude.Maybe [Lude.Text])
coicpClientIdList = Lens.lens (clientIdList :: CreateOpenIdConnectProvider -> Lude.Maybe [Lude.Text]) (\s a -> s {clientIdList = a} :: CreateOpenIdConnectProvider)
{-# DEPRECATED coicpClientIdList "Use generic-lens or generic-optics with 'clientIdList' instead." #-}

instance Lude.AWSRequest CreateOpenIdConnectProvider where
  type
    Rs CreateOpenIdConnectProvider =
      CreateOpenIdConnectProviderResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateOpenIDConnectProviderResult"
      ( \s h x ->
          CreateOpenIdConnectProviderResponse'
            Lude.<$> (x Lude..@? "OpenIDConnectProviderArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOpenIdConnectProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateOpenIdConnectProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOpenIdConnectProvider where
  toQuery CreateOpenIdConnectProvider' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateOpenIDConnectProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Url" Lude.=: url,
        "ThumbprintList" Lude.=: Lude.toQueryList "member" thumbprintList,
        "ClientIDList"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> clientIdList)
      ]

-- | Contains the response to a successful 'CreateOpenIDConnectProvider' request.
--
-- /See:/ 'mkCreateOpenIdConnectProviderResponse' smart constructor.
data CreateOpenIdConnectProviderResponse = CreateOpenIdConnectProviderResponse'
  { -- | The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider that is created. For more information, see 'OpenIDConnectProviderListEntry' .
    openIdConnectProviderARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
-- * 'openIdConnectProviderARN' - The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider that is created. For more information, see 'OpenIDConnectProviderListEntry' .
-- * 'responseStatus' - The response status code.
mkCreateOpenIdConnectProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOpenIdConnectProviderResponse
mkCreateOpenIdConnectProviderResponse pResponseStatus_ =
  CreateOpenIdConnectProviderResponse'
    { openIdConnectProviderARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider that is created. For more information, see 'OpenIDConnectProviderListEntry' .
--
-- /Note:/ Consider using 'openIdConnectProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coicprsOpenIdConnectProviderARN :: Lens.Lens' CreateOpenIdConnectProviderResponse (Lude.Maybe Lude.Text)
coicprsOpenIdConnectProviderARN = Lens.lens (openIdConnectProviderARN :: CreateOpenIdConnectProviderResponse -> Lude.Maybe Lude.Text) (\s a -> s {openIdConnectProviderARN = a} :: CreateOpenIdConnectProviderResponse)
{-# DEPRECATED coicprsOpenIdConnectProviderARN "Use generic-lens or generic-optics with 'openIdConnectProviderARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coicprsResponseStatus :: Lens.Lens' CreateOpenIdConnectProviderResponse Lude.Int
coicprsResponseStatus = Lens.lens (responseStatus :: CreateOpenIdConnectProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOpenIdConnectProviderResponse)
{-# DEPRECATED coicprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
