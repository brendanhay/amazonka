{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateOpenIDConnectProvider
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
module Network.AWS.IAM.CreateOpenIDConnectProvider
    (
    -- * Creating a request
      CreateOpenIDConnectProvider (..)
    , mkCreateOpenIDConnectProvider
    -- ** Request lenses
    , coidcpUrl
    , coidcpThumbprintList
    , coidcpClientIDList

    -- * Destructuring the response
    , CreateOpenIDConnectProviderResponse (..)
    , mkCreateOpenIDConnectProviderResponse
    -- ** Response lenses
    , coidcprrsOpenIDConnectProviderArn
    , coidcprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateOpenIDConnectProvider' smart constructor.
data CreateOpenIDConnectProvider = CreateOpenIDConnectProvider'
  { url :: Types.OpenIDConnectProviderUrlType
    -- ^ The URL of the identity provider. The URL must begin with @https://@ and should correspond to the @iss@ claim in the provider's OpenID Connect ID tokens. Per the OIDC standard, path components are allowed but query parameters are not. Typically the URL consists of only a hostname, like @https://server.example.org@ or @https://example.com@ .
--
-- You cannot register the same provider multiple times in a single AWS account. If you try to submit a URL that has already been used for an OpenID Connect provider in the AWS account, you will get an error.
  , thumbprintList :: [Types.ThumbprintType]
    -- ^ A list of server certificate thumbprints for the OpenID Connect (OIDC) identity provider's server certificates. Typically this list includes only one entry. However, IAM lets you have up to five thumbprints for an OIDC provider. This lets you maintain multiple thumbprints if the identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of the X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.
-- You must provide at least one thumbprint when creating an IAM OIDC provider. For example, assume that the OIDC provider is @server.example.com@ and the provider stores its keys at https://keys.server.example.com/openid-connect. In that case, the thumbprint string would be the hex-encoded SHA-1 hash value of the certificate used by https://keys.server.example.com.
-- For more information about obtaining the OIDC provider's thumbprint, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the Thumbprint for an OpenID Connect Provider> in the /IAM User Guide/ .
  , clientIDList :: Core.Maybe [Types.ClientIDType]
    -- ^ A list of client IDs (also known as audiences). When a mobile or web app registers with an OpenID Connect provider, they establish a value that identifies the application. (This is the value that's sent as the @client_id@ parameter on OAuth requests.)
--
-- You can register multiple client IDs with the same provider. For example, you might have multiple applications that use the same OIDC provider. You cannot register more than 100 client IDs with a single IAM OIDC provider.
-- There is no defined format for a client ID. The @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to 255 characters long.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOpenIDConnectProvider' value with any optional fields omitted.
mkCreateOpenIDConnectProvider
    :: Types.OpenIDConnectProviderUrlType -- ^ 'url'
    -> CreateOpenIDConnectProvider
mkCreateOpenIDConnectProvider url
  = CreateOpenIDConnectProvider'{url, thumbprintList = Core.mempty,
                                 clientIDList = Core.Nothing}

-- | The URL of the identity provider. The URL must begin with @https://@ and should correspond to the @iss@ claim in the provider's OpenID Connect ID tokens. Per the OIDC standard, path components are allowed but query parameters are not. Typically the URL consists of only a hostname, like @https://server.example.org@ or @https://example.com@ .
--
-- You cannot register the same provider multiple times in a single AWS account. If you try to submit a URL that has already been used for an OpenID Connect provider in the AWS account, you will get an error.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coidcpUrl :: Lens.Lens' CreateOpenIDConnectProvider Types.OpenIDConnectProviderUrlType
coidcpUrl = Lens.field @"url"
{-# INLINEABLE coidcpUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | A list of server certificate thumbprints for the OpenID Connect (OIDC) identity provider's server certificates. Typically this list includes only one entry. However, IAM lets you have up to five thumbprints for an OIDC provider. This lets you maintain multiple thumbprints if the identity provider is rotating certificates.
--
-- The server certificate thumbprint is the hex-encoded SHA-1 hash value of the X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.
-- You must provide at least one thumbprint when creating an IAM OIDC provider. For example, assume that the OIDC provider is @server.example.com@ and the provider stores its keys at https://keys.server.example.com/openid-connect. In that case, the thumbprint string would be the hex-encoded SHA-1 hash value of the certificate used by https://keys.server.example.com.
-- For more information about obtaining the OIDC provider's thumbprint, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/identity-providers-oidc-obtain-thumbprint.html Obtaining the Thumbprint for an OpenID Connect Provider> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'thumbprintList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coidcpThumbprintList :: Lens.Lens' CreateOpenIDConnectProvider [Types.ThumbprintType]
coidcpThumbprintList = Lens.field @"thumbprintList"
{-# INLINEABLE coidcpThumbprintList #-}
{-# DEPRECATED thumbprintList "Use generic-lens or generic-optics with 'thumbprintList' instead"  #-}

-- | A list of client IDs (also known as audiences). When a mobile or web app registers with an OpenID Connect provider, they establish a value that identifies the application. (This is the value that's sent as the @client_id@ parameter on OAuth requests.)
--
-- You can register multiple client IDs with the same provider. For example, you might have multiple applications that use the same OIDC provider. You cannot register more than 100 client IDs with a single IAM OIDC provider.
-- There is no defined format for a client ID. The @CreateOpenIDConnectProviderRequest@ operation accepts client IDs up to 255 characters long.
--
-- /Note:/ Consider using 'clientIDList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coidcpClientIDList :: Lens.Lens' CreateOpenIDConnectProvider (Core.Maybe [Types.ClientIDType])
coidcpClientIDList = Lens.field @"clientIDList"
{-# INLINEABLE coidcpClientIDList #-}
{-# DEPRECATED clientIDList "Use generic-lens or generic-optics with 'clientIDList' instead"  #-}

instance Core.ToQuery CreateOpenIDConnectProvider where
        toQuery CreateOpenIDConnectProvider{..}
          = Core.toQueryPair "Action"
              ("CreateOpenIDConnectProvider" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "Url" url
              Core.<>
              Core.toQueryPair "ThumbprintList"
                (Core.toQueryList "member" thumbprintList)
              Core.<>
              Core.toQueryPair "ClientIDList"
                (Core.maybe Core.mempty (Core.toQueryList "member") clientIDList)

instance Core.ToHeaders CreateOpenIDConnectProvider where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateOpenIDConnectProvider where
        type Rs CreateOpenIDConnectProvider =
             CreateOpenIDConnectProviderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateOpenIDConnectProviderResult"
              (\ s h x ->
                 CreateOpenIDConnectProviderResponse' Core.<$>
                   (x Core..@? "OpenIDConnectProviderArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'CreateOpenIDConnectProvider' request. 
--
-- /See:/ 'mkCreateOpenIDConnectProviderResponse' smart constructor.
data CreateOpenIDConnectProviderResponse = CreateOpenIDConnectProviderResponse'
  { openIDConnectProviderArn :: Core.Maybe Types.OpenIDConnectProviderArn
    -- ^ The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider that is created. For more information, see 'OpenIDConnectProviderListEntry' . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOpenIDConnectProviderResponse' value with any optional fields omitted.
mkCreateOpenIDConnectProviderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOpenIDConnectProviderResponse
mkCreateOpenIDConnectProviderResponse responseStatus
  = CreateOpenIDConnectProviderResponse'{openIDConnectProviderArn =
                                           Core.Nothing,
                                         responseStatus}

-- | The Amazon Resource Name (ARN) of the new IAM OpenID Connect provider that is created. For more information, see 'OpenIDConnectProviderListEntry' . 
--
-- /Note:/ Consider using 'openIDConnectProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coidcprrsOpenIDConnectProviderArn :: Lens.Lens' CreateOpenIDConnectProviderResponse (Core.Maybe Types.OpenIDConnectProviderArn)
coidcprrsOpenIDConnectProviderArn = Lens.field @"openIDConnectProviderArn"
{-# INLINEABLE coidcprrsOpenIDConnectProviderArn #-}
{-# DEPRECATED openIDConnectProviderArn "Use generic-lens or generic-optics with 'openIDConnectProviderArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coidcprrsResponseStatus :: Lens.Lens' CreateOpenIDConnectProviderResponse Core.Int
coidcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE coidcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
