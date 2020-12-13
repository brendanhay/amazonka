{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain name.
module Network.AWS.APIGateway.CreateDomainName
  ( -- * Creating a request
    CreateDomainName (..),
    mkCreateDomainName,

    -- ** Request lenses
    cdnCertificateName,
    cdnRegionalCertificateARN,
    cdnCertificateARN,
    cdnSecurityPolicy,
    cdnDomainName,
    cdnMutualTLSAuthentication,
    cdnCertificatePrivateKey,
    cdnRegionalCertificateName,
    cdnCertificateBody,
    cdnCertificateChain,
    cdnEndpointConfiguration,
    cdnTags,

    -- * Destructuring the response
    DomainName (..),
    mkDomainName,

    -- ** Response lenses
    dnRegionalHostedZoneId,
    dnCertificateName,
    dnRegionalCertificateARN,
    dnCertificateARN,
    dnDistributionHostedZoneId,
    dnSecurityPolicy,
    dnDomainName,
    dnMutualTLSAuthentication,
    dnRegionalCertificateName,
    dnRegionalDomainName,
    dnCertificateUploadDate,
    dnDistributionDomainName,
    dnDomainNameStatusMessage,
    dnEndpointConfiguration,
    dnDomainNameStatus,
    dnTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to create a new domain name.
--
-- /See:/ 'mkCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { -- | The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
    certificateName :: Lude.Maybe Lude.Text,
    -- | The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
    regionalCertificateARN :: Lude.Maybe Lude.Text,
    -- | The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
    securityPolicy :: Lude.Maybe SecurityPolicy,
    -- | [Required] The name of the 'DomainName' resource.
    domainName :: Lude.Text,
    mutualTLSAuthentication :: Lude.Maybe MutualTLSAuthenticationInput,
    -- | [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
    certificatePrivateKey :: Lude.Maybe Lude.Text,
    -- | The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
    regionalCertificateName :: Lude.Maybe Lude.Text,
    -- | [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
    certificateBody :: Lude.Maybe Lude.Text,
    -- | [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
    certificateChain :: Lude.Maybe Lude.Text,
    -- | The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
    endpointConfiguration :: Lude.Maybe EndpointConfiguration,
    -- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainName' with the minimum fields required to make a request.
--
-- * 'certificateName' - The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
-- * 'regionalCertificateARN' - The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
-- * 'certificateARN' - The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
-- * 'securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
-- * 'domainName' - [Required] The name of the 'DomainName' resource.
-- * 'mutualTLSAuthentication' -
-- * 'certificatePrivateKey' - [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
-- * 'regionalCertificateName' - The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
-- * 'certificateBody' - [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
-- * 'certificateChain' - [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
-- * 'endpointConfiguration' - The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
-- * 'tags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
mkCreateDomainName ::
  -- | 'domainName'
  Lude.Text ->
  CreateDomainName
mkCreateDomainName pDomainName_ =
  CreateDomainName'
    { certificateName = Lude.Nothing,
      regionalCertificateARN = Lude.Nothing,
      certificateARN = Lude.Nothing,
      securityPolicy = Lude.Nothing,
      domainName = pDomainName_,
      mutualTLSAuthentication = Lude.Nothing,
      certificatePrivateKey = Lude.Nothing,
      regionalCertificateName = Lude.Nothing,
      certificateBody = Lude.Nothing,
      certificateChain = Lude.Nothing,
      endpointConfiguration = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateName :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnCertificateName = Lens.lens (certificateName :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: CreateDomainName)
{-# DEPRECATED cdnCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'regionalCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnRegionalCertificateARN :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnRegionalCertificateARN = Lens.lens (regionalCertificateARN :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {regionalCertificateARN = a} :: CreateDomainName)
{-# DEPRECATED cdnRegionalCertificateARN "Use generic-lens or generic-optics with 'regionalCertificateARN' instead." #-}

-- | The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateARN :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnCertificateARN = Lens.lens (certificateARN :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CreateDomainName)
{-# DEPRECATED cdnCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
--
-- /Note:/ Consider using 'securityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnSecurityPolicy :: Lens.Lens' CreateDomainName (Lude.Maybe SecurityPolicy)
cdnSecurityPolicy = Lens.lens (securityPolicy :: CreateDomainName -> Lude.Maybe SecurityPolicy) (\s a -> s {securityPolicy = a} :: CreateDomainName)
{-# DEPRECATED cdnSecurityPolicy "Use generic-lens or generic-optics with 'securityPolicy' instead." #-}

-- | [Required] The name of the 'DomainName' resource.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnDomainName :: Lens.Lens' CreateDomainName Lude.Text
cdnDomainName = Lens.lens (domainName :: CreateDomainName -> Lude.Text) (\s a -> s {domainName = a} :: CreateDomainName)
{-# DEPRECATED cdnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mutualTLSAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnMutualTLSAuthentication :: Lens.Lens' CreateDomainName (Lude.Maybe MutualTLSAuthenticationInput)
cdnMutualTLSAuthentication = Lens.lens (mutualTLSAuthentication :: CreateDomainName -> Lude.Maybe MutualTLSAuthenticationInput) (\s a -> s {mutualTLSAuthentication = a} :: CreateDomainName)
{-# DEPRECATED cdnMutualTLSAuthentication "Use generic-lens or generic-optics with 'mutualTLSAuthentication' instead." #-}

-- | [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
--
-- /Note:/ Consider using 'certificatePrivateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificatePrivateKey :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnCertificatePrivateKey = Lens.lens (certificatePrivateKey :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificatePrivateKey = a} :: CreateDomainName)
{-# DEPRECATED cdnCertificatePrivateKey "Use generic-lens or generic-optics with 'certificatePrivateKey' instead." #-}

-- | The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
--
-- /Note:/ Consider using 'regionalCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnRegionalCertificateName :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnRegionalCertificateName = Lens.lens (regionalCertificateName :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {regionalCertificateName = a} :: CreateDomainName)
{-# DEPRECATED cdnRegionalCertificateName "Use generic-lens or generic-optics with 'regionalCertificateName' instead." #-}

-- | [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateBody :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnCertificateBody = Lens.lens (certificateBody :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificateBody = a} :: CreateDomainName)
{-# DEPRECATED cdnCertificateBody "Use generic-lens or generic-optics with 'certificateBody' instead." #-}

-- | [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateChain :: Lens.Lens' CreateDomainName (Lude.Maybe Lude.Text)
cdnCertificateChain = Lens.lens (certificateChain :: CreateDomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificateChain = a} :: CreateDomainName)
{-# DEPRECATED cdnCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnEndpointConfiguration :: Lens.Lens' CreateDomainName (Lude.Maybe EndpointConfiguration)
cdnEndpointConfiguration = Lens.lens (endpointConfiguration :: CreateDomainName -> Lude.Maybe EndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: CreateDomainName)
{-# DEPRECATED cdnEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnTags :: Lens.Lens' CreateDomainName (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdnTags = Lens.lens (tags :: CreateDomainName -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateDomainName)
{-# DEPRECATED cdnTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDomainName where
  type Rs CreateDomainName = DomainName
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateDomainName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateDomainName where
  toJSON CreateDomainName' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("certificateName" Lude..=) Lude.<$> certificateName,
            ("regionalCertificateArn" Lude..=) Lude.<$> regionalCertificateARN,
            ("certificateArn" Lude..=) Lude.<$> certificateARN,
            ("securityPolicy" Lude..=) Lude.<$> securityPolicy,
            Lude.Just ("domainName" Lude..= domainName),
            ("mutualTlsAuthentication" Lude..=)
              Lude.<$> mutualTLSAuthentication,
            ("certificatePrivateKey" Lude..=) Lude.<$> certificatePrivateKey,
            ("regionalCertificateName" Lude..=)
              Lude.<$> regionalCertificateName,
            ("certificateBody" Lude..=) Lude.<$> certificateBody,
            ("certificateChain" Lude..=) Lude.<$> certificateChain,
            ("endpointConfiguration" Lude..=) Lude.<$> endpointConfiguration,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDomainName where
  toPath = Lude.const "/domainnames"

instance Lude.ToQuery CreateDomainName where
  toQuery = Lude.const Lude.mempty
