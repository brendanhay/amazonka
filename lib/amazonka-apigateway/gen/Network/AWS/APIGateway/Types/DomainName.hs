{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DomainName
  ( DomainName (..),

    -- * Smart constructor
    mkDomainName,

    -- * Lenses
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

import Network.AWS.APIGateway.Types.DomainNameStatus
import Network.AWS.APIGateway.Types.EndpointConfiguration
import Network.AWS.APIGateway.Types.MutualTLSAuthentication
import Network.AWS.APIGateway.Types.SecurityPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a custom domain name as a user-friendly host name of an API ('RestApi' ).
--
-- When you deploy an API, API Gateway creates a default host name for the API. This default API host name is of the @{restapi-id}.execute-api.{region}.amazonaws.com@ format. With the default host name, you can access the API's root resource with the URL of @https://{restapi-id}.execute-api.{region}.amazonaws.com/{stage}/@ . When you set up a custom domain name of @apis.example.com@ for this API, you can then access the same resource using the URL of the @https://apis.examples.com/myApi@ , where @myApi@ is the base path mapping ('BasePathMapping' ) of your API under the custom domain name.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Set a Custom Host Name for an API>
--
-- /See:/ 'mkDomainName' smart constructor.
data DomainName = DomainName'
  { regionalHostedZoneId ::
      Lude.Maybe Lude.Text,
    certificateName :: Lude.Maybe Lude.Text,
    regionalCertificateARN :: Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    distributionHostedZoneId :: Lude.Maybe Lude.Text,
    securityPolicy :: Lude.Maybe SecurityPolicy,
    domainName :: Lude.Maybe Lude.Text,
    mutualTLSAuthentication :: Lude.Maybe MutualTLSAuthentication,
    regionalCertificateName :: Lude.Maybe Lude.Text,
    regionalDomainName :: Lude.Maybe Lude.Text,
    certificateUploadDate :: Lude.Maybe Lude.Timestamp,
    distributionDomainName :: Lude.Maybe Lude.Text,
    domainNameStatusMessage :: Lude.Maybe Lude.Text,
    endpointConfiguration :: Lude.Maybe EndpointConfiguration,
    domainNameStatus :: Lude.Maybe DomainNameStatus,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainName' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
-- * 'certificateName' - The name of the certificate that will be used by edge-optimized endpoint for this domain name.
-- * 'certificateUploadDate' - The timestamp when the certificate that was used by edge-optimized endpoint for this domain name was uploaded.
-- * 'distributionDomainName' - The domain name of the Amazon CloudFront distribution associated with this custom domain name for an edge-optimized endpoint. You set up this association when adding a DNS record pointing the custom domain name to this distribution name. For more information about CloudFront distributions, see the <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation> .
-- * 'distributionHostedZoneId' - The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> .
-- * 'domainName' - The custom domain name as an API host name, for example, @my-api.example.com@ .
-- * 'domainNameStatus' - The status of the 'DomainName' migration. The valid values are @AVAILABLE@ and @UPDATING@ . If the status is @UPDATING@ , the domain cannot be modified further until the existing operation is complete. If it is @AVAILABLE@ , the domain can be updated.
-- * 'domainNameStatusMessage' - An optional text message containing detailed information about status of the 'DomainName' migration.
-- * 'endpointConfiguration' - The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
-- * 'mutualTLSAuthentication' - The mutual TLS authentication configuration for a custom domain name. If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your API.
-- * 'regionalCertificateARN' - The reference to an AWS-managed certificate that will be used for validating the regional domain name. AWS Certificate Manager is the only supported source.
-- * 'regionalCertificateName' - The name of the certificate that will be used for validating the regional domain name.
-- * 'regionalDomainName' - The domain name associated with the regional endpoint for this custom domain name. You set up this association by adding a DNS record that points the custom domain name to this regional domain name. The regional domain name is returned by API Gateway when you create a regional endpoint.
-- * 'regionalHostedZoneId' - The region-specific Amazon Route 53 Hosted Zone ID of the regional endpoint. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> .
-- * 'securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
mkDomainName ::
  DomainName
mkDomainName =
  DomainName'
    { regionalHostedZoneId = Lude.Nothing,
      certificateName = Lude.Nothing,
      regionalCertificateARN = Lude.Nothing,
      certificateARN = Lude.Nothing,
      distributionHostedZoneId = Lude.Nothing,
      securityPolicy = Lude.Nothing,
      domainName = Lude.Nothing,
      mutualTLSAuthentication = Lude.Nothing,
      regionalCertificateName = Lude.Nothing,
      regionalDomainName = Lude.Nothing,
      certificateUploadDate = Lude.Nothing,
      distributionDomainName = Lude.Nothing,
      domainNameStatusMessage = Lude.Nothing,
      endpointConfiguration = Lude.Nothing,
      domainNameStatus = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The region-specific Amazon Route 53 Hosted Zone ID of the regional endpoint. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> .
--
-- /Note:/ Consider using 'regionalHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalHostedZoneId :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnRegionalHostedZoneId = Lens.lens (regionalHostedZoneId :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {regionalHostedZoneId = a} :: DomainName)
{-# DEPRECATED dnRegionalHostedZoneId "Use generic-lens or generic-optics with 'regionalHostedZoneId' instead." #-}

-- | The name of the certificate that will be used by edge-optimized endpoint for this domain name.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnCertificateName :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnCertificateName = Lens.lens (certificateName :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: DomainName)
{-# DEPRECATED dnCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The reference to an AWS-managed certificate that will be used for validating the regional domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'regionalCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalCertificateARN :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnRegionalCertificateARN = Lens.lens (regionalCertificateARN :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {regionalCertificateARN = a} :: DomainName)
{-# DEPRECATED dnRegionalCertificateARN "Use generic-lens or generic-optics with 'regionalCertificateARN' instead." #-}

-- | The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnCertificateARN :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnCertificateARN = Lens.lens (certificateARN :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: DomainName)
{-# DEPRECATED dnCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> .
--
-- /Note:/ Consider using 'distributionHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDistributionHostedZoneId :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnDistributionHostedZoneId = Lens.lens (distributionHostedZoneId :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {distributionHostedZoneId = a} :: DomainName)
{-# DEPRECATED dnDistributionHostedZoneId "Use generic-lens or generic-optics with 'distributionHostedZoneId' instead." #-}

-- | The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
--
-- /Note:/ Consider using 'securityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnSecurityPolicy :: Lens.Lens' DomainName (Lude.Maybe SecurityPolicy)
dnSecurityPolicy = Lens.lens (securityPolicy :: DomainName -> Lude.Maybe SecurityPolicy) (\s a -> s {securityPolicy = a} :: DomainName)
{-# DEPRECATED dnSecurityPolicy "Use generic-lens or generic-optics with 'securityPolicy' instead." #-}

-- | The custom domain name as an API host name, for example, @my-api.example.com@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDomainName :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnDomainName = Lens.lens (domainName :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DomainName)
{-# DEPRECATED dnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The mutual TLS authentication configuration for a custom domain name. If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your API.
--
-- /Note:/ Consider using 'mutualTLSAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnMutualTLSAuthentication :: Lens.Lens' DomainName (Lude.Maybe MutualTLSAuthentication)
dnMutualTLSAuthentication = Lens.lens (mutualTLSAuthentication :: DomainName -> Lude.Maybe MutualTLSAuthentication) (\s a -> s {mutualTLSAuthentication = a} :: DomainName)
{-# DEPRECATED dnMutualTLSAuthentication "Use generic-lens or generic-optics with 'mutualTLSAuthentication' instead." #-}

-- | The name of the certificate that will be used for validating the regional domain name.
--
-- /Note:/ Consider using 'regionalCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalCertificateName :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnRegionalCertificateName = Lens.lens (regionalCertificateName :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {regionalCertificateName = a} :: DomainName)
{-# DEPRECATED dnRegionalCertificateName "Use generic-lens or generic-optics with 'regionalCertificateName' instead." #-}

-- | The domain name associated with the regional endpoint for this custom domain name. You set up this association by adding a DNS record that points the custom domain name to this regional domain name. The regional domain name is returned by API Gateway when you create a regional endpoint.
--
-- /Note:/ Consider using 'regionalDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalDomainName :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnRegionalDomainName = Lens.lens (regionalDomainName :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {regionalDomainName = a} :: DomainName)
{-# DEPRECATED dnRegionalDomainName "Use generic-lens or generic-optics with 'regionalDomainName' instead." #-}

-- | The timestamp when the certificate that was used by edge-optimized endpoint for this domain name was uploaded.
--
-- /Note:/ Consider using 'certificateUploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnCertificateUploadDate :: Lens.Lens' DomainName (Lude.Maybe Lude.Timestamp)
dnCertificateUploadDate = Lens.lens (certificateUploadDate :: DomainName -> Lude.Maybe Lude.Timestamp) (\s a -> s {certificateUploadDate = a} :: DomainName)
{-# DEPRECATED dnCertificateUploadDate "Use generic-lens or generic-optics with 'certificateUploadDate' instead." #-}

-- | The domain name of the Amazon CloudFront distribution associated with this custom domain name for an edge-optimized endpoint. You set up this association when adding a DNS record pointing the custom domain name to this distribution name. For more information about CloudFront distributions, see the <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation> .
--
-- /Note:/ Consider using 'distributionDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDistributionDomainName :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnDistributionDomainName = Lens.lens (distributionDomainName :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {distributionDomainName = a} :: DomainName)
{-# DEPRECATED dnDistributionDomainName "Use generic-lens or generic-optics with 'distributionDomainName' instead." #-}

-- | An optional text message containing detailed information about status of the 'DomainName' migration.
--
-- /Note:/ Consider using 'domainNameStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDomainNameStatusMessage :: Lens.Lens' DomainName (Lude.Maybe Lude.Text)
dnDomainNameStatusMessage = Lens.lens (domainNameStatusMessage :: DomainName -> Lude.Maybe Lude.Text) (\s a -> s {domainNameStatusMessage = a} :: DomainName)
{-# DEPRECATED dnDomainNameStatusMessage "Use generic-lens or generic-optics with 'domainNameStatusMessage' instead." #-}

-- | The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnEndpointConfiguration :: Lens.Lens' DomainName (Lude.Maybe EndpointConfiguration)
dnEndpointConfiguration = Lens.lens (endpointConfiguration :: DomainName -> Lude.Maybe EndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: DomainName)
{-# DEPRECATED dnEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | The status of the 'DomainName' migration. The valid values are @AVAILABLE@ and @UPDATING@ . If the status is @UPDATING@ , the domain cannot be modified further until the existing operation is complete. If it is @AVAILABLE@ , the domain can be updated.
--
-- /Note:/ Consider using 'domainNameStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDomainNameStatus :: Lens.Lens' DomainName (Lude.Maybe DomainNameStatus)
dnDomainNameStatus = Lens.lens (domainNameStatus :: DomainName -> Lude.Maybe DomainNameStatus) (\s a -> s {domainNameStatus = a} :: DomainName)
{-# DEPRECATED dnDomainNameStatus "Use generic-lens or generic-optics with 'domainNameStatus' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnTags :: Lens.Lens' DomainName (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dnTags = Lens.lens (tags :: DomainName -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DomainName)
{-# DEPRECATED dnTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON DomainName where
  parseJSON =
    Lude.withObject
      "DomainName"
      ( \x ->
          DomainName'
            Lude.<$> (x Lude..:? "regionalHostedZoneId")
            Lude.<*> (x Lude..:? "certificateName")
            Lude.<*> (x Lude..:? "regionalCertificateArn")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "distributionHostedZoneId")
            Lude.<*> (x Lude..:? "securityPolicy")
            Lude.<*> (x Lude..:? "domainName")
            Lude.<*> (x Lude..:? "mutualTlsAuthentication")
            Lude.<*> (x Lude..:? "regionalCertificateName")
            Lude.<*> (x Lude..:? "regionalDomainName")
            Lude.<*> (x Lude..:? "certificateUploadDate")
            Lude.<*> (x Lude..:? "distributionDomainName")
            Lude.<*> (x Lude..:? "domainNameStatusMessage")
            Lude.<*> (x Lude..:? "endpointConfiguration")
            Lude.<*> (x Lude..:? "domainNameStatus")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
