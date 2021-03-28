{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.DomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.DomainName
  ( DomainName (..)
  -- * Smart constructor
  , mkDomainName
  -- * Lenses
  , dnCertificateArn
  , dnCertificateName
  , dnCertificateUploadDate
  , dnDistributionDomainName
  , dnDistributionHostedZoneId
  , dnDomainName
  , dnDomainNameStatus
  , dnDomainNameStatusMessage
  , dnEndpointConfiguration
  , dnMutualTlsAuthentication
  , dnRegionalCertificateArn
  , dnRegionalCertificateName
  , dnRegionalDomainName
  , dnRegionalHostedZoneId
  , dnSecurityPolicy
  , dnTags
  ) where

import qualified Network.AWS.ApiGateway.Types.DomainNameStatus as Types
import qualified Network.AWS.ApiGateway.Types.EndpointConfiguration as Types
import qualified Network.AWS.ApiGateway.Types.MutualTlsAuthentication as Types
import qualified Network.AWS.ApiGateway.Types.SecurityPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a custom domain name as a user-friendly host name of an API ('RestApi' ).
--
-- When you deploy an API, API Gateway creates a default host name for the API. This default API host name is of the @{restapi-id}.execute-api.{region}.amazonaws.com@ format. With the default host name, you can access the API's root resource with the URL of @https://{restapi-id}.execute-api.{region}.amazonaws.com/{stage}/@ . When you set up a custom domain name of @apis.example.com@ for this API, you can then access the same resource using the URL of the @https://apis.examples.com/myApi@ , where @myApi@ is the base path mapping ('BasePathMapping' ) of your API under the custom domain name. 
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Set a Custom Host Name for an API> 
--
-- /See:/ 'mkDomainName' smart constructor.
data DomainName = DomainName'
  { certificateArn :: Core.Maybe Core.Text
    -- ^ The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
  , certificateName :: Core.Maybe Core.Text
    -- ^ The name of the certificate that will be used by edge-optimized endpoint for this domain name.
  , certificateUploadDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the certificate that was used by edge-optimized endpoint for this domain name was uploaded.
  , distributionDomainName :: Core.Maybe Core.Text
    -- ^ The domain name of the Amazon CloudFront distribution associated with this custom domain name for an edge-optimized endpoint. You set up this association when adding a DNS record pointing the custom domain name to this distribution name. For more information about CloudFront distributions, see the <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation> .
  , distributionHostedZoneId :: Core.Maybe Core.Text
    -- ^ The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> . 
  , domainName :: Core.Maybe Core.Text
    -- ^ The custom domain name as an API host name, for example, @my-api.example.com@ .
  , domainNameStatus :: Core.Maybe Types.DomainNameStatus
    -- ^ The status of the 'DomainName' migration. The valid values are @AVAILABLE@ and @UPDATING@ . If the status is @UPDATING@ , the domain cannot be modified further until the existing operation is complete. If it is @AVAILABLE@ , the domain can be updated.
  , domainNameStatusMessage :: Core.Maybe Core.Text
    -- ^ An optional text message containing detailed information about status of the 'DomainName' migration.
  , endpointConfiguration :: Core.Maybe Types.EndpointConfiguration
    -- ^ The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
  , mutualTlsAuthentication :: Core.Maybe Types.MutualTlsAuthentication
    -- ^ The mutual TLS authentication configuration for a custom domain name. If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your API.
  , regionalCertificateArn :: Core.Maybe Core.Text
    -- ^ The reference to an AWS-managed certificate that will be used for validating the regional domain name. AWS Certificate Manager is the only supported source.
  , regionalCertificateName :: Core.Maybe Core.Text
    -- ^ The name of the certificate that will be used for validating the regional domain name.
  , regionalDomainName :: Core.Maybe Core.Text
    -- ^ The domain name associated with the regional endpoint for this custom domain name. You set up this association by adding a DNS record that points the custom domain name to this regional domain name. The regional domain name is returned by API Gateway when you create a regional endpoint.
  , regionalHostedZoneId :: Core.Maybe Core.Text
    -- ^ The region-specific Amazon Route 53 Hosted Zone ID of the regional endpoint. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> . 
  , securityPolicy :: Core.Maybe Types.SecurityPolicy
    -- ^ The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DomainName' value with any optional fields omitted.
mkDomainName
    :: DomainName
mkDomainName
  = DomainName'{certificateArn = Core.Nothing,
                certificateName = Core.Nothing,
                certificateUploadDate = Core.Nothing,
                distributionDomainName = Core.Nothing,
                distributionHostedZoneId = Core.Nothing, domainName = Core.Nothing,
                domainNameStatus = Core.Nothing,
                domainNameStatusMessage = Core.Nothing,
                endpointConfiguration = Core.Nothing,
                mutualTlsAuthentication = Core.Nothing,
                regionalCertificateArn = Core.Nothing,
                regionalCertificateName = Core.Nothing,
                regionalDomainName = Core.Nothing,
                regionalHostedZoneId = Core.Nothing, securityPolicy = Core.Nothing,
                tags = Core.Nothing}

-- | The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnCertificateArn :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE dnCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The name of the certificate that will be used by edge-optimized endpoint for this domain name.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnCertificateName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnCertificateName = Lens.field @"certificateName"
{-# INLINEABLE dnCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | The timestamp when the certificate that was used by edge-optimized endpoint for this domain name was uploaded.
--
-- /Note:/ Consider using 'certificateUploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnCertificateUploadDate :: Lens.Lens' DomainName (Core.Maybe Core.NominalDiffTime)
dnCertificateUploadDate = Lens.field @"certificateUploadDate"
{-# INLINEABLE dnCertificateUploadDate #-}
{-# DEPRECATED certificateUploadDate "Use generic-lens or generic-optics with 'certificateUploadDate' instead"  #-}

-- | The domain name of the Amazon CloudFront distribution associated with this custom domain name for an edge-optimized endpoint. You set up this association when adding a DNS record pointing the custom domain name to this distribution name. For more information about CloudFront distributions, see the <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation> .
--
-- /Note:/ Consider using 'distributionDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDistributionDomainName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnDistributionDomainName = Lens.field @"distributionDomainName"
{-# INLINEABLE dnDistributionDomainName #-}
{-# DEPRECATED distributionDomainName "Use generic-lens or generic-optics with 'distributionDomainName' instead"  #-}

-- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> . 
--
-- /Note:/ Consider using 'distributionHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDistributionHostedZoneId :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnDistributionHostedZoneId = Lens.field @"distributionHostedZoneId"
{-# INLINEABLE dnDistributionHostedZoneId #-}
{-# DEPRECATED distributionHostedZoneId "Use generic-lens or generic-optics with 'distributionHostedZoneId' instead"  #-}

-- | The custom domain name as an API host name, for example, @my-api.example.com@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDomainName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnDomainName = Lens.field @"domainName"
{-# INLINEABLE dnDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The status of the 'DomainName' migration. The valid values are @AVAILABLE@ and @UPDATING@ . If the status is @UPDATING@ , the domain cannot be modified further until the existing operation is complete. If it is @AVAILABLE@ , the domain can be updated.
--
-- /Note:/ Consider using 'domainNameStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDomainNameStatus :: Lens.Lens' DomainName (Core.Maybe Types.DomainNameStatus)
dnDomainNameStatus = Lens.field @"domainNameStatus"
{-# INLINEABLE dnDomainNameStatus #-}
{-# DEPRECATED domainNameStatus "Use generic-lens or generic-optics with 'domainNameStatus' instead"  #-}

-- | An optional text message containing detailed information about status of the 'DomainName' migration.
--
-- /Note:/ Consider using 'domainNameStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnDomainNameStatusMessage :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnDomainNameStatusMessage = Lens.field @"domainNameStatusMessage"
{-# INLINEABLE dnDomainNameStatusMessage #-}
{-# DEPRECATED domainNameStatusMessage "Use generic-lens or generic-optics with 'domainNameStatusMessage' instead"  #-}

-- | The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnEndpointConfiguration :: Lens.Lens' DomainName (Core.Maybe Types.EndpointConfiguration)
dnEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE dnEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | The mutual TLS authentication configuration for a custom domain name. If specified, API Gateway performs two-way authentication between the client and the server. Clients must present a trusted certificate to access your API.
--
-- /Note:/ Consider using 'mutualTlsAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnMutualTlsAuthentication :: Lens.Lens' DomainName (Core.Maybe Types.MutualTlsAuthentication)
dnMutualTlsAuthentication = Lens.field @"mutualTlsAuthentication"
{-# INLINEABLE dnMutualTlsAuthentication #-}
{-# DEPRECATED mutualTlsAuthentication "Use generic-lens or generic-optics with 'mutualTlsAuthentication' instead"  #-}

-- | The reference to an AWS-managed certificate that will be used for validating the regional domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'regionalCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalCertificateArn :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnRegionalCertificateArn = Lens.field @"regionalCertificateArn"
{-# INLINEABLE dnRegionalCertificateArn #-}
{-# DEPRECATED regionalCertificateArn "Use generic-lens or generic-optics with 'regionalCertificateArn' instead"  #-}

-- | The name of the certificate that will be used for validating the regional domain name.
--
-- /Note:/ Consider using 'regionalCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalCertificateName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnRegionalCertificateName = Lens.field @"regionalCertificateName"
{-# INLINEABLE dnRegionalCertificateName #-}
{-# DEPRECATED regionalCertificateName "Use generic-lens or generic-optics with 'regionalCertificateName' instead"  #-}

-- | The domain name associated with the regional endpoint for this custom domain name. You set up this association by adding a DNS record that points the custom domain name to this regional domain name. The regional domain name is returned by API Gateway when you create a regional endpoint.
--
-- /Note:/ Consider using 'regionalDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalDomainName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnRegionalDomainName = Lens.field @"regionalDomainName"
{-# INLINEABLE dnRegionalDomainName #-}
{-# DEPRECATED regionalDomainName "Use generic-lens or generic-optics with 'regionalDomainName' instead"  #-}

-- | The region-specific Amazon Route 53 Hosted Zone ID of the regional endpoint. For more information, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name> and <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway> . 
--
-- /Note:/ Consider using 'regionalHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnRegionalHostedZoneId :: Lens.Lens' DomainName (Core.Maybe Core.Text)
dnRegionalHostedZoneId = Lens.field @"regionalHostedZoneId"
{-# INLINEABLE dnRegionalHostedZoneId #-}
{-# DEPRECATED regionalHostedZoneId "Use generic-lens or generic-optics with 'regionalHostedZoneId' instead"  #-}

-- | The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
--
-- /Note:/ Consider using 'securityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnSecurityPolicy :: Lens.Lens' DomainName (Core.Maybe Types.SecurityPolicy)
dnSecurityPolicy = Lens.field @"securityPolicy"
{-# INLINEABLE dnSecurityPolicy #-}
{-# DEPRECATED securityPolicy "Use generic-lens or generic-optics with 'securityPolicy' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnTags :: Lens.Lens' DomainName (Core.Maybe (Core.HashMap Core.Text Core.Text))
dnTags = Lens.field @"tags"
{-# INLINEABLE dnTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON DomainName where
        parseJSON
          = Core.withObject "DomainName" Core.$
              \ x ->
                DomainName' Core.<$>
                  (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateName"
                    Core.<*> x Core..:? "certificateUploadDate"
                    Core.<*> x Core..:? "distributionDomainName"
                    Core.<*> x Core..:? "distributionHostedZoneId"
                    Core.<*> x Core..:? "domainName"
                    Core.<*> x Core..:? "domainNameStatus"
                    Core.<*> x Core..:? "domainNameStatusMessage"
                    Core.<*> x Core..:? "endpointConfiguration"
                    Core.<*> x Core..:? "mutualTlsAuthentication"
                    Core.<*> x Core..:? "regionalCertificateArn"
                    Core.<*> x Core..:? "regionalCertificateName"
                    Core.<*> x Core..:? "regionalDomainName"
                    Core.<*> x Core..:? "regionalHostedZoneId"
                    Core.<*> x Core..:? "securityPolicy"
                    Core.<*> x Core..:? "tags"
