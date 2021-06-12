{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DomainName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DomainName where

import Network.AWS.APIGateway.Types.DomainNameStatus
import Network.AWS.APIGateway.Types.EndpointConfiguration
import Network.AWS.APIGateway.Types.MutualTlsAuthentication
import Network.AWS.APIGateway.Types.SecurityPolicy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a custom domain name as a user-friendly host name of an API
-- (RestApi).
--
-- When you deploy an API, API Gateway creates a default host name for the
-- API. This default API host name is of the
-- @{restapi-id}.execute-api.{region}.amazonaws.com@ format. With the
-- default host name, you can access the API\'s root resource with the URL
-- of
-- @https:\/\/{restapi-id}.execute-api.{region}.amazonaws.com\/{stage}\/@.
-- When you set up a custom domain name of @apis.example.com@ for this API,
-- you can then access the same resource using the URL of the
-- @https:\/\/apis.examples.com\/myApi@, where @myApi@ is the base path
-- mapping (BasePathMapping) of your API under the custom domain name.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Set a Custom Host Name for an API>
--
-- /See:/ 'newDomainName' smart constructor.
data DomainName = DomainName'
  { -- | The region-specific Amazon Route 53 Hosted Zone ID of the regional
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
    regionalHostedZoneId :: Core.Maybe Core.Text,
    -- | The name of the certificate that will be used for validating the
    -- regional domain name.
    regionalCertificateName :: Core.Maybe Core.Text,
    -- | The mutual TLS authentication configuration for a custom domain name. If
    -- specified, API Gateway performs two-way authentication between the
    -- client and the server. Clients must present a trusted certificate to
    -- access your API.
    mutualTlsAuthentication :: Core.Maybe MutualTlsAuthentication,
    -- | The endpoint configuration of this DomainName showing the endpoint types
    -- of the domain name.
    endpointConfiguration :: Core.Maybe EndpointConfiguration,
    -- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
    -- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
    -- more information, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
    distributionHostedZoneId :: Core.Maybe Core.Text,
    -- | The reference to an AWS-managed certificate that will be used by
    -- edge-optimized endpoint for this domain name. AWS Certificate Manager is
    -- the only supported source.
    certificateArn :: Core.Maybe Core.Text,
    -- | An optional text message containing detailed information about status of
    -- the DomainName migration.
    domainNameStatusMessage :: Core.Maybe Core.Text,
    -- | The domain name of the Amazon CloudFront distribution associated with
    -- this custom domain name for an edge-optimized endpoint. You set up this
    -- association when adding a DNS record pointing the custom domain name to
    -- this distribution name. For more information about CloudFront
    -- distributions, see the
    -- <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation>.
    distributionDomainName :: Core.Maybe Core.Text,
    -- | The timestamp when the certificate that was used by edge-optimized
    -- endpoint for this domain name was uploaded.
    certificateUploadDate :: Core.Maybe Core.POSIX,
    -- | The custom domain name as an API host name, for example,
    -- @my-api.example.com@.
    domainName :: Core.Maybe Core.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Transport Layer Security (TLS) version + cipher suite for this
    -- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
    securityPolicy :: Core.Maybe SecurityPolicy,
    -- | The status of the DomainName migration. The valid values are @AVAILABLE@
    -- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
    -- modified further until the existing operation is complete. If it is
    -- @AVAILABLE@, the domain can be updated.
    domainNameStatus :: Core.Maybe DomainNameStatus,
    -- | The reference to an AWS-managed certificate that will be used for
    -- validating the regional domain name. AWS Certificate Manager is the only
    -- supported source.
    regionalCertificateArn :: Core.Maybe Core.Text,
    -- | The name of the certificate that will be used by edge-optimized endpoint
    -- for this domain name.
    certificateName :: Core.Maybe Core.Text,
    -- | The domain name associated with the regional endpoint for this custom
    -- domain name. You set up this association by adding a DNS record that
    -- points the custom domain name to this regional domain name. The regional
    -- domain name is returned by API Gateway when you create a regional
    -- endpoint.
    regionalDomainName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionalHostedZoneId', 'domainName_regionalHostedZoneId' - The region-specific Amazon Route 53 Hosted Zone ID of the regional
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
--
-- 'regionalCertificateName', 'domainName_regionalCertificateName' - The name of the certificate that will be used for validating the
-- regional domain name.
--
-- 'mutualTlsAuthentication', 'domainName_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name. If
-- specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your API.
--
-- 'endpointConfiguration', 'domainName_endpointConfiguration' - The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
--
-- 'distributionHostedZoneId', 'domainName_distributionHostedZoneId' - The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
-- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
-- more information, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
--
-- 'certificateArn', 'domainName_certificateArn' - The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
--
-- 'domainNameStatusMessage', 'domainName_domainNameStatusMessage' - An optional text message containing detailed information about status of
-- the DomainName migration.
--
-- 'distributionDomainName', 'domainName_distributionDomainName' - The domain name of the Amazon CloudFront distribution associated with
-- this custom domain name for an edge-optimized endpoint. You set up this
-- association when adding a DNS record pointing the custom domain name to
-- this distribution name. For more information about CloudFront
-- distributions, see the
-- <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation>.
--
-- 'certificateUploadDate', 'domainName_certificateUploadDate' - The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
--
-- 'domainName', 'domainName_domainName' - The custom domain name as an API host name, for example,
-- @my-api.example.com@.
--
-- 'tags', 'domainName_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'securityPolicy', 'domainName_securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
--
-- 'domainNameStatus', 'domainName_domainNameStatus' - The status of the DomainName migration. The valid values are @AVAILABLE@
-- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
-- modified further until the existing operation is complete. If it is
-- @AVAILABLE@, the domain can be updated.
--
-- 'regionalCertificateArn', 'domainName_regionalCertificateArn' - The reference to an AWS-managed certificate that will be used for
-- validating the regional domain name. AWS Certificate Manager is the only
-- supported source.
--
-- 'certificateName', 'domainName_certificateName' - The name of the certificate that will be used by edge-optimized endpoint
-- for this domain name.
--
-- 'regionalDomainName', 'domainName_regionalDomainName' - The domain name associated with the regional endpoint for this custom
-- domain name. You set up this association by adding a DNS record that
-- points the custom domain name to this regional domain name. The regional
-- domain name is returned by API Gateway when you create a regional
-- endpoint.
newDomainName ::
  DomainName
newDomainName =
  DomainName'
    { regionalHostedZoneId = Core.Nothing,
      regionalCertificateName = Core.Nothing,
      mutualTlsAuthentication = Core.Nothing,
      endpointConfiguration = Core.Nothing,
      distributionHostedZoneId = Core.Nothing,
      certificateArn = Core.Nothing,
      domainNameStatusMessage = Core.Nothing,
      distributionDomainName = Core.Nothing,
      certificateUploadDate = Core.Nothing,
      domainName = Core.Nothing,
      tags = Core.Nothing,
      securityPolicy = Core.Nothing,
      domainNameStatus = Core.Nothing,
      regionalCertificateArn = Core.Nothing,
      certificateName = Core.Nothing,
      regionalDomainName = Core.Nothing
    }

-- | The region-specific Amazon Route 53 Hosted Zone ID of the regional
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
domainName_regionalHostedZoneId :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_regionalHostedZoneId = Lens.lens (\DomainName' {regionalHostedZoneId} -> regionalHostedZoneId) (\s@DomainName' {} a -> s {regionalHostedZoneId = a} :: DomainName)

-- | The name of the certificate that will be used for validating the
-- regional domain name.
domainName_regionalCertificateName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_regionalCertificateName = Lens.lens (\DomainName' {regionalCertificateName} -> regionalCertificateName) (\s@DomainName' {} a -> s {regionalCertificateName = a} :: DomainName)

-- | The mutual TLS authentication configuration for a custom domain name. If
-- specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your API.
domainName_mutualTlsAuthentication :: Lens.Lens' DomainName (Core.Maybe MutualTlsAuthentication)
domainName_mutualTlsAuthentication = Lens.lens (\DomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@DomainName' {} a -> s {mutualTlsAuthentication = a} :: DomainName)

-- | The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
domainName_endpointConfiguration :: Lens.Lens' DomainName (Core.Maybe EndpointConfiguration)
domainName_endpointConfiguration = Lens.lens (\DomainName' {endpointConfiguration} -> endpointConfiguration) (\s@DomainName' {} a -> s {endpointConfiguration = a} :: DomainName)

-- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
-- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
-- more information, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
domainName_distributionHostedZoneId :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_distributionHostedZoneId = Lens.lens (\DomainName' {distributionHostedZoneId} -> distributionHostedZoneId) (\s@DomainName' {} a -> s {distributionHostedZoneId = a} :: DomainName)

-- | The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
domainName_certificateArn :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_certificateArn = Lens.lens (\DomainName' {certificateArn} -> certificateArn) (\s@DomainName' {} a -> s {certificateArn = a} :: DomainName)

-- | An optional text message containing detailed information about status of
-- the DomainName migration.
domainName_domainNameStatusMessage :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_domainNameStatusMessage = Lens.lens (\DomainName' {domainNameStatusMessage} -> domainNameStatusMessage) (\s@DomainName' {} a -> s {domainNameStatusMessage = a} :: DomainName)

-- | The domain name of the Amazon CloudFront distribution associated with
-- this custom domain name for an edge-optimized endpoint. You set up this
-- association when adding a DNS record pointing the custom domain name to
-- this distribution name. For more information about CloudFront
-- distributions, see the
-- <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation>.
domainName_distributionDomainName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_distributionDomainName = Lens.lens (\DomainName' {distributionDomainName} -> distributionDomainName) (\s@DomainName' {} a -> s {distributionDomainName = a} :: DomainName)

-- | The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
domainName_certificateUploadDate :: Lens.Lens' DomainName (Core.Maybe Core.UTCTime)
domainName_certificateUploadDate = Lens.lens (\DomainName' {certificateUploadDate} -> certificateUploadDate) (\s@DomainName' {} a -> s {certificateUploadDate = a} :: DomainName) Core.. Lens.mapping Core._Time

-- | The custom domain name as an API host name, for example,
-- @my-api.example.com@.
domainName_domainName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_domainName = Lens.lens (\DomainName' {domainName} -> domainName) (\s@DomainName' {} a -> s {domainName = a} :: DomainName)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
domainName_tags :: Lens.Lens' DomainName (Core.Maybe (Core.HashMap Core.Text Core.Text))
domainName_tags = Lens.lens (\DomainName' {tags} -> tags) (\s@DomainName' {} a -> s {tags = a} :: DomainName) Core.. Lens.mapping Lens._Coerce

-- | The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
domainName_securityPolicy :: Lens.Lens' DomainName (Core.Maybe SecurityPolicy)
domainName_securityPolicy = Lens.lens (\DomainName' {securityPolicy} -> securityPolicy) (\s@DomainName' {} a -> s {securityPolicy = a} :: DomainName)

-- | The status of the DomainName migration. The valid values are @AVAILABLE@
-- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
-- modified further until the existing operation is complete. If it is
-- @AVAILABLE@, the domain can be updated.
domainName_domainNameStatus :: Lens.Lens' DomainName (Core.Maybe DomainNameStatus)
domainName_domainNameStatus = Lens.lens (\DomainName' {domainNameStatus} -> domainNameStatus) (\s@DomainName' {} a -> s {domainNameStatus = a} :: DomainName)

-- | The reference to an AWS-managed certificate that will be used for
-- validating the regional domain name. AWS Certificate Manager is the only
-- supported source.
domainName_regionalCertificateArn :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_regionalCertificateArn = Lens.lens (\DomainName' {regionalCertificateArn} -> regionalCertificateArn) (\s@DomainName' {} a -> s {regionalCertificateArn = a} :: DomainName)

-- | The name of the certificate that will be used by edge-optimized endpoint
-- for this domain name.
domainName_certificateName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_certificateName = Lens.lens (\DomainName' {certificateName} -> certificateName) (\s@DomainName' {} a -> s {certificateName = a} :: DomainName)

-- | The domain name associated with the regional endpoint for this custom
-- domain name. You set up this association by adding a DNS record that
-- points the custom domain name to this regional domain name. The regional
-- domain name is returned by API Gateway when you create a regional
-- endpoint.
domainName_regionalDomainName :: Lens.Lens' DomainName (Core.Maybe Core.Text)
domainName_regionalDomainName = Lens.lens (\DomainName' {regionalDomainName} -> regionalDomainName) (\s@DomainName' {} a -> s {regionalDomainName = a} :: DomainName)

instance Core.FromJSON DomainName where
  parseJSON =
    Core.withObject
      "DomainName"
      ( \x ->
          DomainName'
            Core.<$> (x Core..:? "regionalHostedZoneId")
            Core.<*> (x Core..:? "regionalCertificateName")
            Core.<*> (x Core..:? "mutualTlsAuthentication")
            Core.<*> (x Core..:? "endpointConfiguration")
            Core.<*> (x Core..:? "distributionHostedZoneId")
            Core.<*> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "domainNameStatusMessage")
            Core.<*> (x Core..:? "distributionDomainName")
            Core.<*> (x Core..:? "certificateUploadDate")
            Core.<*> (x Core..:? "domainName")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "securityPolicy")
            Core.<*> (x Core..:? "domainNameStatus")
            Core.<*> (x Core..:? "regionalCertificateArn")
            Core.<*> (x Core..:? "certificateName")
            Core.<*> (x Core..:? "regionalDomainName")
      )

instance Core.Hashable DomainName

instance Core.NFData DomainName
