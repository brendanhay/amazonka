{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    regionalHostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The name of the certificate that will be used for validating the
    -- regional domain name.
    regionalCertificateName :: Prelude.Maybe Prelude.Text,
    -- | The mutual TLS authentication configuration for a custom domain name. If
    -- specified, API Gateway performs two-way authentication between the
    -- client and the server. Clients must present a trusted certificate to
    -- access your API.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthentication,
    -- | The endpoint configuration of this DomainName showing the endpoint types
    -- of the domain name.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
    -- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
    -- more information, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
    distributionHostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The reference to an AWS-managed certificate that will be used by
    -- edge-optimized endpoint for this domain name. AWS Certificate Manager is
    -- the only supported source.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | An optional text message containing detailed information about status of
    -- the DomainName migration.
    domainNameStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the Amazon CloudFront distribution associated with
    -- this custom domain name for an edge-optimized endpoint. You set up this
    -- association when adding a DNS record pointing the custom domain name to
    -- this distribution name. For more information about CloudFront
    -- distributions, see the
    -- <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation>.
    distributionDomainName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the certificate that was used by edge-optimized
    -- endpoint for this domain name was uploaded.
    certificateUploadDate :: Prelude.Maybe Prelude.POSIX,
    -- | The custom domain name as an API host name, for example,
    -- @my-api.example.com@.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Transport Layer Security (TLS) version + cipher suite for this
    -- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
    securityPolicy :: Prelude.Maybe SecurityPolicy,
    -- | The status of the DomainName migration. The valid values are @AVAILABLE@
    -- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
    -- modified further until the existing operation is complete. If it is
    -- @AVAILABLE@, the domain can be updated.
    domainNameStatus :: Prelude.Maybe DomainNameStatus,
    -- | The reference to an AWS-managed certificate that will be used for
    -- validating the regional domain name. AWS Certificate Manager is the only
    -- supported source.
    regionalCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the certificate that will be used by edge-optimized endpoint
    -- for this domain name.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | The domain name associated with the regional endpoint for this custom
    -- domain name. You set up this association by adding a DNS record that
    -- points the custom domain name to this regional domain name. The regional
    -- domain name is returned by API Gateway when you create a regional
    -- endpoint.
    regionalDomainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { regionalHostedZoneId = Prelude.Nothing,
      regionalCertificateName = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      distributionHostedZoneId = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      domainNameStatusMessage = Prelude.Nothing,
      distributionDomainName = Prelude.Nothing,
      certificateUploadDate = Prelude.Nothing,
      domainName = Prelude.Nothing,
      tags = Prelude.Nothing,
      securityPolicy = Prelude.Nothing,
      domainNameStatus = Prelude.Nothing,
      regionalCertificateArn = Prelude.Nothing,
      certificateName = Prelude.Nothing,
      regionalDomainName = Prelude.Nothing
    }

-- | The region-specific Amazon Route 53 Hosted Zone ID of the regional
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
domainName_regionalHostedZoneId :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalHostedZoneId = Lens.lens (\DomainName' {regionalHostedZoneId} -> regionalHostedZoneId) (\s@DomainName' {} a -> s {regionalHostedZoneId = a} :: DomainName)

-- | The name of the certificate that will be used for validating the
-- regional domain name.
domainName_regionalCertificateName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalCertificateName = Lens.lens (\DomainName' {regionalCertificateName} -> regionalCertificateName) (\s@DomainName' {} a -> s {regionalCertificateName = a} :: DomainName)

-- | The mutual TLS authentication configuration for a custom domain name. If
-- specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your API.
domainName_mutualTlsAuthentication :: Lens.Lens' DomainName (Prelude.Maybe MutualTlsAuthentication)
domainName_mutualTlsAuthentication = Lens.lens (\DomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@DomainName' {} a -> s {mutualTlsAuthentication = a} :: DomainName)

-- | The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
domainName_endpointConfiguration :: Lens.Lens' DomainName (Prelude.Maybe EndpointConfiguration)
domainName_endpointConfiguration = Lens.lens (\DomainName' {endpointConfiguration} -> endpointConfiguration) (\s@DomainName' {} a -> s {endpointConfiguration = a} :: DomainName)

-- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
-- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
-- more information, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-regional-api-custom-domain-create.html Set up a Regional Custom Domain Name>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#apigateway_region AWS Regions and Endpoints for API Gateway>.
domainName_distributionHostedZoneId :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_distributionHostedZoneId = Lens.lens (\DomainName' {distributionHostedZoneId} -> distributionHostedZoneId) (\s@DomainName' {} a -> s {distributionHostedZoneId = a} :: DomainName)

-- | The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
domainName_certificateArn :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_certificateArn = Lens.lens (\DomainName' {certificateArn} -> certificateArn) (\s@DomainName' {} a -> s {certificateArn = a} :: DomainName)

-- | An optional text message containing detailed information about status of
-- the DomainName migration.
domainName_domainNameStatusMessage :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_domainNameStatusMessage = Lens.lens (\DomainName' {domainNameStatusMessage} -> domainNameStatusMessage) (\s@DomainName' {} a -> s {domainNameStatusMessage = a} :: DomainName)

-- | The domain name of the Amazon CloudFront distribution associated with
-- this custom domain name for an edge-optimized endpoint. You set up this
-- association when adding a DNS record pointing the custom domain name to
-- this distribution name. For more information about CloudFront
-- distributions, see the
-- <https://aws.amazon.com/documentation/cloudfront/ Amazon CloudFront documentation>.
domainName_distributionDomainName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_distributionDomainName = Lens.lens (\DomainName' {distributionDomainName} -> distributionDomainName) (\s@DomainName' {} a -> s {distributionDomainName = a} :: DomainName)

-- | The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
domainName_certificateUploadDate :: Lens.Lens' DomainName (Prelude.Maybe Prelude.UTCTime)
domainName_certificateUploadDate = Lens.lens (\DomainName' {certificateUploadDate} -> certificateUploadDate) (\s@DomainName' {} a -> s {certificateUploadDate = a} :: DomainName) Prelude.. Lens.mapping Prelude._Time

-- | The custom domain name as an API host name, for example,
-- @my-api.example.com@.
domainName_domainName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_domainName = Lens.lens (\DomainName' {domainName} -> domainName) (\s@DomainName' {} a -> s {domainName = a} :: DomainName)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
domainName_tags :: Lens.Lens' DomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainName_tags = Lens.lens (\DomainName' {tags} -> tags) (\s@DomainName' {} a -> s {tags = a} :: DomainName) Prelude.. Lens.mapping Prelude._Coerce

-- | The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
domainName_securityPolicy :: Lens.Lens' DomainName (Prelude.Maybe SecurityPolicy)
domainName_securityPolicy = Lens.lens (\DomainName' {securityPolicy} -> securityPolicy) (\s@DomainName' {} a -> s {securityPolicy = a} :: DomainName)

-- | The status of the DomainName migration. The valid values are @AVAILABLE@
-- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
-- modified further until the existing operation is complete. If it is
-- @AVAILABLE@, the domain can be updated.
domainName_domainNameStatus :: Lens.Lens' DomainName (Prelude.Maybe DomainNameStatus)
domainName_domainNameStatus = Lens.lens (\DomainName' {domainNameStatus} -> domainNameStatus) (\s@DomainName' {} a -> s {domainNameStatus = a} :: DomainName)

-- | The reference to an AWS-managed certificate that will be used for
-- validating the regional domain name. AWS Certificate Manager is the only
-- supported source.
domainName_regionalCertificateArn :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalCertificateArn = Lens.lens (\DomainName' {regionalCertificateArn} -> regionalCertificateArn) (\s@DomainName' {} a -> s {regionalCertificateArn = a} :: DomainName)

-- | The name of the certificate that will be used by edge-optimized endpoint
-- for this domain name.
domainName_certificateName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_certificateName = Lens.lens (\DomainName' {certificateName} -> certificateName) (\s@DomainName' {} a -> s {certificateName = a} :: DomainName)

-- | The domain name associated with the regional endpoint for this custom
-- domain name. You set up this association by adding a DNS record that
-- points the custom domain name to this regional domain name. The regional
-- domain name is returned by API Gateway when you create a regional
-- endpoint.
domainName_regionalDomainName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalDomainName = Lens.lens (\DomainName' {regionalDomainName} -> regionalDomainName) (\s@DomainName' {} a -> s {regionalDomainName = a} :: DomainName)

instance Prelude.FromJSON DomainName where
  parseJSON =
    Prelude.withObject
      "DomainName"
      ( \x ->
          DomainName'
            Prelude.<$> (x Prelude..:? "regionalHostedZoneId")
            Prelude.<*> (x Prelude..:? "regionalCertificateName")
            Prelude.<*> (x Prelude..:? "mutualTlsAuthentication")
            Prelude.<*> (x Prelude..:? "endpointConfiguration")
            Prelude.<*> (x Prelude..:? "distributionHostedZoneId")
            Prelude.<*> (x Prelude..:? "certificateArn")
            Prelude.<*> (x Prelude..:? "domainNameStatusMessage")
            Prelude.<*> (x Prelude..:? "distributionDomainName")
            Prelude.<*> (x Prelude..:? "certificateUploadDate")
            Prelude.<*> (x Prelude..:? "domainName")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "securityPolicy")
            Prelude.<*> (x Prelude..:? "domainNameStatus")
            Prelude.<*> (x Prelude..:? "regionalCertificateArn")
            Prelude.<*> (x Prelude..:? "certificateName")
            Prelude.<*> (x Prelude..:? "regionalDomainName")
      )

instance Prelude.Hashable DomainName

instance Prelude.NFData DomainName
