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
-- Module      : Amazonka.APIGateway.Types.DomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.DomainName where

import Amazonka.APIGateway.Types.DomainNameStatus
import Amazonka.APIGateway.Types.EndpointConfiguration
import Amazonka.APIGateway.Types.MutualTlsAuthentication
import Amazonka.APIGateway.Types.SecurityPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a custom domain name as a user-friendly host name of an API
-- (RestApi).
--
-- /See:/ 'newDomainName' smart constructor.
data DomainName = DomainName'
  { -- | The reference to an AWS-managed certificate that will be used by
    -- edge-optimized endpoint for this domain name. AWS Certificate Manager is
    -- the only supported source.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the certificate that will be used by edge-optimized endpoint
    -- for this domain name.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the certificate that was used by edge-optimized
    -- endpoint for this domain name was uploaded.
    certificateUploadDate :: Prelude.Maybe Data.POSIX,
    -- | The domain name of the Amazon CloudFront distribution associated with
    -- this custom domain name for an edge-optimized endpoint. You set up this
    -- association when adding a DNS record pointing the custom domain name to
    -- this distribution name. For more information about CloudFront
    -- distributions, see the Amazon CloudFront documentation.
    distributionDomainName :: Prelude.Maybe Prelude.Text,
    -- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
    -- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
    -- more information, see Set up a Regional Custom Domain Name and AWS
    -- Regions and Endpoints for API Gateway.
    distributionHostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name as an API host name, for example,
    -- @my-api.example.com@.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The status of the DomainName migration. The valid values are @AVAILABLE@
    -- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
    -- modified further until the existing operation is complete. If it is
    -- @AVAILABLE@, the domain can be updated.
    domainNameStatus :: Prelude.Maybe DomainNameStatus,
    -- | An optional text message containing detailed information about status of
    -- the DomainName migration.
    domainNameStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The endpoint configuration of this DomainName showing the endpoint types
    -- of the domain name.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | The mutual TLS authentication configuration for a custom domain name. If
    -- specified, API Gateway performs two-way authentication between the
    -- client and the server. Clients must present a trusted certificate to
    -- access your API.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthentication,
    -- | The ARN of the public certificate issued by ACM to validate ownership of
    -- your custom domain. Only required when configuring mutual TLS and using
    -- an ACM imported or private CA certificate ARN as the
    -- regionalCertificateArn.
    ownershipVerificationCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The reference to an AWS-managed certificate that will be used for
    -- validating the regional domain name. AWS Certificate Manager is the only
    -- supported source.
    regionalCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the certificate that will be used for validating the
    -- regional domain name.
    regionalCertificateName :: Prelude.Maybe Prelude.Text,
    -- | The domain name associated with the regional endpoint for this custom
    -- domain name. You set up this association by adding a DNS record that
    -- points the custom domain name to this regional domain name. The regional
    -- domain name is returned by API Gateway when you create a regional
    -- endpoint.
    regionalDomainName :: Prelude.Maybe Prelude.Text,
    -- | The region-specific Amazon Route 53 Hosted Zone ID of the regional
    -- endpoint. For more information, see Set up a Regional Custom Domain Name
    -- and AWS Regions and Endpoints for API Gateway.
    regionalHostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Transport Layer Security (TLS) version + cipher suite for this
    -- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
    securityPolicy :: Prelude.Maybe SecurityPolicy,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'domainName_certificateArn' - The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
--
-- 'certificateName', 'domainName_certificateName' - The name of the certificate that will be used by edge-optimized endpoint
-- for this domain name.
--
-- 'certificateUploadDate', 'domainName_certificateUploadDate' - The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
--
-- 'distributionDomainName', 'domainName_distributionDomainName' - The domain name of the Amazon CloudFront distribution associated with
-- this custom domain name for an edge-optimized endpoint. You set up this
-- association when adding a DNS record pointing the custom domain name to
-- this distribution name. For more information about CloudFront
-- distributions, see the Amazon CloudFront documentation.
--
-- 'distributionHostedZoneId', 'domainName_distributionHostedZoneId' - The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
-- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
-- more information, see Set up a Regional Custom Domain Name and AWS
-- Regions and Endpoints for API Gateway.
--
-- 'domainName', 'domainName_domainName' - The custom domain name as an API host name, for example,
-- @my-api.example.com@.
--
-- 'domainNameStatus', 'domainName_domainNameStatus' - The status of the DomainName migration. The valid values are @AVAILABLE@
-- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
-- modified further until the existing operation is complete. If it is
-- @AVAILABLE@, the domain can be updated.
--
-- 'domainNameStatusMessage', 'domainName_domainNameStatusMessage' - An optional text message containing detailed information about status of
-- the DomainName migration.
--
-- 'endpointConfiguration', 'domainName_endpointConfiguration' - The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
--
-- 'mutualTlsAuthentication', 'domainName_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name. If
-- specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your API.
--
-- 'ownershipVerificationCertificateArn', 'domainName_ownershipVerificationCertificateArn' - The ARN of the public certificate issued by ACM to validate ownership of
-- your custom domain. Only required when configuring mutual TLS and using
-- an ACM imported or private CA certificate ARN as the
-- regionalCertificateArn.
--
-- 'regionalCertificateArn', 'domainName_regionalCertificateArn' - The reference to an AWS-managed certificate that will be used for
-- validating the regional domain name. AWS Certificate Manager is the only
-- supported source.
--
-- 'regionalCertificateName', 'domainName_regionalCertificateName' - The name of the certificate that will be used for validating the
-- regional domain name.
--
-- 'regionalDomainName', 'domainName_regionalDomainName' - The domain name associated with the regional endpoint for this custom
-- domain name. You set up this association by adding a DNS record that
-- points the custom domain name to this regional domain name. The regional
-- domain name is returned by API Gateway when you create a regional
-- endpoint.
--
-- 'regionalHostedZoneId', 'domainName_regionalHostedZoneId' - The region-specific Amazon Route 53 Hosted Zone ID of the regional
-- endpoint. For more information, see Set up a Regional Custom Domain Name
-- and AWS Regions and Endpoints for API Gateway.
--
-- 'securityPolicy', 'domainName_securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
--
-- 'tags', 'domainName_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
newDomainName ::
  DomainName
newDomainName =
  DomainName'
    { certificateArn = Prelude.Nothing,
      certificateName = Prelude.Nothing,
      certificateUploadDate = Prelude.Nothing,
      distributionDomainName = Prelude.Nothing,
      distributionHostedZoneId = Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainNameStatus = Prelude.Nothing,
      domainNameStatusMessage = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      ownershipVerificationCertificateArn =
        Prelude.Nothing,
      regionalCertificateArn = Prelude.Nothing,
      regionalCertificateName = Prelude.Nothing,
      regionalDomainName = Prelude.Nothing,
      regionalHostedZoneId = Prelude.Nothing,
      securityPolicy = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
domainName_certificateArn :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_certificateArn = Lens.lens (\DomainName' {certificateArn} -> certificateArn) (\s@DomainName' {} a -> s {certificateArn = a} :: DomainName)

-- | The name of the certificate that will be used by edge-optimized endpoint
-- for this domain name.
domainName_certificateName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_certificateName = Lens.lens (\DomainName' {certificateName} -> certificateName) (\s@DomainName' {} a -> s {certificateName = a} :: DomainName)

-- | The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
domainName_certificateUploadDate :: Lens.Lens' DomainName (Prelude.Maybe Prelude.UTCTime)
domainName_certificateUploadDate = Lens.lens (\DomainName' {certificateUploadDate} -> certificateUploadDate) (\s@DomainName' {} a -> s {certificateUploadDate = a} :: DomainName) Prelude.. Lens.mapping Data._Time

-- | The domain name of the Amazon CloudFront distribution associated with
-- this custom domain name for an edge-optimized endpoint. You set up this
-- association when adding a DNS record pointing the custom domain name to
-- this distribution name. For more information about CloudFront
-- distributions, see the Amazon CloudFront documentation.
domainName_distributionDomainName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_distributionDomainName = Lens.lens (\DomainName' {distributionDomainName} -> distributionDomainName) (\s@DomainName' {} a -> s {distributionDomainName = a} :: DomainName)

-- | The region-agnostic Amazon Route 53 Hosted Zone ID of the edge-optimized
-- endpoint. The valid value is @Z2FDTNDATAQYW2@ for all the regions. For
-- more information, see Set up a Regional Custom Domain Name and AWS
-- Regions and Endpoints for API Gateway.
domainName_distributionHostedZoneId :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_distributionHostedZoneId = Lens.lens (\DomainName' {distributionHostedZoneId} -> distributionHostedZoneId) (\s@DomainName' {} a -> s {distributionHostedZoneId = a} :: DomainName)

-- | The custom domain name as an API host name, for example,
-- @my-api.example.com@.
domainName_domainName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_domainName = Lens.lens (\DomainName' {domainName} -> domainName) (\s@DomainName' {} a -> s {domainName = a} :: DomainName)

-- | The status of the DomainName migration. The valid values are @AVAILABLE@
-- and @UPDATING@. If the status is @UPDATING@, the domain cannot be
-- modified further until the existing operation is complete. If it is
-- @AVAILABLE@, the domain can be updated.
domainName_domainNameStatus :: Lens.Lens' DomainName (Prelude.Maybe DomainNameStatus)
domainName_domainNameStatus = Lens.lens (\DomainName' {domainNameStatus} -> domainNameStatus) (\s@DomainName' {} a -> s {domainNameStatus = a} :: DomainName)

-- | An optional text message containing detailed information about status of
-- the DomainName migration.
domainName_domainNameStatusMessage :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_domainNameStatusMessage = Lens.lens (\DomainName' {domainNameStatusMessage} -> domainNameStatusMessage) (\s@DomainName' {} a -> s {domainNameStatusMessage = a} :: DomainName)

-- | The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
domainName_endpointConfiguration :: Lens.Lens' DomainName (Prelude.Maybe EndpointConfiguration)
domainName_endpointConfiguration = Lens.lens (\DomainName' {endpointConfiguration} -> endpointConfiguration) (\s@DomainName' {} a -> s {endpointConfiguration = a} :: DomainName)

-- | The mutual TLS authentication configuration for a custom domain name. If
-- specified, API Gateway performs two-way authentication between the
-- client and the server. Clients must present a trusted certificate to
-- access your API.
domainName_mutualTlsAuthentication :: Lens.Lens' DomainName (Prelude.Maybe MutualTlsAuthentication)
domainName_mutualTlsAuthentication = Lens.lens (\DomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@DomainName' {} a -> s {mutualTlsAuthentication = a} :: DomainName)

-- | The ARN of the public certificate issued by ACM to validate ownership of
-- your custom domain. Only required when configuring mutual TLS and using
-- an ACM imported or private CA certificate ARN as the
-- regionalCertificateArn.
domainName_ownershipVerificationCertificateArn :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_ownershipVerificationCertificateArn = Lens.lens (\DomainName' {ownershipVerificationCertificateArn} -> ownershipVerificationCertificateArn) (\s@DomainName' {} a -> s {ownershipVerificationCertificateArn = a} :: DomainName)

-- | The reference to an AWS-managed certificate that will be used for
-- validating the regional domain name. AWS Certificate Manager is the only
-- supported source.
domainName_regionalCertificateArn :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalCertificateArn = Lens.lens (\DomainName' {regionalCertificateArn} -> regionalCertificateArn) (\s@DomainName' {} a -> s {regionalCertificateArn = a} :: DomainName)

-- | The name of the certificate that will be used for validating the
-- regional domain name.
domainName_regionalCertificateName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalCertificateName = Lens.lens (\DomainName' {regionalCertificateName} -> regionalCertificateName) (\s@DomainName' {} a -> s {regionalCertificateName = a} :: DomainName)

-- | The domain name associated with the regional endpoint for this custom
-- domain name. You set up this association by adding a DNS record that
-- points the custom domain name to this regional domain name. The regional
-- domain name is returned by API Gateway when you create a regional
-- endpoint.
domainName_regionalDomainName :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalDomainName = Lens.lens (\DomainName' {regionalDomainName} -> regionalDomainName) (\s@DomainName' {} a -> s {regionalDomainName = a} :: DomainName)

-- | The region-specific Amazon Route 53 Hosted Zone ID of the regional
-- endpoint. For more information, see Set up a Regional Custom Domain Name
-- and AWS Regions and Endpoints for API Gateway.
domainName_regionalHostedZoneId :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_regionalHostedZoneId = Lens.lens (\DomainName' {regionalHostedZoneId} -> regionalHostedZoneId) (\s@DomainName' {} a -> s {regionalHostedZoneId = a} :: DomainName)

-- | The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
domainName_securityPolicy :: Lens.Lens' DomainName (Prelude.Maybe SecurityPolicy)
domainName_securityPolicy = Lens.lens (\DomainName' {securityPolicy} -> securityPolicy) (\s@DomainName' {} a -> s {securityPolicy = a} :: DomainName)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
domainName_tags :: Lens.Lens' DomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainName_tags = Lens.lens (\DomainName' {tags} -> tags) (\s@DomainName' {} a -> s {tags = a} :: DomainName) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DomainName where
  parseJSON =
    Data.withObject
      "DomainName"
      ( \x ->
          DomainName'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "certificateName")
            Prelude.<*> (x Data..:? "certificateUploadDate")
            Prelude.<*> (x Data..:? "distributionDomainName")
            Prelude.<*> (x Data..:? "distributionHostedZoneId")
            Prelude.<*> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "domainNameStatus")
            Prelude.<*> (x Data..:? "domainNameStatusMessage")
            Prelude.<*> (x Data..:? "endpointConfiguration")
            Prelude.<*> (x Data..:? "mutualTlsAuthentication")
            Prelude.<*> (x Data..:? "ownershipVerificationCertificateArn")
            Prelude.<*> (x Data..:? "regionalCertificateArn")
            Prelude.<*> (x Data..:? "regionalCertificateName")
            Prelude.<*> (x Data..:? "regionalDomainName")
            Prelude.<*> (x Data..:? "regionalHostedZoneId")
            Prelude.<*> (x Data..:? "securityPolicy")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DomainName where
  hashWithSalt _salt DomainName' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateName
      `Prelude.hashWithSalt` certificateUploadDate
      `Prelude.hashWithSalt` distributionDomainName
      `Prelude.hashWithSalt` distributionHostedZoneId
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainNameStatus
      `Prelude.hashWithSalt` domainNameStatusMessage
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` ownershipVerificationCertificateArn
      `Prelude.hashWithSalt` regionalCertificateArn
      `Prelude.hashWithSalt` regionalCertificateName
      `Prelude.hashWithSalt` regionalDomainName
      `Prelude.hashWithSalt` regionalHostedZoneId
      `Prelude.hashWithSalt` securityPolicy
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DomainName where
  rnf DomainName' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf certificateUploadDate
      `Prelude.seq` Prelude.rnf distributionDomainName
      `Prelude.seq` Prelude.rnf distributionHostedZoneId
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainNameStatus
      `Prelude.seq` Prelude.rnf domainNameStatusMessage
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf ownershipVerificationCertificateArn
      `Prelude.seq` Prelude.rnf regionalCertificateArn
      `Prelude.seq` Prelude.rnf regionalCertificateName
      `Prelude.seq` Prelude.rnf regionalDomainName
      `Prelude.seq` Prelude.rnf regionalHostedZoneId
      `Prelude.seq` Prelude.rnf securityPolicy
      `Prelude.seq` Prelude.rnf tags
