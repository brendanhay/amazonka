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
-- Module      : Amazonka.ApiGatewayV2.Types.DomainNameConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.DomainNameConfiguration where

import Amazonka.ApiGatewayV2.Types.DomainNameStatus
import Amazonka.ApiGatewayV2.Types.EndpointType
import Amazonka.ApiGatewayV2.Types.SecurityPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The domain name configuration.
--
-- /See:/ 'newDomainNameConfiguration' smart constructor.
data DomainNameConfiguration = DomainNameConfiguration'
  { -- | A domain name for the API.
    apiGatewayDomainName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the public certificate issued by ACM to validate ownership of
    -- your custom domain. Only required when configuring mutual TLS and using
    -- an ACM imported or private CA certificate ARN as the
    -- regionalCertificateArn
    ownershipVerificationCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the certificate that will be used by the
    -- edge-optimized endpoint for this domain name.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Route 53 Hosted Zone ID of the endpoint.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | An AWS-managed certificate that will be used by the edge-optimized
    -- endpoint for this domain name. AWS Certificate Manager is the only
    -- supported source.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The endpoint type.
    endpointType :: Prelude.Maybe EndpointType,
    -- | The Transport Layer Security (TLS) version of the security policy for
    -- this domain name. The valid values are TLS_1_0 and TLS_1_2.
    securityPolicy :: Prelude.Maybe SecurityPolicy,
    -- | The timestamp when the certificate that was used by edge-optimized
    -- endpoint for this domain name was uploaded.
    certificateUploadDate :: Prelude.Maybe Core.POSIX,
    -- | An optional text message containing detailed information about status of
    -- the domain name migration.
    domainNameStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the domain name migration. The valid values are AVAILABLE,
    -- UPDATING, PENDING_CERTIFICATE_REIMPORT, and
    -- PENDING_OWNERSHIP_VERIFICATION. If the status is UPDATING, the domain
    -- cannot be modified further until the existing operation is complete. If
    -- it is AVAILABLE, the domain can be updated.
    domainNameStatus :: Prelude.Maybe DomainNameStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainNameConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayDomainName', 'domainNameConfiguration_apiGatewayDomainName' - A domain name for the API.
--
-- 'ownershipVerificationCertificateArn', 'domainNameConfiguration_ownershipVerificationCertificateArn' - The ARN of the public certificate issued by ACM to validate ownership of
-- your custom domain. Only required when configuring mutual TLS and using
-- an ACM imported or private CA certificate ARN as the
-- regionalCertificateArn
--
-- 'certificateName', 'domainNameConfiguration_certificateName' - The user-friendly name of the certificate that will be used by the
-- edge-optimized endpoint for this domain name.
--
-- 'hostedZoneId', 'domainNameConfiguration_hostedZoneId' - The Amazon Route 53 Hosted Zone ID of the endpoint.
--
-- 'certificateArn', 'domainNameConfiguration_certificateArn' - An AWS-managed certificate that will be used by the edge-optimized
-- endpoint for this domain name. AWS Certificate Manager is the only
-- supported source.
--
-- 'endpointType', 'domainNameConfiguration_endpointType' - The endpoint type.
--
-- 'securityPolicy', 'domainNameConfiguration_securityPolicy' - The Transport Layer Security (TLS) version of the security policy for
-- this domain name. The valid values are TLS_1_0 and TLS_1_2.
--
-- 'certificateUploadDate', 'domainNameConfiguration_certificateUploadDate' - The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
--
-- 'domainNameStatusMessage', 'domainNameConfiguration_domainNameStatusMessage' - An optional text message containing detailed information about status of
-- the domain name migration.
--
-- 'domainNameStatus', 'domainNameConfiguration_domainNameStatus' - The status of the domain name migration. The valid values are AVAILABLE,
-- UPDATING, PENDING_CERTIFICATE_REIMPORT, and
-- PENDING_OWNERSHIP_VERIFICATION. If the status is UPDATING, the domain
-- cannot be modified further until the existing operation is complete. If
-- it is AVAILABLE, the domain can be updated.
newDomainNameConfiguration ::
  DomainNameConfiguration
newDomainNameConfiguration =
  DomainNameConfiguration'
    { apiGatewayDomainName =
        Prelude.Nothing,
      ownershipVerificationCertificateArn =
        Prelude.Nothing,
      certificateName = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      securityPolicy = Prelude.Nothing,
      certificateUploadDate = Prelude.Nothing,
      domainNameStatusMessage = Prelude.Nothing,
      domainNameStatus = Prelude.Nothing
    }

-- | A domain name for the API.
domainNameConfiguration_apiGatewayDomainName :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.Text)
domainNameConfiguration_apiGatewayDomainName = Lens.lens (\DomainNameConfiguration' {apiGatewayDomainName} -> apiGatewayDomainName) (\s@DomainNameConfiguration' {} a -> s {apiGatewayDomainName = a} :: DomainNameConfiguration)

-- | The ARN of the public certificate issued by ACM to validate ownership of
-- your custom domain. Only required when configuring mutual TLS and using
-- an ACM imported or private CA certificate ARN as the
-- regionalCertificateArn
domainNameConfiguration_ownershipVerificationCertificateArn :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.Text)
domainNameConfiguration_ownershipVerificationCertificateArn = Lens.lens (\DomainNameConfiguration' {ownershipVerificationCertificateArn} -> ownershipVerificationCertificateArn) (\s@DomainNameConfiguration' {} a -> s {ownershipVerificationCertificateArn = a} :: DomainNameConfiguration)

-- | The user-friendly name of the certificate that will be used by the
-- edge-optimized endpoint for this domain name.
domainNameConfiguration_certificateName :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.Text)
domainNameConfiguration_certificateName = Lens.lens (\DomainNameConfiguration' {certificateName} -> certificateName) (\s@DomainNameConfiguration' {} a -> s {certificateName = a} :: DomainNameConfiguration)

-- | The Amazon Route 53 Hosted Zone ID of the endpoint.
domainNameConfiguration_hostedZoneId :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.Text)
domainNameConfiguration_hostedZoneId = Lens.lens (\DomainNameConfiguration' {hostedZoneId} -> hostedZoneId) (\s@DomainNameConfiguration' {} a -> s {hostedZoneId = a} :: DomainNameConfiguration)

-- | An AWS-managed certificate that will be used by the edge-optimized
-- endpoint for this domain name. AWS Certificate Manager is the only
-- supported source.
domainNameConfiguration_certificateArn :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.Text)
domainNameConfiguration_certificateArn = Lens.lens (\DomainNameConfiguration' {certificateArn} -> certificateArn) (\s@DomainNameConfiguration' {} a -> s {certificateArn = a} :: DomainNameConfiguration)

-- | The endpoint type.
domainNameConfiguration_endpointType :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe EndpointType)
domainNameConfiguration_endpointType = Lens.lens (\DomainNameConfiguration' {endpointType} -> endpointType) (\s@DomainNameConfiguration' {} a -> s {endpointType = a} :: DomainNameConfiguration)

-- | The Transport Layer Security (TLS) version of the security policy for
-- this domain name. The valid values are TLS_1_0 and TLS_1_2.
domainNameConfiguration_securityPolicy :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe SecurityPolicy)
domainNameConfiguration_securityPolicy = Lens.lens (\DomainNameConfiguration' {securityPolicy} -> securityPolicy) (\s@DomainNameConfiguration' {} a -> s {securityPolicy = a} :: DomainNameConfiguration)

-- | The timestamp when the certificate that was used by edge-optimized
-- endpoint for this domain name was uploaded.
domainNameConfiguration_certificateUploadDate :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.UTCTime)
domainNameConfiguration_certificateUploadDate = Lens.lens (\DomainNameConfiguration' {certificateUploadDate} -> certificateUploadDate) (\s@DomainNameConfiguration' {} a -> s {certificateUploadDate = a} :: DomainNameConfiguration) Prelude.. Lens.mapping Core._Time

-- | An optional text message containing detailed information about status of
-- the domain name migration.
domainNameConfiguration_domainNameStatusMessage :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe Prelude.Text)
domainNameConfiguration_domainNameStatusMessage = Lens.lens (\DomainNameConfiguration' {domainNameStatusMessage} -> domainNameStatusMessage) (\s@DomainNameConfiguration' {} a -> s {domainNameStatusMessage = a} :: DomainNameConfiguration)

-- | The status of the domain name migration. The valid values are AVAILABLE,
-- UPDATING, PENDING_CERTIFICATE_REIMPORT, and
-- PENDING_OWNERSHIP_VERIFICATION. If the status is UPDATING, the domain
-- cannot be modified further until the existing operation is complete. If
-- it is AVAILABLE, the domain can be updated.
domainNameConfiguration_domainNameStatus :: Lens.Lens' DomainNameConfiguration (Prelude.Maybe DomainNameStatus)
domainNameConfiguration_domainNameStatus = Lens.lens (\DomainNameConfiguration' {domainNameStatus} -> domainNameStatus) (\s@DomainNameConfiguration' {} a -> s {domainNameStatus = a} :: DomainNameConfiguration)

instance Core.FromJSON DomainNameConfiguration where
  parseJSON =
    Core.withObject
      "DomainNameConfiguration"
      ( \x ->
          DomainNameConfiguration'
            Prelude.<$> (x Core..:? "apiGatewayDomainName")
            Prelude.<*> (x Core..:? "ownershipVerificationCertificateArn")
            Prelude.<*> (x Core..:? "certificateName")
            Prelude.<*> (x Core..:? "hostedZoneId")
            Prelude.<*> (x Core..:? "certificateArn")
            Prelude.<*> (x Core..:? "endpointType")
            Prelude.<*> (x Core..:? "securityPolicy")
            Prelude.<*> (x Core..:? "certificateUploadDate")
            Prelude.<*> (x Core..:? "domainNameStatusMessage")
            Prelude.<*> (x Core..:? "domainNameStatus")
      )

instance Prelude.Hashable DomainNameConfiguration

instance Prelude.NFData DomainNameConfiguration

instance Core.ToJSON DomainNameConfiguration where
  toJSON DomainNameConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("apiGatewayDomainName" Core..=)
              Prelude.<$> apiGatewayDomainName,
            ("ownershipVerificationCertificateArn" Core..=)
              Prelude.<$> ownershipVerificationCertificateArn,
            ("certificateName" Core..=)
              Prelude.<$> certificateName,
            ("hostedZoneId" Core..=) Prelude.<$> hostedZoneId,
            ("certificateArn" Core..=)
              Prelude.<$> certificateArn,
            ("endpointType" Core..=) Prelude.<$> endpointType,
            ("securityPolicy" Core..=)
              Prelude.<$> securityPolicy,
            ("certificateUploadDate" Core..=)
              Prelude.<$> certificateUploadDate,
            ("domainNameStatusMessage" Core..=)
              Prelude.<$> domainNameStatusMessage,
            ("domainNameStatus" Core..=)
              Prelude.<$> domainNameStatus
          ]
      )
