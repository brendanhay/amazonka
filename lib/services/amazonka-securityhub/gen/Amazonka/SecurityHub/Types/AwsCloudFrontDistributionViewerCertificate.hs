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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the TLS\/SSL configuration that the
-- CloudFront distribution uses to communicate with viewers.
--
-- /See:/ 'newAwsCloudFrontDistributionViewerCertificate' smart constructor.
data AwsCloudFrontDistributionViewerCertificate = AwsCloudFrontDistributionViewerCertificate'
  { -- | The identifier of the IAM certificate. Used if the certificate is stored
    -- in IAM. If you provide @IamCertificateId@, then you also must provide
    -- @MinimumProtocolVersion@ and @SslSupportMethod@.
    iamCertificateId :: Prelude.Maybe Prelude.Text,
    -- | Whether the distribution uses the CloudFront domain name. If set to
    -- @false@, then you provide either @AcmCertificateArn@ or
    -- @IamCertificateId@.
    cloudFrontDefaultCertificate :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the certificate. Note that in CloudFront, this
    -- attribute is deprecated.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The security policy that CloudFront uses for HTTPS connections with
    -- viewers. If @SslSupportMethod@ is @sni-only@, then
    -- @MinimumProtocolVersion@ must be @TLSv1@ or higher.
    minimumProtocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the ACM certificate. Used if the certificate is stored in
    -- ACM. If you provide an ACM certificate ARN, you must also provide
    -- @MinimumCertificateVersion@ and @SslSupportMethod@.
    acmCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The viewers that the distribution accepts HTTPS connections from.
    sslSupportMethod :: Prelude.Maybe Prelude.Text,
    -- | The source of the certificate identified by @Certificate@. Note that in
    -- CloudFront, this attribute is deprecated.
    certificateSource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionViewerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamCertificateId', 'awsCloudFrontDistributionViewerCertificate_iamCertificateId' - The identifier of the IAM certificate. Used if the certificate is stored
-- in IAM. If you provide @IamCertificateId@, then you also must provide
-- @MinimumProtocolVersion@ and @SslSupportMethod@.
--
-- 'cloudFrontDefaultCertificate', 'awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate' - Whether the distribution uses the CloudFront domain name. If set to
-- @false@, then you provide either @AcmCertificateArn@ or
-- @IamCertificateId@.
--
-- 'certificate', 'awsCloudFrontDistributionViewerCertificate_certificate' - The identifier of the certificate. Note that in CloudFront, this
-- attribute is deprecated.
--
-- 'minimumProtocolVersion', 'awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion' - The security policy that CloudFront uses for HTTPS connections with
-- viewers. If @SslSupportMethod@ is @sni-only@, then
-- @MinimumProtocolVersion@ must be @TLSv1@ or higher.
--
-- 'acmCertificateArn', 'awsCloudFrontDistributionViewerCertificate_acmCertificateArn' - The ARN of the ACM certificate. Used if the certificate is stored in
-- ACM. If you provide an ACM certificate ARN, you must also provide
-- @MinimumCertificateVersion@ and @SslSupportMethod@.
--
-- 'sslSupportMethod', 'awsCloudFrontDistributionViewerCertificate_sslSupportMethod' - The viewers that the distribution accepts HTTPS connections from.
--
-- 'certificateSource', 'awsCloudFrontDistributionViewerCertificate_certificateSource' - The source of the certificate identified by @Certificate@. Note that in
-- CloudFront, this attribute is deprecated.
newAwsCloudFrontDistributionViewerCertificate ::
  AwsCloudFrontDistributionViewerCertificate
newAwsCloudFrontDistributionViewerCertificate =
  AwsCloudFrontDistributionViewerCertificate'
    { iamCertificateId =
        Prelude.Nothing,
      cloudFrontDefaultCertificate =
        Prelude.Nothing,
      certificate = Prelude.Nothing,
      minimumProtocolVersion =
        Prelude.Nothing,
      acmCertificateArn =
        Prelude.Nothing,
      sslSupportMethod =
        Prelude.Nothing,
      certificateSource =
        Prelude.Nothing
    }

-- | The identifier of the IAM certificate. Used if the certificate is stored
-- in IAM. If you provide @IamCertificateId@, then you also must provide
-- @MinimumProtocolVersion@ and @SslSupportMethod@.
awsCloudFrontDistributionViewerCertificate_iamCertificateId :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_iamCertificateId = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {iamCertificateId} -> iamCertificateId) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {iamCertificateId = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | Whether the distribution uses the CloudFront domain name. If set to
-- @false@, then you provide either @AcmCertificateArn@ or
-- @IamCertificateId@.
awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Bool)
awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {cloudFrontDefaultCertificate} -> cloudFrontDefaultCertificate) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {cloudFrontDefaultCertificate = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The identifier of the certificate. Note that in CloudFront, this
-- attribute is deprecated.
awsCloudFrontDistributionViewerCertificate_certificate :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_certificate = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {certificate} -> certificate) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {certificate = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The security policy that CloudFront uses for HTTPS connections with
-- viewers. If @SslSupportMethod@ is @sni-only@, then
-- @MinimumProtocolVersion@ must be @TLSv1@ or higher.
awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {minimumProtocolVersion} -> minimumProtocolVersion) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {minimumProtocolVersion = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The ARN of the ACM certificate. Used if the certificate is stored in
-- ACM. If you provide an ACM certificate ARN, you must also provide
-- @MinimumCertificateVersion@ and @SslSupportMethod@.
awsCloudFrontDistributionViewerCertificate_acmCertificateArn :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_acmCertificateArn = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {acmCertificateArn} -> acmCertificateArn) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {acmCertificateArn = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The viewers that the distribution accepts HTTPS connections from.
awsCloudFrontDistributionViewerCertificate_sslSupportMethod :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_sslSupportMethod = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {sslSupportMethod} -> sslSupportMethod) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {sslSupportMethod = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The source of the certificate identified by @Certificate@. Note that in
-- CloudFront, this attribute is deprecated.
awsCloudFrontDistributionViewerCertificate_certificateSource :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_certificateSource = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {certificateSource} -> certificateSource) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {certificateSource = a} :: AwsCloudFrontDistributionViewerCertificate)

instance
  Core.FromJSON
    AwsCloudFrontDistributionViewerCertificate
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionViewerCertificate"
      ( \x ->
          AwsCloudFrontDistributionViewerCertificate'
            Prelude.<$> (x Core..:? "IamCertificateId")
              Prelude.<*> (x Core..:? "CloudFrontDefaultCertificate")
              Prelude.<*> (x Core..:? "Certificate")
              Prelude.<*> (x Core..:? "MinimumProtocolVersion")
              Prelude.<*> (x Core..:? "AcmCertificateArn")
              Prelude.<*> (x Core..:? "SslSupportMethod")
              Prelude.<*> (x Core..:? "CertificateSource")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionViewerCertificate
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionViewerCertificate' {..} =
      _salt `Prelude.hashWithSalt` iamCertificateId
        `Prelude.hashWithSalt` cloudFrontDefaultCertificate
        `Prelude.hashWithSalt` certificate
        `Prelude.hashWithSalt` minimumProtocolVersion
        `Prelude.hashWithSalt` acmCertificateArn
        `Prelude.hashWithSalt` sslSupportMethod
        `Prelude.hashWithSalt` certificateSource

instance
  Prelude.NFData
    AwsCloudFrontDistributionViewerCertificate
  where
  rnf AwsCloudFrontDistributionViewerCertificate' {..} =
    Prelude.rnf iamCertificateId
      `Prelude.seq` Prelude.rnf cloudFrontDefaultCertificate
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf minimumProtocolVersion
      `Prelude.seq` Prelude.rnf acmCertificateArn
      `Prelude.seq` Prelude.rnf sslSupportMethod
      `Prelude.seq` Prelude.rnf certificateSource

instance
  Core.ToJSON
    AwsCloudFrontDistributionViewerCertificate
  where
  toJSON
    AwsCloudFrontDistributionViewerCertificate' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("IamCertificateId" Core..=)
                Prelude.<$> iamCertificateId,
              ("CloudFrontDefaultCertificate" Core..=)
                Prelude.<$> cloudFrontDefaultCertificate,
              ("Certificate" Core..=) Prelude.<$> certificate,
              ("MinimumProtocolVersion" Core..=)
                Prelude.<$> minimumProtocolVersion,
              ("AcmCertificateArn" Core..=)
                Prelude.<$> acmCertificateArn,
              ("SslSupportMethod" Core..=)
                Prelude.<$> sslSupportMethod,
              ("CertificateSource" Core..=)
                Prelude.<$> certificateSource
            ]
        )
