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
-- Module      : Network.AWS.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
--
-- /See:/ 'newAwsCloudFrontDistributionViewerCertificate' smart constructor.
data AwsCloudFrontDistributionViewerCertificate = AwsCloudFrontDistributionViewerCertificate'
  { -- | The viewers that the distribution accepts HTTPS connections from.
    sslSupportMethod :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the ACM certificate. Used if the certificate is stored in
    -- ACM. If you provide an ACM certificate ARN, you must also provide
    -- @MinimumCertificateVersion@ and @SslSupportMethod@.
    acmCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The source of the certificate identified by @Certificate@. Note that in
    -- CloudFront, this attribute is deprecated.
    certificateSource :: Prelude.Maybe Prelude.Text,
    -- | The security policy that CloudFront uses for HTTPS connections with
    -- viewers. If @SslSupportMethod@ is @sni-only@, then
    -- @MinimumProtocolVersion@ must be @TLSv1@ or higher.
    minimumProtocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the certificate. Note that in CloudFront, this
    -- attribute is deprecated.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the IAM certificate. Used if the certificate is stored
    -- in IAM. If you provide @IamCertificateId@, then you also must provide
    -- @MinimumProtocolVersion@ and @SslSupportMethod@.
    iamCertificateId :: Prelude.Maybe Prelude.Text,
    -- | Whether the distribution uses the CloudFront domain name. If set to
    -- @false@, then you provide either @AcmCertificateArn@ or
    -- @IamCertificateId@.
    cloudFrontDefaultCertificate :: Prelude.Maybe Prelude.Bool
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
-- 'sslSupportMethod', 'awsCloudFrontDistributionViewerCertificate_sslSupportMethod' - The viewers that the distribution accepts HTTPS connections from.
--
-- 'acmCertificateArn', 'awsCloudFrontDistributionViewerCertificate_acmCertificateArn' - The ARN of the ACM certificate. Used if the certificate is stored in
-- ACM. If you provide an ACM certificate ARN, you must also provide
-- @MinimumCertificateVersion@ and @SslSupportMethod@.
--
-- 'certificateSource', 'awsCloudFrontDistributionViewerCertificate_certificateSource' - The source of the certificate identified by @Certificate@. Note that in
-- CloudFront, this attribute is deprecated.
--
-- 'minimumProtocolVersion', 'awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion' - The security policy that CloudFront uses for HTTPS connections with
-- viewers. If @SslSupportMethod@ is @sni-only@, then
-- @MinimumProtocolVersion@ must be @TLSv1@ or higher.
--
-- 'certificate', 'awsCloudFrontDistributionViewerCertificate_certificate' - The identifier of the certificate. Note that in CloudFront, this
-- attribute is deprecated.
--
-- 'iamCertificateId', 'awsCloudFrontDistributionViewerCertificate_iamCertificateId' - The identifier of the IAM certificate. Used if the certificate is stored
-- in IAM. If you provide @IamCertificateId@, then you also must provide
-- @MinimumProtocolVersion@ and @SslSupportMethod@.
--
-- 'cloudFrontDefaultCertificate', 'awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate' - Whether the distribution uses the CloudFront domain name. If set to
-- @false@, then you provide either @AcmCertificateArn@ or
-- @IamCertificateId@.
newAwsCloudFrontDistributionViewerCertificate ::
  AwsCloudFrontDistributionViewerCertificate
newAwsCloudFrontDistributionViewerCertificate =
  AwsCloudFrontDistributionViewerCertificate'
    { sslSupportMethod =
        Prelude.Nothing,
      acmCertificateArn =
        Prelude.Nothing,
      certificateSource =
        Prelude.Nothing,
      minimumProtocolVersion =
        Prelude.Nothing,
      certificate = Prelude.Nothing,
      iamCertificateId =
        Prelude.Nothing,
      cloudFrontDefaultCertificate =
        Prelude.Nothing
    }

-- | The viewers that the distribution accepts HTTPS connections from.
awsCloudFrontDistributionViewerCertificate_sslSupportMethod :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_sslSupportMethod = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {sslSupportMethod} -> sslSupportMethod) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {sslSupportMethod = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The ARN of the ACM certificate. Used if the certificate is stored in
-- ACM. If you provide an ACM certificate ARN, you must also provide
-- @MinimumCertificateVersion@ and @SslSupportMethod@.
awsCloudFrontDistributionViewerCertificate_acmCertificateArn :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_acmCertificateArn = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {acmCertificateArn} -> acmCertificateArn) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {acmCertificateArn = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The source of the certificate identified by @Certificate@. Note that in
-- CloudFront, this attribute is deprecated.
awsCloudFrontDistributionViewerCertificate_certificateSource :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_certificateSource = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {certificateSource} -> certificateSource) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {certificateSource = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The security policy that CloudFront uses for HTTPS connections with
-- viewers. If @SslSupportMethod@ is @sni-only@, then
-- @MinimumProtocolVersion@ must be @TLSv1@ or higher.
awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {minimumProtocolVersion} -> minimumProtocolVersion) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {minimumProtocolVersion = a} :: AwsCloudFrontDistributionViewerCertificate)

-- | The identifier of the certificate. Note that in CloudFront, this
-- attribute is deprecated.
awsCloudFrontDistributionViewerCertificate_certificate :: Lens.Lens' AwsCloudFrontDistributionViewerCertificate (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionViewerCertificate_certificate = Lens.lens (\AwsCloudFrontDistributionViewerCertificate' {certificate} -> certificate) (\s@AwsCloudFrontDistributionViewerCertificate' {} a -> s {certificate = a} :: AwsCloudFrontDistributionViewerCertificate)

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

instance
  Core.FromJSON
    AwsCloudFrontDistributionViewerCertificate
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionViewerCertificate"
      ( \x ->
          AwsCloudFrontDistributionViewerCertificate'
            Prelude.<$> (x Core..:? "SslSupportMethod")
              Prelude.<*> (x Core..:? "AcmCertificateArn")
              Prelude.<*> (x Core..:? "CertificateSource")
              Prelude.<*> (x Core..:? "MinimumProtocolVersion")
              Prelude.<*> (x Core..:? "Certificate")
              Prelude.<*> (x Core..:? "IamCertificateId")
              Prelude.<*> (x Core..:? "CloudFrontDefaultCertificate")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionViewerCertificate

instance
  Prelude.NFData
    AwsCloudFrontDistributionViewerCertificate

instance
  Core.ToJSON
    AwsCloudFrontDistributionViewerCertificate
  where
  toJSON
    AwsCloudFrontDistributionViewerCertificate' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("SslSupportMethod" Core..=)
                Prelude.<$> sslSupportMethod,
              ("AcmCertificateArn" Core..=)
                Prelude.<$> acmCertificateArn,
              ("CertificateSource" Core..=)
                Prelude.<$> certificateSource,
              ("MinimumProtocolVersion" Core..=)
                Prelude.<$> minimumProtocolVersion,
              ("Certificate" Core..=) Prelude.<$> certificate,
              ("IamCertificateId" Core..=)
                Prelude.<$> iamCertificateId,
              ("CloudFrontDefaultCertificate" Core..=)
                Prelude.<$> cloudFrontDefaultCertificate
            ]
        )
