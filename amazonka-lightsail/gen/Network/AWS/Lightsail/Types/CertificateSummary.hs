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
-- Module      : Network.AWS.Lightsail.Types.CertificateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CertificateSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Certificate
import Network.AWS.Lightsail.Types.Tag

-- | Describes an Amazon Lightsail SSL\/TLS certificate.
--
-- /See:/ 'newCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Core.Maybe Core.Text,
    -- | The domain name of the certificate.
    domainName :: Core.Maybe Core.Text,
    -- | An object that describes a certificate in detail.
    certificateDetail :: Core.Maybe Certificate,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | The name of the certificate.
    certificateName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'certificateSummary_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'domainName', 'certificateSummary_domainName' - The domain name of the certificate.
--
-- 'certificateDetail', 'certificateSummary_certificateDetail' - An object that describes a certificate in detail.
--
-- 'tags', 'certificateSummary_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'certificateName', 'certificateSummary_certificateName' - The name of the certificate.
newCertificateSummary ::
  CertificateSummary
newCertificateSummary =
  CertificateSummary'
    { certificateArn = Core.Nothing,
      domainName = Core.Nothing,
      certificateDetail = Core.Nothing,
      tags = Core.Nothing,
      certificateName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the certificate.
certificateSummary_certificateArn :: Lens.Lens' CertificateSummary (Core.Maybe Core.Text)
certificateSummary_certificateArn = Lens.lens (\CertificateSummary' {certificateArn} -> certificateArn) (\s@CertificateSummary' {} a -> s {certificateArn = a} :: CertificateSummary)

-- | The domain name of the certificate.
certificateSummary_domainName :: Lens.Lens' CertificateSummary (Core.Maybe Core.Text)
certificateSummary_domainName = Lens.lens (\CertificateSummary' {domainName} -> domainName) (\s@CertificateSummary' {} a -> s {domainName = a} :: CertificateSummary)

-- | An object that describes a certificate in detail.
certificateSummary_certificateDetail :: Lens.Lens' CertificateSummary (Core.Maybe Certificate)
certificateSummary_certificateDetail = Lens.lens (\CertificateSummary' {certificateDetail} -> certificateDetail) (\s@CertificateSummary' {} a -> s {certificateDetail = a} :: CertificateSummary)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
certificateSummary_tags :: Lens.Lens' CertificateSummary (Core.Maybe [Tag])
certificateSummary_tags = Lens.lens (\CertificateSummary' {tags} -> tags) (\s@CertificateSummary' {} a -> s {tags = a} :: CertificateSummary) Core.. Lens.mapping Lens._Coerce

-- | The name of the certificate.
certificateSummary_certificateName :: Lens.Lens' CertificateSummary (Core.Maybe Core.Text)
certificateSummary_certificateName = Lens.lens (\CertificateSummary' {certificateName} -> certificateName) (\s@CertificateSummary' {} a -> s {certificateName = a} :: CertificateSummary)

instance Core.FromJSON CertificateSummary where
  parseJSON =
    Core.withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            Core.<$> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "domainName")
            Core.<*> (x Core..:? "certificateDetail")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "certificateName")
      )

instance Core.Hashable CertificateSummary

instance Core.NFData CertificateSummary
