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
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon Lightsail SSL\/TLS certificate.
--
-- /See:/ 'newCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { -- | An object that describes a certificate in detail.
    certificateDetail :: Prelude.Maybe Certificate,
    -- | The name of the certificate.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateDetail', 'certificateSummary_certificateDetail' - An object that describes a certificate in detail.
--
-- 'certificateName', 'certificateSummary_certificateName' - The name of the certificate.
--
-- 'certificateArn', 'certificateSummary_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'domainName', 'certificateSummary_domainName' - The domain name of the certificate.
--
-- 'tags', 'certificateSummary_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
newCertificateSummary ::
  CertificateSummary
newCertificateSummary =
  CertificateSummary'
    { certificateDetail =
        Prelude.Nothing,
      certificateName = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      domainName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | An object that describes a certificate in detail.
certificateSummary_certificateDetail :: Lens.Lens' CertificateSummary (Prelude.Maybe Certificate)
certificateSummary_certificateDetail = Lens.lens (\CertificateSummary' {certificateDetail} -> certificateDetail) (\s@CertificateSummary' {} a -> s {certificateDetail = a} :: CertificateSummary)

-- | The name of the certificate.
certificateSummary_certificateName :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_certificateName = Lens.lens (\CertificateSummary' {certificateName} -> certificateName) (\s@CertificateSummary' {} a -> s {certificateName = a} :: CertificateSummary)

-- | The Amazon Resource Name (ARN) of the certificate.
certificateSummary_certificateArn :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_certificateArn = Lens.lens (\CertificateSummary' {certificateArn} -> certificateArn) (\s@CertificateSummary' {} a -> s {certificateArn = a} :: CertificateSummary)

-- | The domain name of the certificate.
certificateSummary_domainName :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_domainName = Lens.lens (\CertificateSummary' {domainName} -> domainName) (\s@CertificateSummary' {} a -> s {domainName = a} :: CertificateSummary)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
certificateSummary_tags :: Lens.Lens' CertificateSummary (Prelude.Maybe [Tag])
certificateSummary_tags = Lens.lens (\CertificateSummary' {tags} -> tags) (\s@CertificateSummary' {} a -> s {tags = a} :: CertificateSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CertificateSummary where
  parseJSON =
    Core.withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            Prelude.<$> (x Core..:? "certificateDetail")
            Prelude.<*> (x Core..:? "certificateName")
            Prelude.<*> (x Core..:? "certificateArn")
            Prelude.<*> (x Core..:? "domainName")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CertificateSummary

instance Prelude.NFData CertificateSummary
