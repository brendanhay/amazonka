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
-- Module      : Network.AWS.RDS.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Certificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A CA certificate for an AWS account.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The unique key that identifies a certificate.
    certificateIdentifier :: Core.Maybe Core.Text,
    -- | The starting date from which the certificate is valid.
    validFrom :: Core.Maybe Core.ISO8601,
    -- | Whether there is an override for the default certificate identifier.
    customerOverride :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Core.Maybe Core.Text,
    -- | The type of the certificate.
    certificateType :: Core.Maybe Core.Text,
    -- | The thumbprint of the certificate.
    thumbprint :: Core.Maybe Core.Text,
    -- | If there is an override for the default certificate identifier, when the
    -- override expires.
    customerOverrideValidTill :: Core.Maybe Core.ISO8601,
    -- | The final date that the certificate continues to be valid.
    validTill :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateIdentifier', 'certificate_certificateIdentifier' - The unique key that identifies a certificate.
--
-- 'validFrom', 'certificate_validFrom' - The starting date from which the certificate is valid.
--
-- 'customerOverride', 'certificate_customerOverride' - Whether there is an override for the default certificate identifier.
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate.
--
-- 'certificateType', 'certificate_certificateType' - The type of the certificate.
--
-- 'thumbprint', 'certificate_thumbprint' - The thumbprint of the certificate.
--
-- 'customerOverrideValidTill', 'certificate_customerOverrideValidTill' - If there is an override for the default certificate identifier, when the
-- override expires.
--
-- 'validTill', 'certificate_validTill' - The final date that the certificate continues to be valid.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateIdentifier = Core.Nothing,
      validFrom = Core.Nothing,
      customerOverride = Core.Nothing,
      certificateArn = Core.Nothing,
      certificateType = Core.Nothing,
      thumbprint = Core.Nothing,
      customerOverrideValidTill = Core.Nothing,
      validTill = Core.Nothing
    }

-- | The unique key that identifies a certificate.
certificate_certificateIdentifier :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateIdentifier = Lens.lens (\Certificate' {certificateIdentifier} -> certificateIdentifier) (\s@Certificate' {} a -> s {certificateIdentifier = a} :: Certificate)

-- | The starting date from which the certificate is valid.
certificate_validFrom :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
certificate_validFrom = Lens.lens (\Certificate' {validFrom} -> validFrom) (\s@Certificate' {} a -> s {validFrom = a} :: Certificate) Core.. Lens.mapping Core._Time

-- | Whether there is an override for the default certificate identifier.
certificate_customerOverride :: Lens.Lens' Certificate (Core.Maybe Core.Bool)
certificate_customerOverride = Lens.lens (\Certificate' {customerOverride} -> customerOverride) (\s@Certificate' {} a -> s {customerOverride = a} :: Certificate)

-- | The Amazon Resource Name (ARN) for the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The type of the certificate.
certificate_certificateType :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateType = Lens.lens (\Certificate' {certificateType} -> certificateType) (\s@Certificate' {} a -> s {certificateType = a} :: Certificate)

-- | The thumbprint of the certificate.
certificate_thumbprint :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_thumbprint = Lens.lens (\Certificate' {thumbprint} -> thumbprint) (\s@Certificate' {} a -> s {thumbprint = a} :: Certificate)

-- | If there is an override for the default certificate identifier, when the
-- override expires.
certificate_customerOverrideValidTill :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
certificate_customerOverrideValidTill = Lens.lens (\Certificate' {customerOverrideValidTill} -> customerOverrideValidTill) (\s@Certificate' {} a -> s {customerOverrideValidTill = a} :: Certificate) Core.. Lens.mapping Core._Time

-- | The final date that the certificate continues to be valid.
certificate_validTill :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
certificate_validTill = Lens.lens (\Certificate' {validTill} -> validTill) (\s@Certificate' {} a -> s {validTill = a} :: Certificate) Core.. Lens.mapping Core._Time

instance Core.FromXML Certificate where
  parseXML x =
    Certificate'
      Core.<$> (x Core..@? "CertificateIdentifier")
      Core.<*> (x Core..@? "ValidFrom")
      Core.<*> (x Core..@? "CustomerOverride")
      Core.<*> (x Core..@? "CertificateArn")
      Core.<*> (x Core..@? "CertificateType")
      Core.<*> (x Core..@? "Thumbprint")
      Core.<*> (x Core..@? "CustomerOverrideValidTill")
      Core.<*> (x Core..@? "ValidTill")

instance Core.Hashable Certificate

instance Core.NFData Certificate
