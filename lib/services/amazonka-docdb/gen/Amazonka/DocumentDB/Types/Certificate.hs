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
-- Module      : Amazonka.DocumentDB.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A certificate authority (CA) certificate for an account.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The type of the certificate.
    --
    -- Example: @CA@
    certificateType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the certificate.
    --
    -- Example: @arn:aws:rds:us-east-1::cert:rds-ca-2019@
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The date-time after which the certificate is no longer valid.
    --
    -- Example: @2024-07-31T17:57:09Z@
    validTill :: Prelude.Maybe Core.ISO8601,
    -- | The unique key that identifies a certificate.
    --
    -- Example: @rds-ca-2019@
    certificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The thumbprint of the certificate.
    thumbprint :: Prelude.Maybe Prelude.Text,
    -- | The starting date-time from which the certificate is valid.
    --
    -- Example: @2019-07-31T17:57:09Z@
    validFrom :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateType', 'certificate_certificateType' - The type of the certificate.
--
-- Example: @CA@
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate.
--
-- Example: @arn:aws:rds:us-east-1::cert:rds-ca-2019@
--
-- 'validTill', 'certificate_validTill' - The date-time after which the certificate is no longer valid.
--
-- Example: @2024-07-31T17:57:09Z@
--
-- 'certificateIdentifier', 'certificate_certificateIdentifier' - The unique key that identifies a certificate.
--
-- Example: @rds-ca-2019@
--
-- 'thumbprint', 'certificate_thumbprint' - The thumbprint of the certificate.
--
-- 'validFrom', 'certificate_validFrom' - The starting date-time from which the certificate is valid.
--
-- Example: @2019-07-31T17:57:09Z@
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateType = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      validTill = Prelude.Nothing,
      certificateIdentifier = Prelude.Nothing,
      thumbprint = Prelude.Nothing,
      validFrom = Prelude.Nothing
    }

-- | The type of the certificate.
--
-- Example: @CA@
certificate_certificateType :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateType = Lens.lens (\Certificate' {certificateType} -> certificateType) (\s@Certificate' {} a -> s {certificateType = a} :: Certificate)

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- Example: @arn:aws:rds:us-east-1::cert:rds-ca-2019@
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The date-time after which the certificate is no longer valid.
--
-- Example: @2024-07-31T17:57:09Z@
certificate_validTill :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validTill = Lens.lens (\Certificate' {validTill} -> validTill) (\s@Certificate' {} a -> s {validTill = a} :: Certificate) Prelude.. Lens.mapping Core._Time

-- | The unique key that identifies a certificate.
--
-- Example: @rds-ca-2019@
certificate_certificateIdentifier :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateIdentifier = Lens.lens (\Certificate' {certificateIdentifier} -> certificateIdentifier) (\s@Certificate' {} a -> s {certificateIdentifier = a} :: Certificate)

-- | The thumbprint of the certificate.
certificate_thumbprint :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_thumbprint = Lens.lens (\Certificate' {thumbprint} -> thumbprint) (\s@Certificate' {} a -> s {thumbprint = a} :: Certificate)

-- | The starting date-time from which the certificate is valid.
--
-- Example: @2019-07-31T17:57:09Z@
certificate_validFrom :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validFrom = Lens.lens (\Certificate' {validFrom} -> validFrom) (\s@Certificate' {} a -> s {validFrom = a} :: Certificate) Prelude.. Lens.mapping Core._Time

instance Core.FromXML Certificate where
  parseXML x =
    Certificate'
      Prelude.<$> (x Core..@? "CertificateType")
      Prelude.<*> (x Core..@? "CertificateArn")
      Prelude.<*> (x Core..@? "ValidTill")
      Prelude.<*> (x Core..@? "CertificateIdentifier")
      Prelude.<*> (x Core..@? "Thumbprint")
      Prelude.<*> (x Core..@? "ValidFrom")

instance Prelude.Hashable Certificate

instance Prelude.NFData Certificate
