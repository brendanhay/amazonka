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
-- Module      : Network.AWS.IoT.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Certificate where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import qualified Network.AWS.Lens as Lens

-- | Information about a certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The status of the certificate.
    --
    -- The status value REGISTER_INACTIVE is deprecated and should not be used.
    status :: Core.Maybe CertificateStatus,
    -- | The mode of the certificate.
    certificateMode :: Core.Maybe CertificateMode,
    -- | The ARN of the certificate.
    certificateArn :: Core.Maybe Core.Text,
    -- | The date and time the certificate was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Core.Maybe Core.Text
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
-- 'status', 'certificate_status' - The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- 'certificateMode', 'certificate_certificateMode' - The mode of the certificate.
--
-- 'certificateArn', 'certificate_certificateArn' - The ARN of the certificate.
--
-- 'creationDate', 'certificate_creationDate' - The date and time the certificate was created.
--
-- 'certificateId', 'certificate_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { status = Core.Nothing,
      certificateMode = Core.Nothing,
      certificateArn = Core.Nothing,
      creationDate = Core.Nothing,
      certificateId = Core.Nothing
    }

-- | The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
certificate_status :: Lens.Lens' Certificate (Core.Maybe CertificateStatus)
certificate_status = Lens.lens (\Certificate' {status} -> status) (\s@Certificate' {} a -> s {status = a} :: Certificate)

-- | The mode of the certificate.
certificate_certificateMode :: Lens.Lens' Certificate (Core.Maybe CertificateMode)
certificate_certificateMode = Lens.lens (\Certificate' {certificateMode} -> certificateMode) (\s@Certificate' {} a -> s {certificateMode = a} :: Certificate)

-- | The ARN of the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The date and time the certificate was created.
certificate_creationDate :: Lens.Lens' Certificate (Core.Maybe Core.UTCTime)
certificate_creationDate = Lens.lens (\Certificate' {creationDate} -> creationDate) (\s@Certificate' {} a -> s {creationDate = a} :: Certificate) Core.. Lens.mapping Core._Time

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
certificate_certificateId :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateId = Lens.lens (\Certificate' {certificateId} -> certificateId) (\s@Certificate' {} a -> s {certificateId = a} :: Certificate)

instance Core.FromJSON Certificate where
  parseJSON =
    Core.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "certificateMode")
            Core.<*> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "certificateId")
      )

instance Core.Hashable Certificate

instance Core.NFData Certificate
