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
-- Module      : Network.AWS.IoT.Types.CACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificate where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.CACertificateStatus
import qualified Network.AWS.Lens as Lens

-- | A CA certificate.
--
-- /See:/ 'newCACertificate' smart constructor.
data CACertificate = CACertificate'
  { -- | The status of the CA certificate.
    --
    -- The status value REGISTER_INACTIVE is deprecated and should not be used.
    status :: Core.Maybe CACertificateStatus,
    -- | The ARN of the CA certificate.
    certificateArn :: Core.Maybe Core.Text,
    -- | The date the CA certificate was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The ID of the CA certificate.
    certificateId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'cACertificate_status' - The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- 'certificateArn', 'cACertificate_certificateArn' - The ARN of the CA certificate.
--
-- 'creationDate', 'cACertificate_creationDate' - The date the CA certificate was created.
--
-- 'certificateId', 'cACertificate_certificateId' - The ID of the CA certificate.
newCACertificate ::
  CACertificate
newCACertificate =
  CACertificate'
    { status = Core.Nothing,
      certificateArn = Core.Nothing,
      creationDate = Core.Nothing,
      certificateId = Core.Nothing
    }

-- | The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
cACertificate_status :: Lens.Lens' CACertificate (Core.Maybe CACertificateStatus)
cACertificate_status = Lens.lens (\CACertificate' {status} -> status) (\s@CACertificate' {} a -> s {status = a} :: CACertificate)

-- | The ARN of the CA certificate.
cACertificate_certificateArn :: Lens.Lens' CACertificate (Core.Maybe Core.Text)
cACertificate_certificateArn = Lens.lens (\CACertificate' {certificateArn} -> certificateArn) (\s@CACertificate' {} a -> s {certificateArn = a} :: CACertificate)

-- | The date the CA certificate was created.
cACertificate_creationDate :: Lens.Lens' CACertificate (Core.Maybe Core.UTCTime)
cACertificate_creationDate = Lens.lens (\CACertificate' {creationDate} -> creationDate) (\s@CACertificate' {} a -> s {creationDate = a} :: CACertificate) Core.. Lens.mapping Core._Time

-- | The ID of the CA certificate.
cACertificate_certificateId :: Lens.Lens' CACertificate (Core.Maybe Core.Text)
cACertificate_certificateId = Lens.lens (\CACertificate' {certificateId} -> certificateId) (\s@CACertificate' {} a -> s {certificateId = a} :: CACertificate)

instance Core.FromJSON CACertificate where
  parseJSON =
    Core.withObject
      "CACertificate"
      ( \x ->
          CACertificate'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "certificateId")
      )

instance Core.Hashable CACertificate

instance Core.NFData CACertificate
