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
-- Module      : Amazonka.IoT.Types.CACertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CACertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CACertificateStatus
import qualified Amazonka.Prelude as Prelude

-- | A CA certificate.
--
-- /See:/ 'newCACertificate' smart constructor.
data CACertificate = CACertificate'
  { -- | The ARN of the CA certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the CA certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The date the CA certificate was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The status of the CA certificate.
    --
    -- The status value REGISTER_INACTIVE is deprecated and should not be used.
    status :: Prelude.Maybe CACertificateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'cACertificate_certificateArn' - The ARN of the CA certificate.
--
-- 'certificateId', 'cACertificate_certificateId' - The ID of the CA certificate.
--
-- 'creationDate', 'cACertificate_creationDate' - The date the CA certificate was created.
--
-- 'status', 'cACertificate_status' - The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
newCACertificate ::
  CACertificate
newCACertificate =
  CACertificate'
    { certificateArn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN of the CA certificate.
cACertificate_certificateArn :: Lens.Lens' CACertificate (Prelude.Maybe Prelude.Text)
cACertificate_certificateArn = Lens.lens (\CACertificate' {certificateArn} -> certificateArn) (\s@CACertificate' {} a -> s {certificateArn = a} :: CACertificate)

-- | The ID of the CA certificate.
cACertificate_certificateId :: Lens.Lens' CACertificate (Prelude.Maybe Prelude.Text)
cACertificate_certificateId = Lens.lens (\CACertificate' {certificateId} -> certificateId) (\s@CACertificate' {} a -> s {certificateId = a} :: CACertificate)

-- | The date the CA certificate was created.
cACertificate_creationDate :: Lens.Lens' CACertificate (Prelude.Maybe Prelude.UTCTime)
cACertificate_creationDate = Lens.lens (\CACertificate' {creationDate} -> creationDate) (\s@CACertificate' {} a -> s {creationDate = a} :: CACertificate) Prelude.. Lens.mapping Data._Time

-- | The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
cACertificate_status :: Lens.Lens' CACertificate (Prelude.Maybe CACertificateStatus)
cACertificate_status = Lens.lens (\CACertificate' {status} -> status) (\s@CACertificate' {} a -> s {status = a} :: CACertificate)

instance Data.FromJSON CACertificate where
  parseJSON =
    Data.withObject
      "CACertificate"
      ( \x ->
          CACertificate'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "certificateId")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable CACertificate where
  hashWithSalt _salt CACertificate' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status

instance Prelude.NFData CACertificate where
  rnf CACertificate' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
