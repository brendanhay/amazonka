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
-- Module      : Amazonka.IoT.Types.OutgoingCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.OutgoingCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A certificate that has been transferred but not yet accepted.
--
-- /See:/ 'newOutgoingCertificate' smart constructor.
data OutgoingCertificate = OutgoingCertificate'
  { -- | The date the transfer was initiated.
    transferDate :: Prelude.Maybe Core.POSIX,
    -- | The certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The certificate ID.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account to which the transfer was made.
    transferredTo :: Prelude.Maybe Prelude.Text,
    -- | The certificate creation date.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The transfer message.
    transferMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutgoingCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferDate', 'outgoingCertificate_transferDate' - The date the transfer was initiated.
--
-- 'certificateArn', 'outgoingCertificate_certificateArn' - The certificate ARN.
--
-- 'certificateId', 'outgoingCertificate_certificateId' - The certificate ID.
--
-- 'transferredTo', 'outgoingCertificate_transferredTo' - The Amazon Web Services account to which the transfer was made.
--
-- 'creationDate', 'outgoingCertificate_creationDate' - The certificate creation date.
--
-- 'transferMessage', 'outgoingCertificate_transferMessage' - The transfer message.
newOutgoingCertificate ::
  OutgoingCertificate
newOutgoingCertificate =
  OutgoingCertificate'
    { transferDate =
        Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      transferredTo = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      transferMessage = Prelude.Nothing
    }

-- | The date the transfer was initiated.
outgoingCertificate_transferDate :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.UTCTime)
outgoingCertificate_transferDate = Lens.lens (\OutgoingCertificate' {transferDate} -> transferDate) (\s@OutgoingCertificate' {} a -> s {transferDate = a} :: OutgoingCertificate) Prelude.. Lens.mapping Core._Time

-- | The certificate ARN.
outgoingCertificate_certificateArn :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_certificateArn = Lens.lens (\OutgoingCertificate' {certificateArn} -> certificateArn) (\s@OutgoingCertificate' {} a -> s {certificateArn = a} :: OutgoingCertificate)

-- | The certificate ID.
outgoingCertificate_certificateId :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_certificateId = Lens.lens (\OutgoingCertificate' {certificateId} -> certificateId) (\s@OutgoingCertificate' {} a -> s {certificateId = a} :: OutgoingCertificate)

-- | The Amazon Web Services account to which the transfer was made.
outgoingCertificate_transferredTo :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_transferredTo = Lens.lens (\OutgoingCertificate' {transferredTo} -> transferredTo) (\s@OutgoingCertificate' {} a -> s {transferredTo = a} :: OutgoingCertificate)

-- | The certificate creation date.
outgoingCertificate_creationDate :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.UTCTime)
outgoingCertificate_creationDate = Lens.lens (\OutgoingCertificate' {creationDate} -> creationDate) (\s@OutgoingCertificate' {} a -> s {creationDate = a} :: OutgoingCertificate) Prelude.. Lens.mapping Core._Time

-- | The transfer message.
outgoingCertificate_transferMessage :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_transferMessage = Lens.lens (\OutgoingCertificate' {transferMessage} -> transferMessage) (\s@OutgoingCertificate' {} a -> s {transferMessage = a} :: OutgoingCertificate)

instance Core.FromJSON OutgoingCertificate where
  parseJSON =
    Core.withObject
      "OutgoingCertificate"
      ( \x ->
          OutgoingCertificate'
            Prelude.<$> (x Core..:? "transferDate")
            Prelude.<*> (x Core..:? "certificateArn")
            Prelude.<*> (x Core..:? "certificateId")
            Prelude.<*> (x Core..:? "transferredTo")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "transferMessage")
      )

instance Prelude.Hashable OutgoingCertificate where
  hashWithSalt _salt OutgoingCertificate' {..} =
    _salt `Prelude.hashWithSalt` transferDate
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` transferredTo
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` transferMessage

instance Prelude.NFData OutgoingCertificate where
  rnf OutgoingCertificate' {..} =
    Prelude.rnf transferDate
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf transferredTo
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf transferMessage
