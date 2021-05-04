{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.OutgoingCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OutgoingCertificate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A certificate that has been transferred but not yet accepted.
--
-- /See:/ 'newOutgoingCertificate' smart constructor.
data OutgoingCertificate = OutgoingCertificate'
  { -- | The date the transfer was initiated.
    transferDate :: Prelude.Maybe Prelude.POSIX,
    -- | The certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The transfer message.
    transferMessage :: Prelude.Maybe Prelude.Text,
    -- | The certificate creation date.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS account to which the transfer was made.
    transferredTo :: Prelude.Maybe Prelude.Text,
    -- | The certificate ID.
    certificateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'transferMessage', 'outgoingCertificate_transferMessage' - The transfer message.
--
-- 'creationDate', 'outgoingCertificate_creationDate' - The certificate creation date.
--
-- 'transferredTo', 'outgoingCertificate_transferredTo' - The AWS account to which the transfer was made.
--
-- 'certificateId', 'outgoingCertificate_certificateId' - The certificate ID.
newOutgoingCertificate ::
  OutgoingCertificate
newOutgoingCertificate =
  OutgoingCertificate'
    { transferDate =
        Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      transferMessage = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      transferredTo = Prelude.Nothing,
      certificateId = Prelude.Nothing
    }

-- | The date the transfer was initiated.
outgoingCertificate_transferDate :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.UTCTime)
outgoingCertificate_transferDate = Lens.lens (\OutgoingCertificate' {transferDate} -> transferDate) (\s@OutgoingCertificate' {} a -> s {transferDate = a} :: OutgoingCertificate) Prelude.. Lens.mapping Prelude._Time

-- | The certificate ARN.
outgoingCertificate_certificateArn :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_certificateArn = Lens.lens (\OutgoingCertificate' {certificateArn} -> certificateArn) (\s@OutgoingCertificate' {} a -> s {certificateArn = a} :: OutgoingCertificate)

-- | The transfer message.
outgoingCertificate_transferMessage :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_transferMessage = Lens.lens (\OutgoingCertificate' {transferMessage} -> transferMessage) (\s@OutgoingCertificate' {} a -> s {transferMessage = a} :: OutgoingCertificate)

-- | The certificate creation date.
outgoingCertificate_creationDate :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.UTCTime)
outgoingCertificate_creationDate = Lens.lens (\OutgoingCertificate' {creationDate} -> creationDate) (\s@OutgoingCertificate' {} a -> s {creationDate = a} :: OutgoingCertificate) Prelude.. Lens.mapping Prelude._Time

-- | The AWS account to which the transfer was made.
outgoingCertificate_transferredTo :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_transferredTo = Lens.lens (\OutgoingCertificate' {transferredTo} -> transferredTo) (\s@OutgoingCertificate' {} a -> s {transferredTo = a} :: OutgoingCertificate)

-- | The certificate ID.
outgoingCertificate_certificateId :: Lens.Lens' OutgoingCertificate (Prelude.Maybe Prelude.Text)
outgoingCertificate_certificateId = Lens.lens (\OutgoingCertificate' {certificateId} -> certificateId) (\s@OutgoingCertificate' {} a -> s {certificateId = a} :: OutgoingCertificate)

instance Prelude.FromJSON OutgoingCertificate where
  parseJSON =
    Prelude.withObject
      "OutgoingCertificate"
      ( \x ->
          OutgoingCertificate'
            Prelude.<$> (x Prelude..:? "transferDate")
            Prelude.<*> (x Prelude..:? "certificateArn")
            Prelude.<*> (x Prelude..:? "transferMessage")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> (x Prelude..:? "transferredTo")
            Prelude.<*> (x Prelude..:? "certificateId")
      )

instance Prelude.Hashable OutgoingCertificate

instance Prelude.NFData OutgoingCertificate
