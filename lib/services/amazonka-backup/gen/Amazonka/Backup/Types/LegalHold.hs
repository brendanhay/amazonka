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
-- Module      : Amazonka.Backup.Types.LegalHold
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.LegalHold where

import Amazonka.Backup.Types.LegalHoldStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A legal hold is an administrative tool that helps prevent backups from
-- being deleted while under a hold. While the hold is in place, backups
-- under a hold cannot be deleted and lifecycle policies that would alter
-- the backup status (such as transition to cold storage) are delayed until
-- the legal hold is removed. A backup can have more than one legal hold.
-- Legal holds are applied to one or more backups (also known as recovery
-- points). These backups can be filtered by resource types and by resource
-- IDs.
--
-- /See:/ 'newLegalHold' smart constructor.
data LegalHold = LegalHold'
  { -- | This is the time in number format when legal hold was cancelled.
    cancellationDate :: Prelude.Maybe Data.POSIX,
    -- | This is the time in number format when legal hold was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | This is the description of a legal hold.
    description :: Prelude.Maybe Prelude.Text,
    -- | This is an Amazon Resource Number (ARN) that uniquely identifies the
    -- legal hold; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    legalHoldArn :: Prelude.Maybe Prelude.Text,
    -- | ID of specific legal hold on one or more recovery points.
    legalHoldId :: Prelude.Maybe Prelude.Text,
    -- | This is the status of the legal hold. Statuses can be @ACTIVE@,
    -- @CREATING@, @CANCELED@, and @CANCELING@.
    status :: Prelude.Maybe LegalHoldStatus,
    -- | This is the title of a legal hold.
    title :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancellationDate', 'legalHold_cancellationDate' - This is the time in number format when legal hold was cancelled.
--
-- 'creationDate', 'legalHold_creationDate' - This is the time in number format when legal hold was created.
--
-- 'description', 'legalHold_description' - This is the description of a legal hold.
--
-- 'legalHoldArn', 'legalHold_legalHoldArn' - This is an Amazon Resource Number (ARN) that uniquely identifies the
-- legal hold; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'legalHoldId', 'legalHold_legalHoldId' - ID of specific legal hold on one or more recovery points.
--
-- 'status', 'legalHold_status' - This is the status of the legal hold. Statuses can be @ACTIVE@,
-- @CREATING@, @CANCELED@, and @CANCELING@.
--
-- 'title', 'legalHold_title' - This is the title of a legal hold.
newLegalHold ::
  LegalHold
newLegalHold =
  LegalHold'
    { cancellationDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      legalHoldArn = Prelude.Nothing,
      legalHoldId = Prelude.Nothing,
      status = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | This is the time in number format when legal hold was cancelled.
legalHold_cancellationDate :: Lens.Lens' LegalHold (Prelude.Maybe Prelude.UTCTime)
legalHold_cancellationDate = Lens.lens (\LegalHold' {cancellationDate} -> cancellationDate) (\s@LegalHold' {} a -> s {cancellationDate = a} :: LegalHold) Prelude.. Lens.mapping Data._Time

-- | This is the time in number format when legal hold was created.
legalHold_creationDate :: Lens.Lens' LegalHold (Prelude.Maybe Prelude.UTCTime)
legalHold_creationDate = Lens.lens (\LegalHold' {creationDate} -> creationDate) (\s@LegalHold' {} a -> s {creationDate = a} :: LegalHold) Prelude.. Lens.mapping Data._Time

-- | This is the description of a legal hold.
legalHold_description :: Lens.Lens' LegalHold (Prelude.Maybe Prelude.Text)
legalHold_description = Lens.lens (\LegalHold' {description} -> description) (\s@LegalHold' {} a -> s {description = a} :: LegalHold)

-- | This is an Amazon Resource Number (ARN) that uniquely identifies the
-- legal hold; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
legalHold_legalHoldArn :: Lens.Lens' LegalHold (Prelude.Maybe Prelude.Text)
legalHold_legalHoldArn = Lens.lens (\LegalHold' {legalHoldArn} -> legalHoldArn) (\s@LegalHold' {} a -> s {legalHoldArn = a} :: LegalHold)

-- | ID of specific legal hold on one or more recovery points.
legalHold_legalHoldId :: Lens.Lens' LegalHold (Prelude.Maybe Prelude.Text)
legalHold_legalHoldId = Lens.lens (\LegalHold' {legalHoldId} -> legalHoldId) (\s@LegalHold' {} a -> s {legalHoldId = a} :: LegalHold)

-- | This is the status of the legal hold. Statuses can be @ACTIVE@,
-- @CREATING@, @CANCELED@, and @CANCELING@.
legalHold_status :: Lens.Lens' LegalHold (Prelude.Maybe LegalHoldStatus)
legalHold_status = Lens.lens (\LegalHold' {status} -> status) (\s@LegalHold' {} a -> s {status = a} :: LegalHold)

-- | This is the title of a legal hold.
legalHold_title :: Lens.Lens' LegalHold (Prelude.Maybe Prelude.Text)
legalHold_title = Lens.lens (\LegalHold' {title} -> title) (\s@LegalHold' {} a -> s {title = a} :: LegalHold)

instance Data.FromJSON LegalHold where
  parseJSON =
    Data.withObject
      "LegalHold"
      ( \x ->
          LegalHold'
            Prelude.<$> (x Data..:? "CancellationDate")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LegalHoldArn")
            Prelude.<*> (x Data..:? "LegalHoldId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Title")
      )

instance Prelude.Hashable LegalHold where
  hashWithSalt _salt LegalHold' {..} =
    _salt `Prelude.hashWithSalt` cancellationDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` legalHoldArn
      `Prelude.hashWithSalt` legalHoldId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title

instance Prelude.NFData LegalHold where
  rnf LegalHold' {..} =
    Prelude.rnf cancellationDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf legalHoldArn
      `Prelude.seq` Prelude.rnf legalHoldId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf title
