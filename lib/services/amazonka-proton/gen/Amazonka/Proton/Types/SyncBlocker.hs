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
-- Module      : Amazonka.Proton.Types.SyncBlocker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.SyncBlocker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.BlockerStatus
import Amazonka.Proton.Types.BlockerType
import Amazonka.Proton.Types.SyncBlockerContext

-- | Detailed data of the sync blocker.
--
-- /See:/ 'newSyncBlocker' smart constructor.
data SyncBlocker = SyncBlocker'
  { -- | The contexts for the sync blocker.
    contexts :: Prelude.Maybe [SyncBlockerContext],
    -- | The time the sync blocker was resolved.
    resolvedAt :: Prelude.Maybe Data.POSIX,
    -- | The reason the sync blocker was resolved.
    resolvedReason :: Prelude.Maybe Prelude.Text,
    -- | The time when the sync blocker was created.
    createdAt :: Data.POSIX,
    -- | The reason why the sync blocker was created.
    createdReason :: Prelude.Text,
    -- | The ID of the sync blocker.
    id :: Prelude.Text,
    -- | The status of the sync blocker.
    status :: BlockerStatus,
    -- | The type of the sync blocker.
    type' :: BlockerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncBlocker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contexts', 'syncBlocker_contexts' - The contexts for the sync blocker.
--
-- 'resolvedAt', 'syncBlocker_resolvedAt' - The time the sync blocker was resolved.
--
-- 'resolvedReason', 'syncBlocker_resolvedReason' - The reason the sync blocker was resolved.
--
-- 'createdAt', 'syncBlocker_createdAt' - The time when the sync blocker was created.
--
-- 'createdReason', 'syncBlocker_createdReason' - The reason why the sync blocker was created.
--
-- 'id', 'syncBlocker_id' - The ID of the sync blocker.
--
-- 'status', 'syncBlocker_status' - The status of the sync blocker.
--
-- 'type'', 'syncBlocker_type' - The type of the sync blocker.
newSyncBlocker ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'createdReason'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  BlockerStatus ->
  -- | 'type''
  BlockerType ->
  SyncBlocker
newSyncBlocker
  pCreatedAt_
  pCreatedReason_
  pId_
  pStatus_
  pType_ =
    SyncBlocker'
      { contexts = Prelude.Nothing,
        resolvedAt = Prelude.Nothing,
        resolvedReason = Prelude.Nothing,
        createdAt = Data._Time Lens.# pCreatedAt_,
        createdReason = pCreatedReason_,
        id = pId_,
        status = pStatus_,
        type' = pType_
      }

-- | The contexts for the sync blocker.
syncBlocker_contexts :: Lens.Lens' SyncBlocker (Prelude.Maybe [SyncBlockerContext])
syncBlocker_contexts = Lens.lens (\SyncBlocker' {contexts} -> contexts) (\s@SyncBlocker' {} a -> s {contexts = a} :: SyncBlocker) Prelude.. Lens.mapping Lens.coerced

-- | The time the sync blocker was resolved.
syncBlocker_resolvedAt :: Lens.Lens' SyncBlocker (Prelude.Maybe Prelude.UTCTime)
syncBlocker_resolvedAt = Lens.lens (\SyncBlocker' {resolvedAt} -> resolvedAt) (\s@SyncBlocker' {} a -> s {resolvedAt = a} :: SyncBlocker) Prelude.. Lens.mapping Data._Time

-- | The reason the sync blocker was resolved.
syncBlocker_resolvedReason :: Lens.Lens' SyncBlocker (Prelude.Maybe Prelude.Text)
syncBlocker_resolvedReason = Lens.lens (\SyncBlocker' {resolvedReason} -> resolvedReason) (\s@SyncBlocker' {} a -> s {resolvedReason = a} :: SyncBlocker)

-- | The time when the sync blocker was created.
syncBlocker_createdAt :: Lens.Lens' SyncBlocker Prelude.UTCTime
syncBlocker_createdAt = Lens.lens (\SyncBlocker' {createdAt} -> createdAt) (\s@SyncBlocker' {} a -> s {createdAt = a} :: SyncBlocker) Prelude.. Data._Time

-- | The reason why the sync blocker was created.
syncBlocker_createdReason :: Lens.Lens' SyncBlocker Prelude.Text
syncBlocker_createdReason = Lens.lens (\SyncBlocker' {createdReason} -> createdReason) (\s@SyncBlocker' {} a -> s {createdReason = a} :: SyncBlocker)

-- | The ID of the sync blocker.
syncBlocker_id :: Lens.Lens' SyncBlocker Prelude.Text
syncBlocker_id = Lens.lens (\SyncBlocker' {id} -> id) (\s@SyncBlocker' {} a -> s {id = a} :: SyncBlocker)

-- | The status of the sync blocker.
syncBlocker_status :: Lens.Lens' SyncBlocker BlockerStatus
syncBlocker_status = Lens.lens (\SyncBlocker' {status} -> status) (\s@SyncBlocker' {} a -> s {status = a} :: SyncBlocker)

-- | The type of the sync blocker.
syncBlocker_type :: Lens.Lens' SyncBlocker BlockerType
syncBlocker_type = Lens.lens (\SyncBlocker' {type'} -> type') (\s@SyncBlocker' {} a -> s {type' = a} :: SyncBlocker)

instance Data.FromJSON SyncBlocker where
  parseJSON =
    Data.withObject
      "SyncBlocker"
      ( \x ->
          SyncBlocker'
            Prelude.<$> (x Data..:? "contexts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resolvedAt")
            Prelude.<*> (x Data..:? "resolvedReason")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "createdReason")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable SyncBlocker where
  hashWithSalt _salt SyncBlocker' {..} =
    _salt
      `Prelude.hashWithSalt` contexts
      `Prelude.hashWithSalt` resolvedAt
      `Prelude.hashWithSalt` resolvedReason
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdReason
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SyncBlocker where
  rnf SyncBlocker' {..} =
    Prelude.rnf contexts
      `Prelude.seq` Prelude.rnf resolvedAt
      `Prelude.seq` Prelude.rnf resolvedReason
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdReason
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
