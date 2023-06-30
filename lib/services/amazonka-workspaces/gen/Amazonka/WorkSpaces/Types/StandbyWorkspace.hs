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
-- Module      : Amazonka.WorkSpaces.Types.StandbyWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.StandbyWorkspace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.Tag

-- | Describes a Standby WorkSpace.
--
-- /See:/ 'newStandbyWorkspace' smart constructor.
data StandbyWorkspace = StandbyWorkspace'
  { -- | The tags associated with the Standby WorkSpace.
    tags :: Prelude.Maybe [Tag],
    -- | The volume encryption key of the Standby WorkSpace.
    volumeEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Standby WorkSpace.
    primaryWorkspaceId :: Prelude.Text,
    -- | The identifier of the directory for the Standby WorkSpace.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandbyWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'standbyWorkspace_tags' - The tags associated with the Standby WorkSpace.
--
-- 'volumeEncryptionKey', 'standbyWorkspace_volumeEncryptionKey' - The volume encryption key of the Standby WorkSpace.
--
-- 'primaryWorkspaceId', 'standbyWorkspace_primaryWorkspaceId' - The identifier of the Standby WorkSpace.
--
-- 'directoryId', 'standbyWorkspace_directoryId' - The identifier of the directory for the Standby WorkSpace.
newStandbyWorkspace ::
  -- | 'primaryWorkspaceId'
  Prelude.Text ->
  -- | 'directoryId'
  Prelude.Text ->
  StandbyWorkspace
newStandbyWorkspace
  pPrimaryWorkspaceId_
  pDirectoryId_ =
    StandbyWorkspace'
      { tags = Prelude.Nothing,
        volumeEncryptionKey = Prelude.Nothing,
        primaryWorkspaceId = pPrimaryWorkspaceId_,
        directoryId = pDirectoryId_
      }

-- | The tags associated with the Standby WorkSpace.
standbyWorkspace_tags :: Lens.Lens' StandbyWorkspace (Prelude.Maybe [Tag])
standbyWorkspace_tags = Lens.lens (\StandbyWorkspace' {tags} -> tags) (\s@StandbyWorkspace' {} a -> s {tags = a} :: StandbyWorkspace) Prelude.. Lens.mapping Lens.coerced

-- | The volume encryption key of the Standby WorkSpace.
standbyWorkspace_volumeEncryptionKey :: Lens.Lens' StandbyWorkspace (Prelude.Maybe Prelude.Text)
standbyWorkspace_volumeEncryptionKey = Lens.lens (\StandbyWorkspace' {volumeEncryptionKey} -> volumeEncryptionKey) (\s@StandbyWorkspace' {} a -> s {volumeEncryptionKey = a} :: StandbyWorkspace)

-- | The identifier of the Standby WorkSpace.
standbyWorkspace_primaryWorkspaceId :: Lens.Lens' StandbyWorkspace Prelude.Text
standbyWorkspace_primaryWorkspaceId = Lens.lens (\StandbyWorkspace' {primaryWorkspaceId} -> primaryWorkspaceId) (\s@StandbyWorkspace' {} a -> s {primaryWorkspaceId = a} :: StandbyWorkspace)

-- | The identifier of the directory for the Standby WorkSpace.
standbyWorkspace_directoryId :: Lens.Lens' StandbyWorkspace Prelude.Text
standbyWorkspace_directoryId = Lens.lens (\StandbyWorkspace' {directoryId} -> directoryId) (\s@StandbyWorkspace' {} a -> s {directoryId = a} :: StandbyWorkspace)

instance Data.FromJSON StandbyWorkspace where
  parseJSON =
    Data.withObject
      "StandbyWorkspace"
      ( \x ->
          StandbyWorkspace'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VolumeEncryptionKey")
            Prelude.<*> (x Data..: "PrimaryWorkspaceId")
            Prelude.<*> (x Data..: "DirectoryId")
      )

instance Prelude.Hashable StandbyWorkspace where
  hashWithSalt _salt StandbyWorkspace' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeEncryptionKey
      `Prelude.hashWithSalt` primaryWorkspaceId
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData StandbyWorkspace where
  rnf StandbyWorkspace' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeEncryptionKey
      `Prelude.seq` Prelude.rnf primaryWorkspaceId
      `Prelude.seq` Prelude.rnf directoryId

instance Data.ToJSON StandbyWorkspace where
  toJSON StandbyWorkspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("VolumeEncryptionKey" Data..=)
              Prelude.<$> volumeEncryptionKey,
            Prelude.Just
              ("PrimaryWorkspaceId" Data..= primaryWorkspaceId),
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )
