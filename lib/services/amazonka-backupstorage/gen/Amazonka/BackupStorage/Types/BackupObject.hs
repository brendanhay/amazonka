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
-- Module      : Amazonka.BackupStorage.Types.BackupObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupStorage.Types.BackupObject where

import Amazonka.BackupStorage.Types.SummaryChecksumAlgorithm
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object
--
-- /See:/ 'newBackupObject' smart constructor.
data BackupObject = BackupObject'
  { -- | Number of chunks in object
    chunksCount :: Prelude.Maybe Prelude.Integer,
    -- | Metadata string associated with the Object
    metadataString :: Prelude.Maybe Prelude.Text,
    -- | Object name
    name :: Prelude.Text,
    -- | Object checksum
    objectChecksum :: Prelude.Text,
    -- | Checksum algorithm
    objectChecksumAlgorithm :: SummaryChecksumAlgorithm,
    -- | Object token
    objectToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chunksCount', 'backupObject_chunksCount' - Number of chunks in object
--
-- 'metadataString', 'backupObject_metadataString' - Metadata string associated with the Object
--
-- 'name', 'backupObject_name' - Object name
--
-- 'objectChecksum', 'backupObject_objectChecksum' - Object checksum
--
-- 'objectChecksumAlgorithm', 'backupObject_objectChecksumAlgorithm' - Checksum algorithm
--
-- 'objectToken', 'backupObject_objectToken' - Object token
newBackupObject ::
  -- | 'name'
  Prelude.Text ->
  -- | 'objectChecksum'
  Prelude.Text ->
  -- | 'objectChecksumAlgorithm'
  SummaryChecksumAlgorithm ->
  -- | 'objectToken'
  Prelude.Text ->
  BackupObject
newBackupObject
  pName_
  pObjectChecksum_
  pObjectChecksumAlgorithm_
  pObjectToken_ =
    BackupObject'
      { chunksCount = Prelude.Nothing,
        metadataString = Prelude.Nothing,
        name = pName_,
        objectChecksum = pObjectChecksum_,
        objectChecksumAlgorithm = pObjectChecksumAlgorithm_,
        objectToken = pObjectToken_
      }

-- | Number of chunks in object
backupObject_chunksCount :: Lens.Lens' BackupObject (Prelude.Maybe Prelude.Integer)
backupObject_chunksCount = Lens.lens (\BackupObject' {chunksCount} -> chunksCount) (\s@BackupObject' {} a -> s {chunksCount = a} :: BackupObject)

-- | Metadata string associated with the Object
backupObject_metadataString :: Lens.Lens' BackupObject (Prelude.Maybe Prelude.Text)
backupObject_metadataString = Lens.lens (\BackupObject' {metadataString} -> metadataString) (\s@BackupObject' {} a -> s {metadataString = a} :: BackupObject)

-- | Object name
backupObject_name :: Lens.Lens' BackupObject Prelude.Text
backupObject_name = Lens.lens (\BackupObject' {name} -> name) (\s@BackupObject' {} a -> s {name = a} :: BackupObject)

-- | Object checksum
backupObject_objectChecksum :: Lens.Lens' BackupObject Prelude.Text
backupObject_objectChecksum = Lens.lens (\BackupObject' {objectChecksum} -> objectChecksum) (\s@BackupObject' {} a -> s {objectChecksum = a} :: BackupObject)

-- | Checksum algorithm
backupObject_objectChecksumAlgorithm :: Lens.Lens' BackupObject SummaryChecksumAlgorithm
backupObject_objectChecksumAlgorithm = Lens.lens (\BackupObject' {objectChecksumAlgorithm} -> objectChecksumAlgorithm) (\s@BackupObject' {} a -> s {objectChecksumAlgorithm = a} :: BackupObject)

-- | Object token
backupObject_objectToken :: Lens.Lens' BackupObject Prelude.Text
backupObject_objectToken = Lens.lens (\BackupObject' {objectToken} -> objectToken) (\s@BackupObject' {} a -> s {objectToken = a} :: BackupObject)

instance Data.FromJSON BackupObject where
  parseJSON =
    Data.withObject
      "BackupObject"
      ( \x ->
          BackupObject'
            Prelude.<$> (x Data..:? "ChunksCount")
            Prelude.<*> (x Data..:? "MetadataString")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ObjectChecksum")
            Prelude.<*> (x Data..: "ObjectChecksumAlgorithm")
            Prelude.<*> (x Data..: "ObjectToken")
      )

instance Prelude.Hashable BackupObject where
  hashWithSalt _salt BackupObject' {..} =
    _salt
      `Prelude.hashWithSalt` chunksCount
      `Prelude.hashWithSalt` metadataString
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` objectChecksum
      `Prelude.hashWithSalt` objectChecksumAlgorithm
      `Prelude.hashWithSalt` objectToken

instance Prelude.NFData BackupObject where
  rnf BackupObject' {..} =
    Prelude.rnf chunksCount
      `Prelude.seq` Prelude.rnf metadataString
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf objectChecksum
      `Prelude.seq` Prelude.rnf objectChecksumAlgorithm
      `Prelude.seq` Prelude.rnf objectToken
