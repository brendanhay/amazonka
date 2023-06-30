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
-- Module      : Amazonka.Omics.Types.ReadSetListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.FileType
import Amazonka.Omics.Types.ReadSetStatus
import Amazonka.Omics.Types.SequenceInformation
import qualified Amazonka.Prelude as Prelude

-- | A read set.
--
-- /See:/ 'newReadSetListItem' smart constructor.
data ReadSetListItem = ReadSetListItem'
  { -- | The read set\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s genome reference ARN.
    referenceArn :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sample ID.
    sampleId :: Prelude.Maybe Prelude.Text,
    sequenceInformation :: Prelude.Maybe SequenceInformation,
    -- | The read set\'s subject ID.
    subjectId :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s ARN.
    arn :: Prelude.Text,
    -- | When the read set was created.
    creationTime :: Data.ISO8601,
    -- | The read set\'s file type.
    fileType :: FileType,
    -- | The read set\'s ID.
    id :: Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The read set\'s status.
    status :: ReadSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadSetListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'readSetListItem_description' - The read set\'s description.
--
-- 'name', 'readSetListItem_name' - The read set\'s name.
--
-- 'referenceArn', 'readSetListItem_referenceArn' - The read set\'s genome reference ARN.
--
-- 'sampleId', 'readSetListItem_sampleId' - The read set\'s sample ID.
--
-- 'sequenceInformation', 'readSetListItem_sequenceInformation' - Undocumented member.
--
-- 'subjectId', 'readSetListItem_subjectId' - The read set\'s subject ID.
--
-- 'arn', 'readSetListItem_arn' - The read set\'s ARN.
--
-- 'creationTime', 'readSetListItem_creationTime' - When the read set was created.
--
-- 'fileType', 'readSetListItem_fileType' - The read set\'s file type.
--
-- 'id', 'readSetListItem_id' - The read set\'s ID.
--
-- 'sequenceStoreId', 'readSetListItem_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'status', 'readSetListItem_status' - The read set\'s status.
newReadSetListItem ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'fileType'
  FileType ->
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetStatus ->
  ReadSetListItem
newReadSetListItem
  pArn_
  pCreationTime_
  pFileType_
  pId_
  pSequenceStoreId_
  pStatus_ =
    ReadSetListItem'
      { description = Prelude.Nothing,
        name = Prelude.Nothing,
        referenceArn = Prelude.Nothing,
        sampleId = Prelude.Nothing,
        sequenceInformation = Prelude.Nothing,
        subjectId = Prelude.Nothing,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        fileType = pFileType_,
        id = pId_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_
      }

-- | The read set\'s description.
readSetListItem_description :: Lens.Lens' ReadSetListItem (Prelude.Maybe Prelude.Text)
readSetListItem_description = Lens.lens (\ReadSetListItem' {description} -> description) (\s@ReadSetListItem' {} a -> s {description = a} :: ReadSetListItem)

-- | The read set\'s name.
readSetListItem_name :: Lens.Lens' ReadSetListItem (Prelude.Maybe Prelude.Text)
readSetListItem_name = Lens.lens (\ReadSetListItem' {name} -> name) (\s@ReadSetListItem' {} a -> s {name = a} :: ReadSetListItem)

-- | The read set\'s genome reference ARN.
readSetListItem_referenceArn :: Lens.Lens' ReadSetListItem (Prelude.Maybe Prelude.Text)
readSetListItem_referenceArn = Lens.lens (\ReadSetListItem' {referenceArn} -> referenceArn) (\s@ReadSetListItem' {} a -> s {referenceArn = a} :: ReadSetListItem)

-- | The read set\'s sample ID.
readSetListItem_sampleId :: Lens.Lens' ReadSetListItem (Prelude.Maybe Prelude.Text)
readSetListItem_sampleId = Lens.lens (\ReadSetListItem' {sampleId} -> sampleId) (\s@ReadSetListItem' {} a -> s {sampleId = a} :: ReadSetListItem)

-- | Undocumented member.
readSetListItem_sequenceInformation :: Lens.Lens' ReadSetListItem (Prelude.Maybe SequenceInformation)
readSetListItem_sequenceInformation = Lens.lens (\ReadSetListItem' {sequenceInformation} -> sequenceInformation) (\s@ReadSetListItem' {} a -> s {sequenceInformation = a} :: ReadSetListItem)

-- | The read set\'s subject ID.
readSetListItem_subjectId :: Lens.Lens' ReadSetListItem (Prelude.Maybe Prelude.Text)
readSetListItem_subjectId = Lens.lens (\ReadSetListItem' {subjectId} -> subjectId) (\s@ReadSetListItem' {} a -> s {subjectId = a} :: ReadSetListItem)

-- | The read set\'s ARN.
readSetListItem_arn :: Lens.Lens' ReadSetListItem Prelude.Text
readSetListItem_arn = Lens.lens (\ReadSetListItem' {arn} -> arn) (\s@ReadSetListItem' {} a -> s {arn = a} :: ReadSetListItem)

-- | When the read set was created.
readSetListItem_creationTime :: Lens.Lens' ReadSetListItem Prelude.UTCTime
readSetListItem_creationTime = Lens.lens (\ReadSetListItem' {creationTime} -> creationTime) (\s@ReadSetListItem' {} a -> s {creationTime = a} :: ReadSetListItem) Prelude.. Data._Time

-- | The read set\'s file type.
readSetListItem_fileType :: Lens.Lens' ReadSetListItem FileType
readSetListItem_fileType = Lens.lens (\ReadSetListItem' {fileType} -> fileType) (\s@ReadSetListItem' {} a -> s {fileType = a} :: ReadSetListItem)

-- | The read set\'s ID.
readSetListItem_id :: Lens.Lens' ReadSetListItem Prelude.Text
readSetListItem_id = Lens.lens (\ReadSetListItem' {id} -> id) (\s@ReadSetListItem' {} a -> s {id = a} :: ReadSetListItem)

-- | The read set\'s sequence store ID.
readSetListItem_sequenceStoreId :: Lens.Lens' ReadSetListItem Prelude.Text
readSetListItem_sequenceStoreId = Lens.lens (\ReadSetListItem' {sequenceStoreId} -> sequenceStoreId) (\s@ReadSetListItem' {} a -> s {sequenceStoreId = a} :: ReadSetListItem)

-- | The read set\'s status.
readSetListItem_status :: Lens.Lens' ReadSetListItem ReadSetStatus
readSetListItem_status = Lens.lens (\ReadSetListItem' {status} -> status) (\s@ReadSetListItem' {} a -> s {status = a} :: ReadSetListItem)

instance Data.FromJSON ReadSetListItem where
  parseJSON =
    Data.withObject
      "ReadSetListItem"
      ( \x ->
          ReadSetListItem'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "referenceArn")
            Prelude.<*> (x Data..:? "sampleId")
            Prelude.<*> (x Data..:? "sequenceInformation")
            Prelude.<*> (x Data..:? "subjectId")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "fileType")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "sequenceStoreId")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ReadSetListItem where
  hashWithSalt _salt ReadSetListItem' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` referenceArn
      `Prelude.hashWithSalt` sampleId
      `Prelude.hashWithSalt` sequenceInformation
      `Prelude.hashWithSalt` subjectId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` fileType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReadSetListItem where
  rnf ReadSetListItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf sequenceInformation
      `Prelude.seq` Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf fileType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf status
