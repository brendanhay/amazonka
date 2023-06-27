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
-- Module      : Amazonka.Omics.Types.ReadSetUploadPartListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetUploadPartListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetPartSource
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a single part of a file that was added to a multipart
-- upload. A list of these parts is returned in the response to the
-- ListReadSetUploadParts API.
--
-- /See:/ 'newReadSetUploadPartListItem' smart constructor.
data ReadSetUploadPartListItem = ReadSetUploadPartListItem'
  { -- | The time stamp for when a direct upload was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The time stamp for the most recent update to an uploaded part.
    lastUpdatedTime :: Prelude.Maybe Data.ISO8601,
    -- | The number identifying the part in an upload.
    partNumber :: Prelude.Natural,
    -- | The size of the the part in an upload.
    partSize :: Prelude.Natural,
    -- | The origin of the part being direct uploaded.
    partSource :: ReadSetPartSource,
    -- | A unique identifier used to confirm that parts are being added to the
    -- correct upload.
    checksum :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadSetUploadPartListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'readSetUploadPartListItem_creationTime' - The time stamp for when a direct upload was created.
--
-- 'lastUpdatedTime', 'readSetUploadPartListItem_lastUpdatedTime' - The time stamp for the most recent update to an uploaded part.
--
-- 'partNumber', 'readSetUploadPartListItem_partNumber' - The number identifying the part in an upload.
--
-- 'partSize', 'readSetUploadPartListItem_partSize' - The size of the the part in an upload.
--
-- 'partSource', 'readSetUploadPartListItem_partSource' - The origin of the part being direct uploaded.
--
-- 'checksum', 'readSetUploadPartListItem_checksum' - A unique identifier used to confirm that parts are being added to the
-- correct upload.
newReadSetUploadPartListItem ::
  -- | 'partNumber'
  Prelude.Natural ->
  -- | 'partSize'
  Prelude.Natural ->
  -- | 'partSource'
  ReadSetPartSource ->
  -- | 'checksum'
  Prelude.Text ->
  ReadSetUploadPartListItem
newReadSetUploadPartListItem
  pPartNumber_
  pPartSize_
  pPartSource_
  pChecksum_ =
    ReadSetUploadPartListItem'
      { creationTime =
          Prelude.Nothing,
        lastUpdatedTime = Prelude.Nothing,
        partNumber = pPartNumber_,
        partSize = pPartSize_,
        partSource = pPartSource_,
        checksum = pChecksum_
      }

-- | The time stamp for when a direct upload was created.
readSetUploadPartListItem_creationTime :: Lens.Lens' ReadSetUploadPartListItem (Prelude.Maybe Prelude.UTCTime)
readSetUploadPartListItem_creationTime = Lens.lens (\ReadSetUploadPartListItem' {creationTime} -> creationTime) (\s@ReadSetUploadPartListItem' {} a -> s {creationTime = a} :: ReadSetUploadPartListItem) Prelude.. Lens.mapping Data._Time

-- | The time stamp for the most recent update to an uploaded part.
readSetUploadPartListItem_lastUpdatedTime :: Lens.Lens' ReadSetUploadPartListItem (Prelude.Maybe Prelude.UTCTime)
readSetUploadPartListItem_lastUpdatedTime = Lens.lens (\ReadSetUploadPartListItem' {lastUpdatedTime} -> lastUpdatedTime) (\s@ReadSetUploadPartListItem' {} a -> s {lastUpdatedTime = a} :: ReadSetUploadPartListItem) Prelude.. Lens.mapping Data._Time

-- | The number identifying the part in an upload.
readSetUploadPartListItem_partNumber :: Lens.Lens' ReadSetUploadPartListItem Prelude.Natural
readSetUploadPartListItem_partNumber = Lens.lens (\ReadSetUploadPartListItem' {partNumber} -> partNumber) (\s@ReadSetUploadPartListItem' {} a -> s {partNumber = a} :: ReadSetUploadPartListItem)

-- | The size of the the part in an upload.
readSetUploadPartListItem_partSize :: Lens.Lens' ReadSetUploadPartListItem Prelude.Natural
readSetUploadPartListItem_partSize = Lens.lens (\ReadSetUploadPartListItem' {partSize} -> partSize) (\s@ReadSetUploadPartListItem' {} a -> s {partSize = a} :: ReadSetUploadPartListItem)

-- | The origin of the part being direct uploaded.
readSetUploadPartListItem_partSource :: Lens.Lens' ReadSetUploadPartListItem ReadSetPartSource
readSetUploadPartListItem_partSource = Lens.lens (\ReadSetUploadPartListItem' {partSource} -> partSource) (\s@ReadSetUploadPartListItem' {} a -> s {partSource = a} :: ReadSetUploadPartListItem)

-- | A unique identifier used to confirm that parts are being added to the
-- correct upload.
readSetUploadPartListItem_checksum :: Lens.Lens' ReadSetUploadPartListItem Prelude.Text
readSetUploadPartListItem_checksum = Lens.lens (\ReadSetUploadPartListItem' {checksum} -> checksum) (\s@ReadSetUploadPartListItem' {} a -> s {checksum = a} :: ReadSetUploadPartListItem)

instance Data.FromJSON ReadSetUploadPartListItem where
  parseJSON =
    Data.withObject
      "ReadSetUploadPartListItem"
      ( \x ->
          ReadSetUploadPartListItem'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..: "partNumber")
            Prelude.<*> (x Data..: "partSize")
            Prelude.<*> (x Data..: "partSource")
            Prelude.<*> (x Data..: "checksum")
      )

instance Prelude.Hashable ReadSetUploadPartListItem where
  hashWithSalt _salt ReadSetUploadPartListItem' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` partSize
      `Prelude.hashWithSalt` partSource
      `Prelude.hashWithSalt` checksum

instance Prelude.NFData ReadSetUploadPartListItem where
  rnf ReadSetUploadPartListItem' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf partSize
      `Prelude.seq` Prelude.rnf partSource
      `Prelude.seq` Prelude.rnf checksum
