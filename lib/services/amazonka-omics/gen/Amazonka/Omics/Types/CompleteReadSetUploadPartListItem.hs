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
-- Module      : Amazonka.Omics.Types.CompleteReadSetUploadPartListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.CompleteReadSetUploadPartListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetPartSource
import qualified Amazonka.Prelude as Prelude

-- | Part of the response to the CompleteReadSetUpload API, including
-- metadata.
--
-- /See:/ 'newCompleteReadSetUploadPartListItem' smart constructor.
data CompleteReadSetUploadPartListItem = CompleteReadSetUploadPartListItem'
  { -- | A number identifying the part in a read set upload.
    partNumber :: Prelude.Natural,
    -- | The source file of the part being uploaded.
    partSource :: ReadSetPartSource,
    -- | A unique identifier used to confirm that parts are being added to the
    -- correct upload.
    checksum :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteReadSetUploadPartListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partNumber', 'completeReadSetUploadPartListItem_partNumber' - A number identifying the part in a read set upload.
--
-- 'partSource', 'completeReadSetUploadPartListItem_partSource' - The source file of the part being uploaded.
--
-- 'checksum', 'completeReadSetUploadPartListItem_checksum' - A unique identifier used to confirm that parts are being added to the
-- correct upload.
newCompleteReadSetUploadPartListItem ::
  -- | 'partNumber'
  Prelude.Natural ->
  -- | 'partSource'
  ReadSetPartSource ->
  -- | 'checksum'
  Prelude.Text ->
  CompleteReadSetUploadPartListItem
newCompleteReadSetUploadPartListItem
  pPartNumber_
  pPartSource_
  pChecksum_ =
    CompleteReadSetUploadPartListItem'
      { partNumber =
          pPartNumber_,
        partSource = pPartSource_,
        checksum = pChecksum_
      }

-- | A number identifying the part in a read set upload.
completeReadSetUploadPartListItem_partNumber :: Lens.Lens' CompleteReadSetUploadPartListItem Prelude.Natural
completeReadSetUploadPartListItem_partNumber = Lens.lens (\CompleteReadSetUploadPartListItem' {partNumber} -> partNumber) (\s@CompleteReadSetUploadPartListItem' {} a -> s {partNumber = a} :: CompleteReadSetUploadPartListItem)

-- | The source file of the part being uploaded.
completeReadSetUploadPartListItem_partSource :: Lens.Lens' CompleteReadSetUploadPartListItem ReadSetPartSource
completeReadSetUploadPartListItem_partSource = Lens.lens (\CompleteReadSetUploadPartListItem' {partSource} -> partSource) (\s@CompleteReadSetUploadPartListItem' {} a -> s {partSource = a} :: CompleteReadSetUploadPartListItem)

-- | A unique identifier used to confirm that parts are being added to the
-- correct upload.
completeReadSetUploadPartListItem_checksum :: Lens.Lens' CompleteReadSetUploadPartListItem Prelude.Text
completeReadSetUploadPartListItem_checksum = Lens.lens (\CompleteReadSetUploadPartListItem' {checksum} -> checksum) (\s@CompleteReadSetUploadPartListItem' {} a -> s {checksum = a} :: CompleteReadSetUploadPartListItem)

instance
  Prelude.Hashable
    CompleteReadSetUploadPartListItem
  where
  hashWithSalt
    _salt
    CompleteReadSetUploadPartListItem' {..} =
      _salt
        `Prelude.hashWithSalt` partNumber
        `Prelude.hashWithSalt` partSource
        `Prelude.hashWithSalt` checksum

instance
  Prelude.NFData
    CompleteReadSetUploadPartListItem
  where
  rnf CompleteReadSetUploadPartListItem' {..} =
    Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf partSource
      `Prelude.seq` Prelude.rnf checksum

instance
  Data.ToJSON
    CompleteReadSetUploadPartListItem
  where
  toJSON CompleteReadSetUploadPartListItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("partNumber" Data..= partNumber),
            Prelude.Just ("partSource" Data..= partSource),
            Prelude.Just ("checksum" Data..= checksum)
          ]
      )
