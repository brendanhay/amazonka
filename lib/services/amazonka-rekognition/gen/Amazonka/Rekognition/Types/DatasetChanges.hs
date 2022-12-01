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
-- Module      : Amazonka.Rekognition.Types.DatasetChanges
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetChanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes updates or additions to a dataset. A Single update or addition
-- is an entry (JSON Line) that provides information about a single image.
-- To update an existing entry, you match the @source-ref@ field of the
-- update entry with the @source-ref@ filed of the entry that you want to
-- update. If the @source-ref@ field doesn\'t match an existing entry, the
-- entry is added to dataset as a new entry.
--
-- /See:/ 'newDatasetChanges' smart constructor.
data DatasetChanges = DatasetChanges'
  { -- | A Base64-encoded binary data object containing one or JSON lines that
    -- either update the dataset or are additions to the dataset. You change a
    -- dataset by calling UpdateDatasetEntries. If you are using an AWS SDK to
    -- call @UpdateDatasetEntries@, you don\'t need to encode @Changes@ as the
    -- SDK encodes the data for you.
    --
    -- For example JSON lines, see Image-Level labels in manifest files and and
    -- Object localization in manifest files in the /Amazon Rekognition Custom
    -- Labels Developer Guide/.
    groundTruth :: Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groundTruth', 'datasetChanges_groundTruth' - A Base64-encoded binary data object containing one or JSON lines that
-- either update the dataset or are additions to the dataset. You change a
-- dataset by calling UpdateDatasetEntries. If you are using an AWS SDK to
-- call @UpdateDatasetEntries@, you don\'t need to encode @Changes@ as the
-- SDK encodes the data for you.
--
-- For example JSON lines, see Image-Level labels in manifest files and and
-- Object localization in manifest files in the /Amazon Rekognition Custom
-- Labels Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newDatasetChanges ::
  -- | 'groundTruth'
  Prelude.ByteString ->
  DatasetChanges
newDatasetChanges pGroundTruth_ =
  DatasetChanges'
    { groundTruth =
        Core._Base64 Lens.# pGroundTruth_
    }

-- | A Base64-encoded binary data object containing one or JSON lines that
-- either update the dataset or are additions to the dataset. You change a
-- dataset by calling UpdateDatasetEntries. If you are using an AWS SDK to
-- call @UpdateDatasetEntries@, you don\'t need to encode @Changes@ as the
-- SDK encodes the data for you.
--
-- For example JSON lines, see Image-Level labels in manifest files and and
-- Object localization in manifest files in the /Amazon Rekognition Custom
-- Labels Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
datasetChanges_groundTruth :: Lens.Lens' DatasetChanges Prelude.ByteString
datasetChanges_groundTruth = Lens.lens (\DatasetChanges' {groundTruth} -> groundTruth) (\s@DatasetChanges' {} a -> s {groundTruth = a} :: DatasetChanges) Prelude.. Core._Base64

instance Prelude.Hashable DatasetChanges where
  hashWithSalt _salt DatasetChanges' {..} =
    _salt `Prelude.hashWithSalt` groundTruth

instance Prelude.NFData DatasetChanges where
  rnf DatasetChanges' {..} = Prelude.rnf groundTruth

instance Core.ToJSON DatasetChanges where
  toJSON DatasetChanges' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GroundTruth" Core..= groundTruth)]
      )
