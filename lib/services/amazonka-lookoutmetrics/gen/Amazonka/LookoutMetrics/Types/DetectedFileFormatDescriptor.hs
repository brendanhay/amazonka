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
-- Module      : Amazonka.LookoutMetrics.Types.DetectedFileFormatDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedFileFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.DetectedCsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DetectedJsonFormatDescriptor
import qualified Amazonka.Prelude as Prelude

-- | Properties of an inferred data format.
--
-- /See:/ 'newDetectedFileFormatDescriptor' smart constructor.
data DetectedFileFormatDescriptor = DetectedFileFormatDescriptor'
  { -- | Details about a JSON format.
    jsonFormatDescriptor :: Prelude.Maybe DetectedJsonFormatDescriptor,
    -- | Details about a CSV format.
    csvFormatDescriptor :: Prelude.Maybe DetectedCsvFormatDescriptor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedFileFormatDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonFormatDescriptor', 'detectedFileFormatDescriptor_jsonFormatDescriptor' - Details about a JSON format.
--
-- 'csvFormatDescriptor', 'detectedFileFormatDescriptor_csvFormatDescriptor' - Details about a CSV format.
newDetectedFileFormatDescriptor ::
  DetectedFileFormatDescriptor
newDetectedFileFormatDescriptor =
  DetectedFileFormatDescriptor'
    { jsonFormatDescriptor =
        Prelude.Nothing,
      csvFormatDescriptor = Prelude.Nothing
    }

-- | Details about a JSON format.
detectedFileFormatDescriptor_jsonFormatDescriptor :: Lens.Lens' DetectedFileFormatDescriptor (Prelude.Maybe DetectedJsonFormatDescriptor)
detectedFileFormatDescriptor_jsonFormatDescriptor = Lens.lens (\DetectedFileFormatDescriptor' {jsonFormatDescriptor} -> jsonFormatDescriptor) (\s@DetectedFileFormatDescriptor' {} a -> s {jsonFormatDescriptor = a} :: DetectedFileFormatDescriptor)

-- | Details about a CSV format.
detectedFileFormatDescriptor_csvFormatDescriptor :: Lens.Lens' DetectedFileFormatDescriptor (Prelude.Maybe DetectedCsvFormatDescriptor)
detectedFileFormatDescriptor_csvFormatDescriptor = Lens.lens (\DetectedFileFormatDescriptor' {csvFormatDescriptor} -> csvFormatDescriptor) (\s@DetectedFileFormatDescriptor' {} a -> s {csvFormatDescriptor = a} :: DetectedFileFormatDescriptor)

instance Core.FromJSON DetectedFileFormatDescriptor where
  parseJSON =
    Core.withObject
      "DetectedFileFormatDescriptor"
      ( \x ->
          DetectedFileFormatDescriptor'
            Prelude.<$> (x Core..:? "JsonFormatDescriptor")
            Prelude.<*> (x Core..:? "CsvFormatDescriptor")
      )

instance
  Prelude.Hashable
    DetectedFileFormatDescriptor
  where
  hashWithSalt _salt DetectedFileFormatDescriptor' {..} =
    _salt `Prelude.hashWithSalt` jsonFormatDescriptor
      `Prelude.hashWithSalt` csvFormatDescriptor

instance Prelude.NFData DetectedFileFormatDescriptor where
  rnf DetectedFileFormatDescriptor' {..} =
    Prelude.rnf jsonFormatDescriptor
      `Prelude.seq` Prelude.rnf csvFormatDescriptor
