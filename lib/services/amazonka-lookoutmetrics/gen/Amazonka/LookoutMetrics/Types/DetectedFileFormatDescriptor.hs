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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DetectedFileFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DetectedCsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.DetectedJsonFormatDescriptor
import qualified Amazonka.Prelude as Prelude

-- | Properties of an inferred data format.
--
-- /See:/ 'newDetectedFileFormatDescriptor' smart constructor.
data DetectedFileFormatDescriptor = DetectedFileFormatDescriptor'
  { -- | Details about a CSV format.
    csvFormatDescriptor :: Prelude.Maybe DetectedCsvFormatDescriptor,
    -- | Details about a JSON format.
    jsonFormatDescriptor :: Prelude.Maybe DetectedJsonFormatDescriptor
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
-- 'csvFormatDescriptor', 'detectedFileFormatDescriptor_csvFormatDescriptor' - Details about a CSV format.
--
-- 'jsonFormatDescriptor', 'detectedFileFormatDescriptor_jsonFormatDescriptor' - Details about a JSON format.
newDetectedFileFormatDescriptor ::
  DetectedFileFormatDescriptor
newDetectedFileFormatDescriptor =
  DetectedFileFormatDescriptor'
    { csvFormatDescriptor =
        Prelude.Nothing,
      jsonFormatDescriptor = Prelude.Nothing
    }

-- | Details about a CSV format.
detectedFileFormatDescriptor_csvFormatDescriptor :: Lens.Lens' DetectedFileFormatDescriptor (Prelude.Maybe DetectedCsvFormatDescriptor)
detectedFileFormatDescriptor_csvFormatDescriptor = Lens.lens (\DetectedFileFormatDescriptor' {csvFormatDescriptor} -> csvFormatDescriptor) (\s@DetectedFileFormatDescriptor' {} a -> s {csvFormatDescriptor = a} :: DetectedFileFormatDescriptor)

-- | Details about a JSON format.
detectedFileFormatDescriptor_jsonFormatDescriptor :: Lens.Lens' DetectedFileFormatDescriptor (Prelude.Maybe DetectedJsonFormatDescriptor)
detectedFileFormatDescriptor_jsonFormatDescriptor = Lens.lens (\DetectedFileFormatDescriptor' {jsonFormatDescriptor} -> jsonFormatDescriptor) (\s@DetectedFileFormatDescriptor' {} a -> s {jsonFormatDescriptor = a} :: DetectedFileFormatDescriptor)

instance Data.FromJSON DetectedFileFormatDescriptor where
  parseJSON =
    Data.withObject
      "DetectedFileFormatDescriptor"
      ( \x ->
          DetectedFileFormatDescriptor'
            Prelude.<$> (x Data..:? "CsvFormatDescriptor")
            Prelude.<*> (x Data..:? "JsonFormatDescriptor")
      )

instance
  Prelude.Hashable
    DetectedFileFormatDescriptor
  where
  hashWithSalt _salt DetectedFileFormatDescriptor' {..} =
    _salt `Prelude.hashWithSalt` csvFormatDescriptor
      `Prelude.hashWithSalt` jsonFormatDescriptor

instance Prelude.NFData DetectedFileFormatDescriptor where
  rnf DetectedFileFormatDescriptor' {..} =
    Prelude.rnf csvFormatDescriptor
      `Prelude.seq` Prelude.rnf jsonFormatDescriptor
