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
-- Module      : Amazonka.LookoutMetrics.Types.FileFormatDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.FileFormatDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.CsvFormatDescriptor
import Amazonka.LookoutMetrics.Types.JsonFormatDescriptor
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a source file\'s formatting.
--
-- /See:/ 'newFileFormatDescriptor' smart constructor.
data FileFormatDescriptor = FileFormatDescriptor'
  { -- | Contains information about how a source JSON data file should be
    -- analyzed.
    jsonFormatDescriptor :: Prelude.Maybe JsonFormatDescriptor,
    -- | Contains information about how a source CSV data file should be
    -- analyzed.
    csvFormatDescriptor :: Prelude.Maybe CsvFormatDescriptor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileFormatDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonFormatDescriptor', 'fileFormatDescriptor_jsonFormatDescriptor' - Contains information about how a source JSON data file should be
-- analyzed.
--
-- 'csvFormatDescriptor', 'fileFormatDescriptor_csvFormatDescriptor' - Contains information about how a source CSV data file should be
-- analyzed.
newFileFormatDescriptor ::
  FileFormatDescriptor
newFileFormatDescriptor =
  FileFormatDescriptor'
    { jsonFormatDescriptor =
        Prelude.Nothing,
      csvFormatDescriptor = Prelude.Nothing
    }

-- | Contains information about how a source JSON data file should be
-- analyzed.
fileFormatDescriptor_jsonFormatDescriptor :: Lens.Lens' FileFormatDescriptor (Prelude.Maybe JsonFormatDescriptor)
fileFormatDescriptor_jsonFormatDescriptor = Lens.lens (\FileFormatDescriptor' {jsonFormatDescriptor} -> jsonFormatDescriptor) (\s@FileFormatDescriptor' {} a -> s {jsonFormatDescriptor = a} :: FileFormatDescriptor)

-- | Contains information about how a source CSV data file should be
-- analyzed.
fileFormatDescriptor_csvFormatDescriptor :: Lens.Lens' FileFormatDescriptor (Prelude.Maybe CsvFormatDescriptor)
fileFormatDescriptor_csvFormatDescriptor = Lens.lens (\FileFormatDescriptor' {csvFormatDescriptor} -> csvFormatDescriptor) (\s@FileFormatDescriptor' {} a -> s {csvFormatDescriptor = a} :: FileFormatDescriptor)

instance Data.FromJSON FileFormatDescriptor where
  parseJSON =
    Data.withObject
      "FileFormatDescriptor"
      ( \x ->
          FileFormatDescriptor'
            Prelude.<$> (x Data..:? "JsonFormatDescriptor")
            Prelude.<*> (x Data..:? "CsvFormatDescriptor")
      )

instance Prelude.Hashable FileFormatDescriptor where
  hashWithSalt _salt FileFormatDescriptor' {..} =
    _salt `Prelude.hashWithSalt` jsonFormatDescriptor
      `Prelude.hashWithSalt` csvFormatDescriptor

instance Prelude.NFData FileFormatDescriptor where
  rnf FileFormatDescriptor' {..} =
    Prelude.rnf jsonFormatDescriptor
      `Prelude.seq` Prelude.rnf csvFormatDescriptor

instance Data.ToJSON FileFormatDescriptor where
  toJSON FileFormatDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JsonFormatDescriptor" Data..=)
              Prelude.<$> jsonFormatDescriptor,
            ("CsvFormatDescriptor" Data..=)
              Prelude.<$> csvFormatDescriptor
          ]
      )
