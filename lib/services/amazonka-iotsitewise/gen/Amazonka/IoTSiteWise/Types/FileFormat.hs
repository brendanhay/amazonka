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
-- Module      : Amazonka.IoTSiteWise.Types.FileFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.FileFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.Csv
import qualified Amazonka.Prelude as Prelude

-- | The file format of the data.
--
-- /See:/ 'newFileFormat' smart constructor.
data FileFormat = FileFormat'
  { -- | The .csv file format.
    csv :: Prelude.Maybe Csv
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'fileFormat_csv' - The .csv file format.
newFileFormat ::
  FileFormat
newFileFormat = FileFormat' {csv = Prelude.Nothing}

-- | The .csv file format.
fileFormat_csv :: Lens.Lens' FileFormat (Prelude.Maybe Csv)
fileFormat_csv = Lens.lens (\FileFormat' {csv} -> csv) (\s@FileFormat' {} a -> s {csv = a} :: FileFormat)

instance Core.FromJSON FileFormat where
  parseJSON =
    Core.withObject
      "FileFormat"
      (\x -> FileFormat' Prelude.<$> (x Core..:? "csv"))

instance Prelude.Hashable FileFormat where
  hashWithSalt _salt FileFormat' {..} =
    _salt `Prelude.hashWithSalt` csv

instance Prelude.NFData FileFormat where
  rnf FileFormat' {..} = Prelude.rnf csv

instance Core.ToJSON FileFormat where
  toJSON FileFormat' {..} =
    Core.object
      (Prelude.catMaybes [("csv" Core..=) Prelude.<$> csv])
