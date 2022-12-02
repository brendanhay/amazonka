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
-- Module      : Amazonka.Glue.Types.ExportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ExportLabelsTaskRunProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration properties for an exporting labels task run.
--
-- /See:/ 'newExportLabelsTaskRunProperties' smart constructor.
data ExportLabelsTaskRunProperties = ExportLabelsTaskRunProperties'
  { -- | The Amazon Simple Storage Service (Amazon S3) path where you will export
    -- the labels.
    outputS3Path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportLabelsTaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputS3Path', 'exportLabelsTaskRunProperties_outputS3Path' - The Amazon Simple Storage Service (Amazon S3) path where you will export
-- the labels.
newExportLabelsTaskRunProperties ::
  ExportLabelsTaskRunProperties
newExportLabelsTaskRunProperties =
  ExportLabelsTaskRunProperties'
    { outputS3Path =
        Prelude.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path where you will export
-- the labels.
exportLabelsTaskRunProperties_outputS3Path :: Lens.Lens' ExportLabelsTaskRunProperties (Prelude.Maybe Prelude.Text)
exportLabelsTaskRunProperties_outputS3Path = Lens.lens (\ExportLabelsTaskRunProperties' {outputS3Path} -> outputS3Path) (\s@ExportLabelsTaskRunProperties' {} a -> s {outputS3Path = a} :: ExportLabelsTaskRunProperties)

instance Data.FromJSON ExportLabelsTaskRunProperties where
  parseJSON =
    Data.withObject
      "ExportLabelsTaskRunProperties"
      ( \x ->
          ExportLabelsTaskRunProperties'
            Prelude.<$> (x Data..:? "OutputS3Path")
      )

instance
  Prelude.Hashable
    ExportLabelsTaskRunProperties
  where
  hashWithSalt _salt ExportLabelsTaskRunProperties' {..} =
    _salt `Prelude.hashWithSalt` outputS3Path

instance Prelude.NFData ExportLabelsTaskRunProperties where
  rnf ExportLabelsTaskRunProperties' {..} =
    Prelude.rnf outputS3Path
