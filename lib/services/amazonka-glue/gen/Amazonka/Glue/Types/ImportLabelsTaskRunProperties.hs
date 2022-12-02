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
-- Module      : Amazonka.Glue.Types.ImportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ImportLabelsTaskRunProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration properties for an importing labels task run.
--
-- /See:/ 'newImportLabelsTaskRunProperties' smart constructor.
data ImportLabelsTaskRunProperties = ImportLabelsTaskRunProperties'
  { -- | The Amazon Simple Storage Service (Amazon S3) path from where you will
    -- import the labels.
    inputS3Path :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to overwrite your existing labels.
    replace :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportLabelsTaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputS3Path', 'importLabelsTaskRunProperties_inputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you will
-- import the labels.
--
-- 'replace', 'importLabelsTaskRunProperties_replace' - Indicates whether to overwrite your existing labels.
newImportLabelsTaskRunProperties ::
  ImportLabelsTaskRunProperties
newImportLabelsTaskRunProperties =
  ImportLabelsTaskRunProperties'
    { inputS3Path =
        Prelude.Nothing,
      replace = Prelude.Nothing
    }

-- | The Amazon Simple Storage Service (Amazon S3) path from where you will
-- import the labels.
importLabelsTaskRunProperties_inputS3Path :: Lens.Lens' ImportLabelsTaskRunProperties (Prelude.Maybe Prelude.Text)
importLabelsTaskRunProperties_inputS3Path = Lens.lens (\ImportLabelsTaskRunProperties' {inputS3Path} -> inputS3Path) (\s@ImportLabelsTaskRunProperties' {} a -> s {inputS3Path = a} :: ImportLabelsTaskRunProperties)

-- | Indicates whether to overwrite your existing labels.
importLabelsTaskRunProperties_replace :: Lens.Lens' ImportLabelsTaskRunProperties (Prelude.Maybe Prelude.Bool)
importLabelsTaskRunProperties_replace = Lens.lens (\ImportLabelsTaskRunProperties' {replace} -> replace) (\s@ImportLabelsTaskRunProperties' {} a -> s {replace = a} :: ImportLabelsTaskRunProperties)

instance Data.FromJSON ImportLabelsTaskRunProperties where
  parseJSON =
    Data.withObject
      "ImportLabelsTaskRunProperties"
      ( \x ->
          ImportLabelsTaskRunProperties'
            Prelude.<$> (x Data..:? "InputS3Path")
            Prelude.<*> (x Data..:? "Replace")
      )

instance
  Prelude.Hashable
    ImportLabelsTaskRunProperties
  where
  hashWithSalt _salt ImportLabelsTaskRunProperties' {..} =
    _salt `Prelude.hashWithSalt` inputS3Path
      `Prelude.hashWithSalt` replace

instance Prelude.NFData ImportLabelsTaskRunProperties where
  rnf ImportLabelsTaskRunProperties' {..} =
    Prelude.rnf inputS3Path
      `Prelude.seq` Prelude.rnf replace
