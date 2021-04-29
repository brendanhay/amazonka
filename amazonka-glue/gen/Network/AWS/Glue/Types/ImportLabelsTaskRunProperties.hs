{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ImportLabelsTaskRunProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies configuration properties for an importing labels task run.
--
-- /See:/ 'newImportLabelsTaskRunProperties' smart constructor.
data ImportLabelsTaskRunProperties = ImportLabelsTaskRunProperties'
  { -- | Indicates whether to overwrite your existing labels.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Simple Storage Service (Amazon S3) path from where you will
    -- import the labels.
    inputS3Path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportLabelsTaskRunProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replace', 'importLabelsTaskRunProperties_replace' - Indicates whether to overwrite your existing labels.
--
-- 'inputS3Path', 'importLabelsTaskRunProperties_inputS3Path' - The Amazon Simple Storage Service (Amazon S3) path from where you will
-- import the labels.
newImportLabelsTaskRunProperties ::
  ImportLabelsTaskRunProperties
newImportLabelsTaskRunProperties =
  ImportLabelsTaskRunProperties'
    { replace =
        Prelude.Nothing,
      inputS3Path = Prelude.Nothing
    }

-- | Indicates whether to overwrite your existing labels.
importLabelsTaskRunProperties_replace :: Lens.Lens' ImportLabelsTaskRunProperties (Prelude.Maybe Prelude.Bool)
importLabelsTaskRunProperties_replace = Lens.lens (\ImportLabelsTaskRunProperties' {replace} -> replace) (\s@ImportLabelsTaskRunProperties' {} a -> s {replace = a} :: ImportLabelsTaskRunProperties)

-- | The Amazon Simple Storage Service (Amazon S3) path from where you will
-- import the labels.
importLabelsTaskRunProperties_inputS3Path :: Lens.Lens' ImportLabelsTaskRunProperties (Prelude.Maybe Prelude.Text)
importLabelsTaskRunProperties_inputS3Path = Lens.lens (\ImportLabelsTaskRunProperties' {inputS3Path} -> inputS3Path) (\s@ImportLabelsTaskRunProperties' {} a -> s {inputS3Path = a} :: ImportLabelsTaskRunProperties)

instance
  Prelude.FromJSON
    ImportLabelsTaskRunProperties
  where
  parseJSON =
    Prelude.withObject
      "ImportLabelsTaskRunProperties"
      ( \x ->
          ImportLabelsTaskRunProperties'
            Prelude.<$> (x Prelude..:? "Replace")
            Prelude.<*> (x Prelude..:? "InputS3Path")
      )

instance
  Prelude.Hashable
    ImportLabelsTaskRunProperties

instance Prelude.NFData ImportLabelsTaskRunProperties
