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
-- Module      : Amazonka.Omics.Types.SourceFiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.SourceFiles where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Source files for a sequence.
--
-- /See:/ 'newSourceFiles' smart constructor.
data SourceFiles = SourceFiles'
  { -- | The location of the second file in Amazon S3.
    source2 :: Prelude.Maybe Prelude.Text,
    -- | The location of the first file in Amazon S3.
    source1 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceFiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source2', 'sourceFiles_source2' - The location of the second file in Amazon S3.
--
-- 'source1', 'sourceFiles_source1' - The location of the first file in Amazon S3.
newSourceFiles ::
  -- | 'source1'
  Prelude.Text ->
  SourceFiles
newSourceFiles pSource1_ =
  SourceFiles'
    { source2 = Prelude.Nothing,
      source1 = pSource1_
    }

-- | The location of the second file in Amazon S3.
sourceFiles_source2 :: Lens.Lens' SourceFiles (Prelude.Maybe Prelude.Text)
sourceFiles_source2 = Lens.lens (\SourceFiles' {source2} -> source2) (\s@SourceFiles' {} a -> s {source2 = a} :: SourceFiles)

-- | The location of the first file in Amazon S3.
sourceFiles_source1 :: Lens.Lens' SourceFiles Prelude.Text
sourceFiles_source1 = Lens.lens (\SourceFiles' {source1} -> source1) (\s@SourceFiles' {} a -> s {source1 = a} :: SourceFiles)

instance Data.FromJSON SourceFiles where
  parseJSON =
    Data.withObject
      "SourceFiles"
      ( \x ->
          SourceFiles'
            Prelude.<$> (x Data..:? "source2")
            Prelude.<*> (x Data..: "source1")
      )

instance Prelude.Hashable SourceFiles where
  hashWithSalt _salt SourceFiles' {..} =
    _salt
      `Prelude.hashWithSalt` source2
      `Prelude.hashWithSalt` source1

instance Prelude.NFData SourceFiles where
  rnf SourceFiles' {..} =
    Prelude.rnf source2
      `Prelude.seq` Prelude.rnf source1

instance Data.ToJSON SourceFiles where
  toJSON SourceFiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("source2" Data..=) Prelude.<$> source2,
            Prelude.Just ("source1" Data..= source1)
          ]
      )
