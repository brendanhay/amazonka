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
-- Module      : Amazonka.Omics.Types.ReadSetFiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetFiles where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.FileInformation
import qualified Amazonka.Prelude as Prelude

-- | Files in a read set.
--
-- /See:/ 'newReadSetFiles' smart constructor.
data ReadSetFiles = ReadSetFiles'
  { -- | The files\' index.
    index :: Prelude.Maybe FileInformation,
    -- | The location of the first file in Amazon S3.
    source1 :: Prelude.Maybe FileInformation,
    -- | The location of the second file in Amazon S3.
    source2 :: Prelude.Maybe FileInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadSetFiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'readSetFiles_index' - The files\' index.
--
-- 'source1', 'readSetFiles_source1' - The location of the first file in Amazon S3.
--
-- 'source2', 'readSetFiles_source2' - The location of the second file in Amazon S3.
newReadSetFiles ::
  ReadSetFiles
newReadSetFiles =
  ReadSetFiles'
    { index = Prelude.Nothing,
      source1 = Prelude.Nothing,
      source2 = Prelude.Nothing
    }

-- | The files\' index.
readSetFiles_index :: Lens.Lens' ReadSetFiles (Prelude.Maybe FileInformation)
readSetFiles_index = Lens.lens (\ReadSetFiles' {index} -> index) (\s@ReadSetFiles' {} a -> s {index = a} :: ReadSetFiles)

-- | The location of the first file in Amazon S3.
readSetFiles_source1 :: Lens.Lens' ReadSetFiles (Prelude.Maybe FileInformation)
readSetFiles_source1 = Lens.lens (\ReadSetFiles' {source1} -> source1) (\s@ReadSetFiles' {} a -> s {source1 = a} :: ReadSetFiles)

-- | The location of the second file in Amazon S3.
readSetFiles_source2 :: Lens.Lens' ReadSetFiles (Prelude.Maybe FileInformation)
readSetFiles_source2 = Lens.lens (\ReadSetFiles' {source2} -> source2) (\s@ReadSetFiles' {} a -> s {source2 = a} :: ReadSetFiles)

instance Data.FromJSON ReadSetFiles where
  parseJSON =
    Data.withObject
      "ReadSetFiles"
      ( \x ->
          ReadSetFiles'
            Prelude.<$> (x Data..:? "index")
            Prelude.<*> (x Data..:? "source1")
            Prelude.<*> (x Data..:? "source2")
      )

instance Prelude.Hashable ReadSetFiles where
  hashWithSalt _salt ReadSetFiles' {..} =
    _salt `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` source1
      `Prelude.hashWithSalt` source2

instance Prelude.NFData ReadSetFiles where
  rnf ReadSetFiles' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf source1
      `Prelude.seq` Prelude.rnf source2
