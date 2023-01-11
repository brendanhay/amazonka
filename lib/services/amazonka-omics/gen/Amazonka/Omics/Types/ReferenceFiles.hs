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
-- Module      : Amazonka.Omics.Types.ReferenceFiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceFiles where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.FileInformation
import qualified Amazonka.Prelude as Prelude

-- | A set of genome reference files.
--
-- /See:/ 'newReferenceFiles' smart constructor.
data ReferenceFiles = ReferenceFiles'
  { -- | The files\' index.
    index :: Prelude.Maybe FileInformation,
    -- | The source file\'s location in Amazon S3.
    source :: Prelude.Maybe FileInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceFiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'referenceFiles_index' - The files\' index.
--
-- 'source', 'referenceFiles_source' - The source file\'s location in Amazon S3.
newReferenceFiles ::
  ReferenceFiles
newReferenceFiles =
  ReferenceFiles'
    { index = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The files\' index.
referenceFiles_index :: Lens.Lens' ReferenceFiles (Prelude.Maybe FileInformation)
referenceFiles_index = Lens.lens (\ReferenceFiles' {index} -> index) (\s@ReferenceFiles' {} a -> s {index = a} :: ReferenceFiles)

-- | The source file\'s location in Amazon S3.
referenceFiles_source :: Lens.Lens' ReferenceFiles (Prelude.Maybe FileInformation)
referenceFiles_source = Lens.lens (\ReferenceFiles' {source} -> source) (\s@ReferenceFiles' {} a -> s {source = a} :: ReferenceFiles)

instance Data.FromJSON ReferenceFiles where
  parseJSON =
    Data.withObject
      "ReferenceFiles"
      ( \x ->
          ReferenceFiles'
            Prelude.<$> (x Data..:? "index")
            Prelude.<*> (x Data..:? "source")
      )

instance Prelude.Hashable ReferenceFiles where
  hashWithSalt _salt ReferenceFiles' {..} =
    _salt `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` source

instance Prelude.NFData ReferenceFiles where
  rnf ReferenceFiles' {..} =
    Prelude.rnf index `Prelude.seq` Prelude.rnf source
