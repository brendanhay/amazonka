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
-- Module      : Amazonka.Omics.Types.FileInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.FileInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a file.
--
-- /See:/ 'newFileInformation' smart constructor.
data FileInformation = FileInformation'
  { -- | The file\'s content length.
    contentLength :: Prelude.Maybe Prelude.Natural,
    -- | The file\'s part size.
    partSize :: Prelude.Maybe Prelude.Natural,
    -- | The file\'s total parts.
    totalParts :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentLength', 'fileInformation_contentLength' - The file\'s content length.
--
-- 'partSize', 'fileInformation_partSize' - The file\'s part size.
--
-- 'totalParts', 'fileInformation_totalParts' - The file\'s total parts.
newFileInformation ::
  FileInformation
newFileInformation =
  FileInformation'
    { contentLength = Prelude.Nothing,
      partSize = Prelude.Nothing,
      totalParts = Prelude.Nothing
    }

-- | The file\'s content length.
fileInformation_contentLength :: Lens.Lens' FileInformation (Prelude.Maybe Prelude.Natural)
fileInformation_contentLength = Lens.lens (\FileInformation' {contentLength} -> contentLength) (\s@FileInformation' {} a -> s {contentLength = a} :: FileInformation)

-- | The file\'s part size.
fileInformation_partSize :: Lens.Lens' FileInformation (Prelude.Maybe Prelude.Natural)
fileInformation_partSize = Lens.lens (\FileInformation' {partSize} -> partSize) (\s@FileInformation' {} a -> s {partSize = a} :: FileInformation)

-- | The file\'s total parts.
fileInformation_totalParts :: Lens.Lens' FileInformation (Prelude.Maybe Prelude.Natural)
fileInformation_totalParts = Lens.lens (\FileInformation' {totalParts} -> totalParts) (\s@FileInformation' {} a -> s {totalParts = a} :: FileInformation)

instance Data.FromJSON FileInformation where
  parseJSON =
    Data.withObject
      "FileInformation"
      ( \x ->
          FileInformation'
            Prelude.<$> (x Data..:? "contentLength")
            Prelude.<*> (x Data..:? "partSize")
            Prelude.<*> (x Data..:? "totalParts")
      )

instance Prelude.Hashable FileInformation where
  hashWithSalt _salt FileInformation' {..} =
    _salt
      `Prelude.hashWithSalt` contentLength
      `Prelude.hashWithSalt` partSize
      `Prelude.hashWithSalt` totalParts

instance Prelude.NFData FileInformation where
  rnf FileInformation' {..} =
    Prelude.rnf contentLength
      `Prelude.seq` Prelude.rnf partSize
      `Prelude.seq` Prelude.rnf totalParts
