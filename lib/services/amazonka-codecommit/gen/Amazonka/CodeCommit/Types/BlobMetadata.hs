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
-- Module      : Amazonka.CodeCommit.Types.BlobMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.BlobMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific Git blob object.
--
-- /See:/ 'newBlobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { -- | The path to the blob and associated file name, if any.
    path :: Prelude.Maybe Prelude.Text,
    -- | The file mode permissions of the blob. File mode permission codes
    -- include:
    --
    -- -   @100644@ indicates read\/write
    --
    -- -   @100755@ indicates read\/write\/execute
    --
    -- -   @160000@ indicates a submodule
    --
    -- -   @120000@ indicates a symlink
    mode :: Prelude.Maybe Prelude.Text,
    -- | The full ID of the blob.
    blobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlobMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'blobMetadata_path' - The path to the blob and associated file name, if any.
--
-- 'mode', 'blobMetadata_mode' - The file mode permissions of the blob. File mode permission codes
-- include:
--
-- -   @100644@ indicates read\/write
--
-- -   @100755@ indicates read\/write\/execute
--
-- -   @160000@ indicates a submodule
--
-- -   @120000@ indicates a symlink
--
-- 'blobId', 'blobMetadata_blobId' - The full ID of the blob.
newBlobMetadata ::
  BlobMetadata
newBlobMetadata =
  BlobMetadata'
    { path = Prelude.Nothing,
      mode = Prelude.Nothing,
      blobId = Prelude.Nothing
    }

-- | The path to the blob and associated file name, if any.
blobMetadata_path :: Lens.Lens' BlobMetadata (Prelude.Maybe Prelude.Text)
blobMetadata_path = Lens.lens (\BlobMetadata' {path} -> path) (\s@BlobMetadata' {} a -> s {path = a} :: BlobMetadata)

-- | The file mode permissions of the blob. File mode permission codes
-- include:
--
-- -   @100644@ indicates read\/write
--
-- -   @100755@ indicates read\/write\/execute
--
-- -   @160000@ indicates a submodule
--
-- -   @120000@ indicates a symlink
blobMetadata_mode :: Lens.Lens' BlobMetadata (Prelude.Maybe Prelude.Text)
blobMetadata_mode = Lens.lens (\BlobMetadata' {mode} -> mode) (\s@BlobMetadata' {} a -> s {mode = a} :: BlobMetadata)

-- | The full ID of the blob.
blobMetadata_blobId :: Lens.Lens' BlobMetadata (Prelude.Maybe Prelude.Text)
blobMetadata_blobId = Lens.lens (\BlobMetadata' {blobId} -> blobId) (\s@BlobMetadata' {} a -> s {blobId = a} :: BlobMetadata)

instance Core.FromJSON BlobMetadata where
  parseJSON =
    Core.withObject
      "BlobMetadata"
      ( \x ->
          BlobMetadata'
            Prelude.<$> (x Core..:? "path")
            Prelude.<*> (x Core..:? "mode")
            Prelude.<*> (x Core..:? "blobId")
      )

instance Prelude.Hashable BlobMetadata where
  hashWithSalt _salt BlobMetadata' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` blobId

instance Prelude.NFData BlobMetadata where
  rnf BlobMetadata' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf blobId
