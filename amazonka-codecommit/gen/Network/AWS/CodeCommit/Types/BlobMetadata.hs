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
-- Module      : Network.AWS.CodeCommit.Types.BlobMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BlobMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a specific Git blob object.
--
-- /See:/ 'newBlobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { -- | The file mode permissions of the blob. File mode permission codes
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
    blobId :: Prelude.Maybe Prelude.Text,
    -- | The path to the blob and associated file name, if any.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlobMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'path', 'blobMetadata_path' - The path to the blob and associated file name, if any.
newBlobMetadata ::
  BlobMetadata
newBlobMetadata =
  BlobMetadata'
    { mode = Prelude.Nothing,
      blobId = Prelude.Nothing,
      path = Prelude.Nothing
    }

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

-- | The path to the blob and associated file name, if any.
blobMetadata_path :: Lens.Lens' BlobMetadata (Prelude.Maybe Prelude.Text)
blobMetadata_path = Lens.lens (\BlobMetadata' {path} -> path) (\s@BlobMetadata' {} a -> s {path = a} :: BlobMetadata)

instance Prelude.FromJSON BlobMetadata where
  parseJSON =
    Prelude.withObject
      "BlobMetadata"
      ( \x ->
          BlobMetadata'
            Prelude.<$> (x Prelude..:? "mode")
            Prelude.<*> (x Prelude..:? "blobId")
            Prelude.<*> (x Prelude..:? "path")
      )

instance Prelude.Hashable BlobMetadata

instance Prelude.NFData BlobMetadata
