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
-- Module      : Amazonka.IoTSiteWise.Types.File
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.File where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The file in Amazon S3 where your data is saved.
--
-- /See:/ 'newFile' smart constructor.
data File = File'
  { -- | The version ID to identify a specific version of the Amazon S3 object
    -- that contains your data.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket from which data is imported.
    bucket :: Prelude.Text,
    -- | The key of the Amazon S3 object that contains your data. Each object has
    -- a key that is a unique identifier. Each object has exactly one key.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'File' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'file_versionId' - The version ID to identify a specific version of the Amazon S3 object
-- that contains your data.
--
-- 'bucket', 'file_bucket' - The name of the Amazon S3 bucket from which data is imported.
--
-- 'key', 'file_key' - The key of the Amazon S3 object that contains your data. Each object has
-- a key that is a unique identifier. Each object has exactly one key.
newFile ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  File
newFile pBucket_ pKey_ =
  File'
    { versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The version ID to identify a specific version of the Amazon S3 object
-- that contains your data.
file_versionId :: Lens.Lens' File (Prelude.Maybe Prelude.Text)
file_versionId = Lens.lens (\File' {versionId} -> versionId) (\s@File' {} a -> s {versionId = a} :: File)

-- | The name of the Amazon S3 bucket from which data is imported.
file_bucket :: Lens.Lens' File Prelude.Text
file_bucket = Lens.lens (\File' {bucket} -> bucket) (\s@File' {} a -> s {bucket = a} :: File)

-- | The key of the Amazon S3 object that contains your data. Each object has
-- a key that is a unique identifier. Each object has exactly one key.
file_key :: Lens.Lens' File Prelude.Text
file_key = Lens.lens (\File' {key} -> key) (\s@File' {} a -> s {key = a} :: File)

instance Data.FromJSON File where
  parseJSON =
    Data.withObject
      "File"
      ( \x ->
          File'
            Prelude.<$> (x Data..:? "versionId")
            Prelude.<*> (x Data..: "bucket")
            Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable File where
  hashWithSalt _salt File' {..} =
    _salt
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData File where
  rnf File' {..} =
    Prelude.rnf versionId `Prelude.seq`
      Prelude.rnf bucket `Prelude.seq`
        Prelude.rnf key

instance Data.ToJSON File where
  toJSON File' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("versionId" Data..=) Prelude.<$> versionId,
            Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("key" Data..= key)
          ]
      )
