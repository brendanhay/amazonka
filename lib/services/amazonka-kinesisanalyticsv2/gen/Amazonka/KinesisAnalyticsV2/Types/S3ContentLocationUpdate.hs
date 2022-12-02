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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3ContentLocationUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3ContentLocationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an update for the Amazon S3 code content location for an
-- application.
--
-- /See:/ 'newS3ContentLocationUpdate' smart constructor.
data S3ContentLocationUpdate = S3ContentLocationUpdate'
  { -- | The new version of the object containing the application code.
    objectVersionUpdate :: Prelude.Maybe Prelude.Text,
    -- | The new Amazon Resource Name (ARN) for the S3 bucket containing the
    -- application code.
    bucketARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | The new file key for the object containing the application code.
    fileKeyUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ContentLocationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectVersionUpdate', 's3ContentLocationUpdate_objectVersionUpdate' - The new version of the object containing the application code.
--
-- 'bucketARNUpdate', 's3ContentLocationUpdate_bucketARNUpdate' - The new Amazon Resource Name (ARN) for the S3 bucket containing the
-- application code.
--
-- 'fileKeyUpdate', 's3ContentLocationUpdate_fileKeyUpdate' - The new file key for the object containing the application code.
newS3ContentLocationUpdate ::
  S3ContentLocationUpdate
newS3ContentLocationUpdate =
  S3ContentLocationUpdate'
    { objectVersionUpdate =
        Prelude.Nothing,
      bucketARNUpdate = Prelude.Nothing,
      fileKeyUpdate = Prelude.Nothing
    }

-- | The new version of the object containing the application code.
s3ContentLocationUpdate_objectVersionUpdate :: Lens.Lens' S3ContentLocationUpdate (Prelude.Maybe Prelude.Text)
s3ContentLocationUpdate_objectVersionUpdate = Lens.lens (\S3ContentLocationUpdate' {objectVersionUpdate} -> objectVersionUpdate) (\s@S3ContentLocationUpdate' {} a -> s {objectVersionUpdate = a} :: S3ContentLocationUpdate)

-- | The new Amazon Resource Name (ARN) for the S3 bucket containing the
-- application code.
s3ContentLocationUpdate_bucketARNUpdate :: Lens.Lens' S3ContentLocationUpdate (Prelude.Maybe Prelude.Text)
s3ContentLocationUpdate_bucketARNUpdate = Lens.lens (\S3ContentLocationUpdate' {bucketARNUpdate} -> bucketARNUpdate) (\s@S3ContentLocationUpdate' {} a -> s {bucketARNUpdate = a} :: S3ContentLocationUpdate)

-- | The new file key for the object containing the application code.
s3ContentLocationUpdate_fileKeyUpdate :: Lens.Lens' S3ContentLocationUpdate (Prelude.Maybe Prelude.Text)
s3ContentLocationUpdate_fileKeyUpdate = Lens.lens (\S3ContentLocationUpdate' {fileKeyUpdate} -> fileKeyUpdate) (\s@S3ContentLocationUpdate' {} a -> s {fileKeyUpdate = a} :: S3ContentLocationUpdate)

instance Prelude.Hashable S3ContentLocationUpdate where
  hashWithSalt _salt S3ContentLocationUpdate' {..} =
    _salt `Prelude.hashWithSalt` objectVersionUpdate
      `Prelude.hashWithSalt` bucketARNUpdate
      `Prelude.hashWithSalt` fileKeyUpdate

instance Prelude.NFData S3ContentLocationUpdate where
  rnf S3ContentLocationUpdate' {..} =
    Prelude.rnf objectVersionUpdate
      `Prelude.seq` Prelude.rnf bucketARNUpdate
      `Prelude.seq` Prelude.rnf fileKeyUpdate

instance Data.ToJSON S3ContentLocationUpdate where
  toJSON S3ContentLocationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectVersionUpdate" Data..=)
              Prelude.<$> objectVersionUpdate,
            ("BucketARNUpdate" Data..=)
              Prelude.<$> bucketARNUpdate,
            ("FileKeyUpdate" Data..=) Prelude.<$> fileKeyUpdate
          ]
      )
