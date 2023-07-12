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
-- Module      : Amazonka.SageMaker.Types.CheckpointConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CheckpointConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the output location for managed spot training
-- checkpoint data.
--
-- /See:/ 'newCheckpointConfig' smart constructor.
data CheckpointConfig = CheckpointConfig'
  { -- | (Optional) The local directory where checkpoints are written. The
    -- default directory is @\/opt\/ml\/checkpoints\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 path where you want SageMaker to store checkpoints.
    -- For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localPath', 'checkpointConfig_localPath' - (Optional) The local directory where checkpoints are written. The
-- default directory is @\/opt\/ml\/checkpoints\/@.
--
-- 's3Uri', 'checkpointConfig_s3Uri' - Identifies the S3 path where you want SageMaker to store checkpoints.
-- For example, @s3:\/\/bucket-name\/key-name-prefix@.
newCheckpointConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  CheckpointConfig
newCheckpointConfig pS3Uri_ =
  CheckpointConfig'
    { localPath = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | (Optional) The local directory where checkpoints are written. The
-- default directory is @\/opt\/ml\/checkpoints\/@.
checkpointConfig_localPath :: Lens.Lens' CheckpointConfig (Prelude.Maybe Prelude.Text)
checkpointConfig_localPath = Lens.lens (\CheckpointConfig' {localPath} -> localPath) (\s@CheckpointConfig' {} a -> s {localPath = a} :: CheckpointConfig)

-- | Identifies the S3 path where you want SageMaker to store checkpoints.
-- For example, @s3:\/\/bucket-name\/key-name-prefix@.
checkpointConfig_s3Uri :: Lens.Lens' CheckpointConfig Prelude.Text
checkpointConfig_s3Uri = Lens.lens (\CheckpointConfig' {s3Uri} -> s3Uri) (\s@CheckpointConfig' {} a -> s {s3Uri = a} :: CheckpointConfig)

instance Data.FromJSON CheckpointConfig where
  parseJSON =
    Data.withObject
      "CheckpointConfig"
      ( \x ->
          CheckpointConfig'
            Prelude.<$> (x Data..:? "LocalPath")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable CheckpointConfig where
  hashWithSalt _salt CheckpointConfig' {..} =
    _salt
      `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData CheckpointConfig where
  rnf CheckpointConfig' {..} =
    Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON CheckpointConfig where
  toJSON CheckpointConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LocalPath" Data..=) Prelude.<$> localPath,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
