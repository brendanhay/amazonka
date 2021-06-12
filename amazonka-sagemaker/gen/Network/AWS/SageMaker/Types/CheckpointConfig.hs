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
-- Module      : Network.AWS.SageMaker.Types.CheckpointConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CheckpointConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the output location for managed spot training
-- checkpoint data.
--
-- /See:/ 'newCheckpointConfig' smart constructor.
data CheckpointConfig = CheckpointConfig'
  { -- | (Optional) The local directory where checkpoints are written. The
    -- default directory is @\/opt\/ml\/checkpoints\/@.
    localPath :: Core.Maybe Core.Text,
    -- | Identifies the S3 path where you want Amazon SageMaker to store
    -- checkpoints. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 's3Uri', 'checkpointConfig_s3Uri' - Identifies the S3 path where you want Amazon SageMaker to store
-- checkpoints. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newCheckpointConfig ::
  -- | 's3Uri'
  Core.Text ->
  CheckpointConfig
newCheckpointConfig pS3Uri_ =
  CheckpointConfig'
    { localPath = Core.Nothing,
      s3Uri = pS3Uri_
    }

-- | (Optional) The local directory where checkpoints are written. The
-- default directory is @\/opt\/ml\/checkpoints\/@.
checkpointConfig_localPath :: Lens.Lens' CheckpointConfig (Core.Maybe Core.Text)
checkpointConfig_localPath = Lens.lens (\CheckpointConfig' {localPath} -> localPath) (\s@CheckpointConfig' {} a -> s {localPath = a} :: CheckpointConfig)

-- | Identifies the S3 path where you want Amazon SageMaker to store
-- checkpoints. For example, @s3:\/\/bucket-name\/key-name-prefix@.
checkpointConfig_s3Uri :: Lens.Lens' CheckpointConfig Core.Text
checkpointConfig_s3Uri = Lens.lens (\CheckpointConfig' {s3Uri} -> s3Uri) (\s@CheckpointConfig' {} a -> s {s3Uri = a} :: CheckpointConfig)

instance Core.FromJSON CheckpointConfig where
  parseJSON =
    Core.withObject
      "CheckpointConfig"
      ( \x ->
          CheckpointConfig'
            Core.<$> (x Core..:? "LocalPath")
            Core.<*> (x Core..: "S3Uri")
      )

instance Core.Hashable CheckpointConfig

instance Core.NFData CheckpointConfig

instance Core.ToJSON CheckpointConfig where
  toJSON CheckpointConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LocalPath" Core..=) Core.<$> localPath,
            Core.Just ("S3Uri" Core..= s3Uri)
          ]
      )
