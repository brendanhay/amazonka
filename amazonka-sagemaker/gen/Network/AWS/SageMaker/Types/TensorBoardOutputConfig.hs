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
-- Module      : Network.AWS.SageMaker.Types.TensorBoardOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardOutputConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration of storage locations for the Debugger TensorBoard output
-- data.
--
-- /See:/ 'newTensorBoardOutputConfig' smart constructor.
data TensorBoardOutputConfig = TensorBoardOutputConfig'
  { -- | Path to local storage location for tensorBoard output. Defaults to
    -- @\/opt\/ml\/output\/tensorboard@.
    localPath :: Core.Maybe Core.Text,
    -- | Path to Amazon S3 storage location for TensorBoard output.
    s3OutputPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TensorBoardOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localPath', 'tensorBoardOutputConfig_localPath' - Path to local storage location for tensorBoard output. Defaults to
-- @\/opt\/ml\/output\/tensorboard@.
--
-- 's3OutputPath', 'tensorBoardOutputConfig_s3OutputPath' - Path to Amazon S3 storage location for TensorBoard output.
newTensorBoardOutputConfig ::
  -- | 's3OutputPath'
  Core.Text ->
  TensorBoardOutputConfig
newTensorBoardOutputConfig pS3OutputPath_ =
  TensorBoardOutputConfig'
    { localPath = Core.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Path to local storage location for tensorBoard output. Defaults to
-- @\/opt\/ml\/output\/tensorboard@.
tensorBoardOutputConfig_localPath :: Lens.Lens' TensorBoardOutputConfig (Core.Maybe Core.Text)
tensorBoardOutputConfig_localPath = Lens.lens (\TensorBoardOutputConfig' {localPath} -> localPath) (\s@TensorBoardOutputConfig' {} a -> s {localPath = a} :: TensorBoardOutputConfig)

-- | Path to Amazon S3 storage location for TensorBoard output.
tensorBoardOutputConfig_s3OutputPath :: Lens.Lens' TensorBoardOutputConfig Core.Text
tensorBoardOutputConfig_s3OutputPath = Lens.lens (\TensorBoardOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@TensorBoardOutputConfig' {} a -> s {s3OutputPath = a} :: TensorBoardOutputConfig)

instance Core.FromJSON TensorBoardOutputConfig where
  parseJSON =
    Core.withObject
      "TensorBoardOutputConfig"
      ( \x ->
          TensorBoardOutputConfig'
            Core.<$> (x Core..:? "LocalPath")
            Core.<*> (x Core..: "S3OutputPath")
      )

instance Core.Hashable TensorBoardOutputConfig

instance Core.NFData TensorBoardOutputConfig

instance Core.ToJSON TensorBoardOutputConfig where
  toJSON TensorBoardOutputConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LocalPath" Core..=) Core.<$> localPath,
            Core.Just ("S3OutputPath" Core..= s3OutputPath)
          ]
      )
