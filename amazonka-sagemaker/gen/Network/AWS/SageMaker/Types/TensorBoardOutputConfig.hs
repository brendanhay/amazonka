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
-- Module      : Network.AWS.SageMaker.Types.TensorBoardOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardOutputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration of storage locations for the Debugger TensorBoard output
-- data.
--
-- /See:/ 'newTensorBoardOutputConfig' smart constructor.
data TensorBoardOutputConfig = TensorBoardOutputConfig'
  { -- | Path to local storage location for tensorBoard output. Defaults to
    -- @\/opt\/ml\/output\/tensorboard@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | Path to Amazon S3 storage location for TensorBoard output.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  TensorBoardOutputConfig
newTensorBoardOutputConfig pS3OutputPath_ =
  TensorBoardOutputConfig'
    { localPath =
        Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Path to local storage location for tensorBoard output. Defaults to
-- @\/opt\/ml\/output\/tensorboard@.
tensorBoardOutputConfig_localPath :: Lens.Lens' TensorBoardOutputConfig (Prelude.Maybe Prelude.Text)
tensorBoardOutputConfig_localPath = Lens.lens (\TensorBoardOutputConfig' {localPath} -> localPath) (\s@TensorBoardOutputConfig' {} a -> s {localPath = a} :: TensorBoardOutputConfig)

-- | Path to Amazon S3 storage location for TensorBoard output.
tensorBoardOutputConfig_s3OutputPath :: Lens.Lens' TensorBoardOutputConfig Prelude.Text
tensorBoardOutputConfig_s3OutputPath = Lens.lens (\TensorBoardOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@TensorBoardOutputConfig' {} a -> s {s3OutputPath = a} :: TensorBoardOutputConfig)

instance Prelude.FromJSON TensorBoardOutputConfig where
  parseJSON =
    Prelude.withObject
      "TensorBoardOutputConfig"
      ( \x ->
          TensorBoardOutputConfig'
            Prelude.<$> (x Prelude..:? "LocalPath")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable TensorBoardOutputConfig

instance Prelude.NFData TensorBoardOutputConfig

instance Prelude.ToJSON TensorBoardOutputConfig where
  toJSON TensorBoardOutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LocalPath" Prelude..=) Prelude.<$> localPath,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )
