{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TensorBoardOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardOutputConfig
  ( TensorBoardOutputConfig (..),

    -- * Smart constructor
    mkTensorBoardOutputConfig,

    -- * Lenses
    tbocS3OutputPath,
    tbocLocalPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration of storage locations for TensorBoard output.
--
-- /See:/ 'mkTensorBoardOutputConfig' smart constructor.
data TensorBoardOutputConfig = TensorBoardOutputConfig'
  { -- | Path to Amazon S3 storage location for TensorBoard output.
    s3OutputPath :: Lude.Text,
    -- | Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
    localPath :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TensorBoardOutputConfig' with the minimum fields required to make a request.
--
-- * 's3OutputPath' - Path to Amazon S3 storage location for TensorBoard output.
-- * 'localPath' - Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
mkTensorBoardOutputConfig ::
  -- | 's3OutputPath'
  Lude.Text ->
  TensorBoardOutputConfig
mkTensorBoardOutputConfig pS3OutputPath_ =
  TensorBoardOutputConfig'
    { s3OutputPath = pS3OutputPath_,
      localPath = Lude.Nothing
    }

-- | Path to Amazon S3 storage location for TensorBoard output.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbocS3OutputPath :: Lens.Lens' TensorBoardOutputConfig Lude.Text
tbocS3OutputPath = Lens.lens (s3OutputPath :: TensorBoardOutputConfig -> Lude.Text) (\s a -> s {s3OutputPath = a} :: TensorBoardOutputConfig)
{-# DEPRECATED tbocS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbocLocalPath :: Lens.Lens' TensorBoardOutputConfig (Lude.Maybe Lude.Text)
tbocLocalPath = Lens.lens (localPath :: TensorBoardOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {localPath = a} :: TensorBoardOutputConfig)
{-# DEPRECATED tbocLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

instance Lude.FromJSON TensorBoardOutputConfig where
  parseJSON =
    Lude.withObject
      "TensorBoardOutputConfig"
      ( \x ->
          TensorBoardOutputConfig'
            Lude.<$> (x Lude..: "S3OutputPath") Lude.<*> (x Lude..:? "LocalPath")
      )

instance Lude.ToJSON TensorBoardOutputConfig where
  toJSON TensorBoardOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3OutputPath" Lude..= s3OutputPath),
            ("LocalPath" Lude..=) Lude.<$> localPath
          ]
      )
