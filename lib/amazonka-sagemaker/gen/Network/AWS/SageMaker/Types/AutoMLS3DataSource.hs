{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLS3DataSource
  ( AutoMLS3DataSource (..),

    -- * Smart constructor
    mkAutoMLS3DataSource,

    -- * Lenses
    amlsdsS3DataType,
    amlsdsS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLS3DataType

-- | The Amazon S3 data source.
--
-- /See:/ 'mkAutoMLS3DataSource' smart constructor.
data AutoMLS3DataSource = AutoMLS3DataSource'
  { -- | The data type.
    s3DataType :: AutoMLS3DataType,
    -- | The URL to the Amazon S3 data source.
    s3URI :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLS3DataSource' with the minimum fields required to make a request.
--
-- * 's3DataType' - The data type.
-- * 's3URI' - The URL to the Amazon S3 data source.
mkAutoMLS3DataSource ::
  -- | 's3DataType'
  AutoMLS3DataType ->
  -- | 's3URI'
  Lude.Text ->
  AutoMLS3DataSource
mkAutoMLS3DataSource pS3DataType_ pS3URI_ =
  AutoMLS3DataSource' {s3DataType = pS3DataType_, s3URI = pS3URI_}

-- | The data type.
--
-- /Note:/ Consider using 's3DataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlsdsS3DataType :: Lens.Lens' AutoMLS3DataSource AutoMLS3DataType
amlsdsS3DataType = Lens.lens (s3DataType :: AutoMLS3DataSource -> AutoMLS3DataType) (\s a -> s {s3DataType = a} :: AutoMLS3DataSource)
{-# DEPRECATED amlsdsS3DataType "Use generic-lens or generic-optics with 's3DataType' instead." #-}

-- | The URL to the Amazon S3 data source.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlsdsS3URI :: Lens.Lens' AutoMLS3DataSource Lude.Text
amlsdsS3URI = Lens.lens (s3URI :: AutoMLS3DataSource -> Lude.Text) (\s a -> s {s3URI = a} :: AutoMLS3DataSource)
{-# DEPRECATED amlsdsS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON AutoMLS3DataSource where
  parseJSON =
    Lude.withObject
      "AutoMLS3DataSource"
      ( \x ->
          AutoMLS3DataSource'
            Lude.<$> (x Lude..: "S3DataType") Lude.<*> (x Lude..: "S3Uri")
      )

instance Lude.ToJSON AutoMLS3DataSource where
  toJSON AutoMLS3DataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3DataType" Lude..= s3DataType),
            Lude.Just ("S3Uri" Lude..= s3URI)
          ]
      )
