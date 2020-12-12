{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformDataSource
  ( TransformDataSource (..),

    -- * Smart constructor
    mkTransformDataSource,

    -- * Lenses
    tdsS3DataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TransformS3DataSource

-- | Describes the location of the channel data.
--
-- /See:/ 'mkTransformDataSource' smart constructor.
newtype TransformDataSource = TransformDataSource'
  { s3DataSource ::
      TransformS3DataSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformDataSource' with the minimum fields required to make a request.
--
-- * 's3DataSource' - The S3 location of the data source that is associated with a channel.
mkTransformDataSource ::
  -- | 's3DataSource'
  TransformS3DataSource ->
  TransformDataSource
mkTransformDataSource pS3DataSource_ =
  TransformDataSource' {s3DataSource = pS3DataSource_}

-- | The S3 location of the data source that is associated with a channel.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsS3DataSource :: Lens.Lens' TransformDataSource TransformS3DataSource
tdsS3DataSource = Lens.lens (s3DataSource :: TransformDataSource -> TransformS3DataSource) (\s a -> s {s3DataSource = a} :: TransformDataSource)
{-# DEPRECATED tdsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

instance Lude.FromJSON TransformDataSource where
  parseJSON =
    Lude.withObject
      "TransformDataSource"
      (\x -> TransformDataSource' Lude.<$> (x Lude..: "S3DataSource"))

instance Lude.ToJSON TransformDataSource where
  toJSON TransformDataSource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("S3DataSource" Lude..= s3DataSource)])
