-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobS3DataSource
  ( LabelingJobS3DataSource (..),

    -- * Smart constructor
    mkLabelingJobS3DataSource,

    -- * Lenses
    ljsdsManifestS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon S3 location of the input data objects.
--
-- /See:/ 'mkLabelingJobS3DataSource' smart constructor.
newtype LabelingJobS3DataSource = LabelingJobS3DataSource'
  { manifestS3URI ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobS3DataSource' with the minimum fields required to make a request.
--
-- * 'manifestS3URI' - The Amazon S3 location of the manifest file that describes the input data objects.
mkLabelingJobS3DataSource ::
  -- | 'manifestS3URI'
  Lude.Text ->
  LabelingJobS3DataSource
mkLabelingJobS3DataSource pManifestS3URI_ =
  LabelingJobS3DataSource' {manifestS3URI = pManifestS3URI_}

-- | The Amazon S3 location of the manifest file that describes the input data objects.
--
-- /Note:/ Consider using 'manifestS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsdsManifestS3URI :: Lens.Lens' LabelingJobS3DataSource Lude.Text
ljsdsManifestS3URI = Lens.lens (manifestS3URI :: LabelingJobS3DataSource -> Lude.Text) (\s a -> s {manifestS3URI = a} :: LabelingJobS3DataSource)
{-# DEPRECATED ljsdsManifestS3URI "Use generic-lens or generic-optics with 'manifestS3URI' instead." #-}

instance Lude.FromJSON LabelingJobS3DataSource where
  parseJSON =
    Lude.withObject
      "LabelingJobS3DataSource"
      ( \x ->
          LabelingJobS3DataSource' Lude.<$> (x Lude..: "ManifestS3Uri")
      )

instance Lude.ToJSON LabelingJobS3DataSource where
  toJSON LabelingJobS3DataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ManifestS3Uri" Lude..= manifestS3URI)]
      )
