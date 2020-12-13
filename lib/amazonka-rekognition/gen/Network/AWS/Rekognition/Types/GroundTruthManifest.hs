{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.GroundTruthManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.GroundTruthManifest
  ( GroundTruthManifest (..),

    -- * Smart constructor
    mkGroundTruthManifest,

    -- * Lenses
    gtmS3Object,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.S3Object

-- | The S3 bucket that contains an Amazon Sagemaker Ground Truth format manifest file.
--
-- /See:/ 'mkGroundTruthManifest' smart constructor.
newtype GroundTruthManifest = GroundTruthManifest'
  { s3Object :: Lude.Maybe S3Object
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroundTruthManifest' with the minimum fields required to make a request.
--
-- * 's3Object' -
mkGroundTruthManifest ::
  GroundTruthManifest
mkGroundTruthManifest =
  GroundTruthManifest' {s3Object = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmS3Object :: Lens.Lens' GroundTruthManifest (Lude.Maybe S3Object)
gtmS3Object = Lens.lens (s3Object :: GroundTruthManifest -> Lude.Maybe S3Object) (\s a -> s {s3Object = a} :: GroundTruthManifest)
{-# DEPRECATED gtmS3Object "Use generic-lens or generic-optics with 's3Object' instead." #-}

instance Lude.FromJSON GroundTruthManifest where
  parseJSON =
    Lude.withObject
      "GroundTruthManifest"
      (\x -> GroundTruthManifest' Lude.<$> (x Lude..:? "S3Object"))

instance Lude.ToJSON GroundTruthManifest where
  toJSON GroundTruthManifest' {..} =
    Lude.object
      (Lude.catMaybes [("S3Object" Lude..=) Lude.<$> s3Object])
