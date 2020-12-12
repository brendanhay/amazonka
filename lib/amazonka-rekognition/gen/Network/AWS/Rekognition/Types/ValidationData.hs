{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ValidationData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ValidationData
  ( ValidationData (..),

    -- * Smart constructor
    mkValidationData,

    -- * Lenses
    vdAssets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Asset

-- | Contains the Amazon S3 bucket location of the validation data for a model training job.
--
-- The validation data includes error information for individual JSON lines in the dataset. For more information, see Debugging a Failed Model Training in the Amazon Rekognition Custom Labels Developer Guide.
-- You get the @ValidationData@ object for the training dataset ('TrainingDataResult' ) and the test dataset ('TestingDataResult' ) by calling 'DescribeProjectVersions' .
-- The assets array contains a single 'Asset' object. The 'GroundTruthManifest' field of the Asset object contains the S3 bucket location of the validation data.
--
-- /See:/ 'mkValidationData' smart constructor.
newtype ValidationData = ValidationData'
  { assets ::
      Lude.Maybe [Asset]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationData' with the minimum fields required to make a request.
--
-- * 'assets' - The assets that comprise the validation data.
mkValidationData ::
  ValidationData
mkValidationData = ValidationData' {assets = Lude.Nothing}

-- | The assets that comprise the validation data.
--
-- /Note:/ Consider using 'assets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdAssets :: Lens.Lens' ValidationData (Lude.Maybe [Asset])
vdAssets = Lens.lens (assets :: ValidationData -> Lude.Maybe [Asset]) (\s a -> s {assets = a} :: ValidationData)
{-# DEPRECATED vdAssets "Use generic-lens or generic-optics with 'assets' instead." #-}

instance Lude.FromJSON ValidationData where
  parseJSON =
    Lude.withObject
      "ValidationData"
      ( \x ->
          ValidationData'
            Lude.<$> (x Lude..:? "Assets" Lude..!= Lude.mempty)
      )
