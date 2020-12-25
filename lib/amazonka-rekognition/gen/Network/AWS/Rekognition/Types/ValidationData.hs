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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Asset as Types

-- | Contains the Amazon S3 bucket location of the validation data for a model training job.
--
-- The validation data includes error information for individual JSON lines in the dataset. For more information, see Debugging a Failed Model Training in the Amazon Rekognition Custom Labels Developer Guide.
-- You get the @ValidationData@ object for the training dataset ('TrainingDataResult' ) and the test dataset ('TestingDataResult' ) by calling 'DescribeProjectVersions' .
-- The assets array contains a single 'Asset' object. The 'GroundTruthManifest' field of the Asset object contains the S3 bucket location of the validation data.
--
-- /See:/ 'mkValidationData' smart constructor.
newtype ValidationData = ValidationData'
  { -- | The assets that comprise the validation data.
    assets :: Core.Maybe [Types.Asset]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationData' value with any optional fields omitted.
mkValidationData ::
  ValidationData
mkValidationData = ValidationData' {assets = Core.Nothing}

-- | The assets that comprise the validation data.
--
-- /Note:/ Consider using 'assets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdAssets :: Lens.Lens' ValidationData (Core.Maybe [Types.Asset])
vdAssets = Lens.field @"assets"
{-# DEPRECATED vdAssets "Use generic-lens or generic-optics with 'assets' instead." #-}

instance Core.FromJSON ValidationData where
  parseJSON =
    Core.withObject "ValidationData" Core.$
      \x -> ValidationData' Core.<$> (x Core..:? "Assets")
