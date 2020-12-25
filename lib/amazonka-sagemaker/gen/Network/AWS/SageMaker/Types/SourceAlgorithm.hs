{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithm
  ( SourceAlgorithm (..),

    -- * Smart constructor
    mkSourceAlgorithm,

    -- * Lenses
    saAlgorithmName,
    saModelDataUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ArnOrName as Types
import qualified Network.AWS.SageMaker.Types.Url as Types

-- | Specifies an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
--
-- /See:/ 'mkSourceAlgorithm' smart constructor.
data SourceAlgorithm = SourceAlgorithm'
  { -- | The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
    algorithmName :: Types.ArnOrName,
    -- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
    modelDataUrl :: Core.Maybe Types.Url
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceAlgorithm' value with any optional fields omitted.
mkSourceAlgorithm ::
  -- | 'algorithmName'
  Types.ArnOrName ->
  SourceAlgorithm
mkSourceAlgorithm algorithmName =
  SourceAlgorithm' {algorithmName, modelDataUrl = Core.Nothing}

-- | The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saAlgorithmName :: Lens.Lens' SourceAlgorithm Types.ArnOrName
saAlgorithmName = Lens.field @"algorithmName"
{-# DEPRECATED saAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- /Note:/ Consider using 'modelDataUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saModelDataUrl :: Lens.Lens' SourceAlgorithm (Core.Maybe Types.Url)
saModelDataUrl = Lens.field @"modelDataUrl"
{-# DEPRECATED saModelDataUrl "Use generic-lens or generic-optics with 'modelDataUrl' instead." #-}

instance Core.FromJSON SourceAlgorithm where
  toJSON SourceAlgorithm {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AlgorithmName" Core..= algorithmName),
            ("ModelDataUrl" Core..=) Core.<$> modelDataUrl
          ]
      )

instance Core.FromJSON SourceAlgorithm where
  parseJSON =
    Core.withObject "SourceAlgorithm" Core.$
      \x ->
        SourceAlgorithm'
          Core.<$> (x Core..: "AlgorithmName") Core.<*> (x Core..:? "ModelDataUrl")
