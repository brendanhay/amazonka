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
    saModelDataURL,
    saAlgorithmName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
--
-- /See:/ 'mkSourceAlgorithm' smart constructor.
data SourceAlgorithm = SourceAlgorithm'
  { modelDataURL ::
      Lude.Maybe Lude.Text,
    algorithmName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceAlgorithm' with the minimum fields required to make a request.
--
-- * 'algorithmName' - The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
-- * 'modelDataURL' - The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
mkSourceAlgorithm ::
  -- | 'algorithmName'
  Lude.Text ->
  SourceAlgorithm
mkSourceAlgorithm pAlgorithmName_ =
  SourceAlgorithm'
    { modelDataURL = Lude.Nothing,
      algorithmName = pAlgorithmName_
    }

-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- /Note:/ Consider using 'modelDataURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saModelDataURL :: Lens.Lens' SourceAlgorithm (Lude.Maybe Lude.Text)
saModelDataURL = Lens.lens (modelDataURL :: SourceAlgorithm -> Lude.Maybe Lude.Text) (\s a -> s {modelDataURL = a} :: SourceAlgorithm)
{-# DEPRECATED saModelDataURL "Use generic-lens or generic-optics with 'modelDataURL' instead." #-}

-- | The name of an algorithm that was used to create the model package. The algorithm must be either an algorithm resource in your Amazon SageMaker account or an algorithm in AWS Marketplace that you are subscribed to.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saAlgorithmName :: Lens.Lens' SourceAlgorithm Lude.Text
saAlgorithmName = Lens.lens (algorithmName :: SourceAlgorithm -> Lude.Text) (\s a -> s {algorithmName = a} :: SourceAlgorithm)
{-# DEPRECATED saAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

instance Lude.FromJSON SourceAlgorithm where
  parseJSON =
    Lude.withObject
      "SourceAlgorithm"
      ( \x ->
          SourceAlgorithm'
            Lude.<$> (x Lude..:? "ModelDataUrl") Lude.<*> (x Lude..: "AlgorithmName")
      )

instance Lude.ToJSON SourceAlgorithm where
  toJSON SourceAlgorithm' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ModelDataUrl" Lude..=) Lude.<$> modelDataURL,
            Lude.Just ("AlgorithmName" Lude..= algorithmName)
          ]
      )
