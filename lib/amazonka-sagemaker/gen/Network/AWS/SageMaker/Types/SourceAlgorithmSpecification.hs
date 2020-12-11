-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
  ( SourceAlgorithmSpecification (..),

    -- * Smart constructor
    mkSourceAlgorithmSpecification,

    -- * Lenses
    sasSourceAlgorithms,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.SourceAlgorithm

-- | A list of algorithms that were used to create a model package.
--
-- /See:/ 'mkSourceAlgorithmSpecification' smart constructor.
newtype SourceAlgorithmSpecification = SourceAlgorithmSpecification'
  { sourceAlgorithms ::
      Lude.NonEmpty SourceAlgorithm
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceAlgorithmSpecification' with the minimum fields required to make a request.
--
-- * 'sourceAlgorithms' - A list of the algorithms that were used to create a model package.
mkSourceAlgorithmSpecification ::
  -- | 'sourceAlgorithms'
  Lude.NonEmpty SourceAlgorithm ->
  SourceAlgorithmSpecification
mkSourceAlgorithmSpecification pSourceAlgorithms_ =
  SourceAlgorithmSpecification'
    { sourceAlgorithms =
        pSourceAlgorithms_
    }

-- | A list of the algorithms that were used to create a model package.
--
-- /Note:/ Consider using 'sourceAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasSourceAlgorithms :: Lens.Lens' SourceAlgorithmSpecification (Lude.NonEmpty SourceAlgorithm)
sasSourceAlgorithms = Lens.lens (sourceAlgorithms :: SourceAlgorithmSpecification -> Lude.NonEmpty SourceAlgorithm) (\s a -> s {sourceAlgorithms = a} :: SourceAlgorithmSpecification)
{-# DEPRECATED sasSourceAlgorithms "Use generic-lens or generic-optics with 'sourceAlgorithms' instead." #-}

instance Lude.FromJSON SourceAlgorithmSpecification where
  parseJSON =
    Lude.withObject
      "SourceAlgorithmSpecification"
      ( \x ->
          SourceAlgorithmSpecification'
            Lude.<$> (x Lude..: "SourceAlgorithms")
      )

instance Lude.ToJSON SourceAlgorithmSpecification where
  toJSON SourceAlgorithmSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SourceAlgorithms" Lude..= sourceAlgorithms)]
      )
