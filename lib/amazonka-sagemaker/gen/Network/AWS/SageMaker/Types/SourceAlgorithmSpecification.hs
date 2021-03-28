{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
  ( SourceAlgorithmSpecification (..)
  -- * Smart constructor
  , mkSourceAlgorithmSpecification
  -- * Lenses
  , sasSourceAlgorithms
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SourceAlgorithm as Types

-- | A list of algorithms that were used to create a model package.
--
-- /See:/ 'mkSourceAlgorithmSpecification' smart constructor.
newtype SourceAlgorithmSpecification = SourceAlgorithmSpecification'
  { sourceAlgorithms :: Core.NonEmpty Types.SourceAlgorithm
    -- ^ A list of the algorithms that were used to create a model package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SourceAlgorithmSpecification' value with any optional fields omitted.
mkSourceAlgorithmSpecification
    :: Core.NonEmpty Types.SourceAlgorithm -- ^ 'sourceAlgorithms'
    -> SourceAlgorithmSpecification
mkSourceAlgorithmSpecification sourceAlgorithms
  = SourceAlgorithmSpecification'{sourceAlgorithms}

-- | A list of the algorithms that were used to create a model package.
--
-- /Note:/ Consider using 'sourceAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasSourceAlgorithms :: Lens.Lens' SourceAlgorithmSpecification (Core.NonEmpty Types.SourceAlgorithm)
sasSourceAlgorithms = Lens.field @"sourceAlgorithms"
{-# INLINEABLE sasSourceAlgorithms #-}
{-# DEPRECATED sourceAlgorithms "Use generic-lens or generic-optics with 'sourceAlgorithms' instead"  #-}

instance Core.FromJSON SourceAlgorithmSpecification where
        toJSON SourceAlgorithmSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SourceAlgorithms" Core..= sourceAlgorithms)])

instance Core.FromJSON SourceAlgorithmSpecification where
        parseJSON
          = Core.withObject "SourceAlgorithmSpecification" Core.$
              \ x ->
                SourceAlgorithmSpecification' Core.<$>
                  (x Core..: "SourceAlgorithms")
