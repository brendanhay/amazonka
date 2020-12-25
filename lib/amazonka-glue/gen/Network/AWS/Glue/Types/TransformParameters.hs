{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformParameters
  ( TransformParameters (..),

    -- * Smart constructor
    mkTransformParameters,

    -- * Lenses
    tpTransformType,
    tpFindMatchesParameters,
  )
where

import qualified Network.AWS.Glue.Types.FindMatchesParameters as Types
import qualified Network.AWS.Glue.Types.TransformType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The algorithm-specific parameters that are associated with the machine learning transform.
--
-- /See:/ 'mkTransformParameters' smart constructor.
data TransformParameters = TransformParameters'
  { -- | The type of machine learning transform.
    --
    -- For information about the types of machine learning transforms, see <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms> .
    transformType :: Types.TransformType,
    -- | The parameters for the find matches algorithm.
    findMatchesParameters :: Core.Maybe Types.FindMatchesParameters
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformParameters' value with any optional fields omitted.
mkTransformParameters ::
  -- | 'transformType'
  Types.TransformType ->
  TransformParameters
mkTransformParameters transformType =
  TransformParameters'
    { transformType,
      findMatchesParameters = Core.Nothing
    }

-- | The type of machine learning transform.
--
-- For information about the types of machine learning transforms, see <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms> .
--
-- /Note:/ Consider using 'transformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpTransformType :: Lens.Lens' TransformParameters Types.TransformType
tpTransformType = Lens.field @"transformType"
{-# DEPRECATED tpTransformType "Use generic-lens or generic-optics with 'transformType' instead." #-}

-- | The parameters for the find matches algorithm.
--
-- /Note:/ Consider using 'findMatchesParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpFindMatchesParameters :: Lens.Lens' TransformParameters (Core.Maybe Types.FindMatchesParameters)
tpFindMatchesParameters = Lens.field @"findMatchesParameters"
{-# DEPRECATED tpFindMatchesParameters "Use generic-lens or generic-optics with 'findMatchesParameters' instead." #-}

instance Core.FromJSON TransformParameters where
  toJSON TransformParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformType" Core..= transformType),
            ("FindMatchesParameters" Core..=) Core.<$> findMatchesParameters
          ]
      )

instance Core.FromJSON TransformParameters where
  parseJSON =
    Core.withObject "TransformParameters" Core.$
      \x ->
        TransformParameters'
          Core.<$> (x Core..: "TransformType")
          Core.<*> (x Core..:? "FindMatchesParameters")
