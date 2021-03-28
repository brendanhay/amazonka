{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BatchRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BatchRestrictions
  ( BatchRestrictions (..)
  -- * Smart constructor
  , mkBatchRestrictions
  -- * Lenses
  , brComputeTypesAllowed
  , brMaximumBuildsAllowed
  ) where

import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies restrictions for the batch build.
--
-- /See:/ 'mkBatchRestrictions' smart constructor.
data BatchRestrictions = BatchRestrictions'
  { computeTypesAllowed :: Core.Maybe [Types.NonEmptyString]
    -- ^ An array of strings that specify the compute types that are allowed for the batch build. See <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types> in the /AWS CodeBuild User Guide/ for these values. 
  , maximumBuildsAllowed :: Core.Maybe Core.Int
    -- ^ Specifies the maximum number of builds allowed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchRestrictions' value with any optional fields omitted.
mkBatchRestrictions
    :: BatchRestrictions
mkBatchRestrictions
  = BatchRestrictions'{computeTypesAllowed = Core.Nothing,
                       maximumBuildsAllowed = Core.Nothing}

-- | An array of strings that specify the compute types that are allowed for the batch build. See <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build environment compute types> in the /AWS CodeBuild User Guide/ for these values. 
--
-- /Note:/ Consider using 'computeTypesAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brComputeTypesAllowed :: Lens.Lens' BatchRestrictions (Core.Maybe [Types.NonEmptyString])
brComputeTypesAllowed = Lens.field @"computeTypesAllowed"
{-# INLINEABLE brComputeTypesAllowed #-}
{-# DEPRECATED computeTypesAllowed "Use generic-lens or generic-optics with 'computeTypesAllowed' instead"  #-}

-- | Specifies the maximum number of builds allowed.
--
-- /Note:/ Consider using 'maximumBuildsAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brMaximumBuildsAllowed :: Lens.Lens' BatchRestrictions (Core.Maybe Core.Int)
brMaximumBuildsAllowed = Lens.field @"maximumBuildsAllowed"
{-# INLINEABLE brMaximumBuildsAllowed #-}
{-# DEPRECATED maximumBuildsAllowed "Use generic-lens or generic-optics with 'maximumBuildsAllowed' instead"  #-}

instance Core.FromJSON BatchRestrictions where
        toJSON BatchRestrictions{..}
          = Core.object
              (Core.catMaybes
                 [("computeTypesAllowed" Core..=) Core.<$> computeTypesAllowed,
                  ("maximumBuildsAllowed" Core..=) Core.<$> maximumBuildsAllowed])

instance Core.FromJSON BatchRestrictions where
        parseJSON
          = Core.withObject "BatchRestrictions" Core.$
              \ x ->
                BatchRestrictions' Core.<$>
                  (x Core..:? "computeTypesAllowed") Core.<*>
                    x Core..:? "maximumBuildsAllowed"
