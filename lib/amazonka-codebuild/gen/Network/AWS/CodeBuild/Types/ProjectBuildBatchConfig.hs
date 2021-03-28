{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
  ( ProjectBuildBatchConfig (..)
  -- * Smart constructor
  , mkProjectBuildBatchConfig
  -- * Lenses
  , pbbcCombineArtifacts
  , pbbcRestrictions
  , pbbcServiceRole
  , pbbcTimeoutInMins
  ) where

import qualified Network.AWS.CodeBuild.Types.BatchRestrictions as Types
import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains configuration information about a batch build project.
--
-- /See:/ 'mkProjectBuildBatchConfig' smart constructor.
data ProjectBuildBatchConfig = ProjectBuildBatchConfig'
  { combineArtifacts :: Core.Maybe Core.Bool
    -- ^ Specifies if the build artifacts for the batch build should be combined into a single artifact location.
  , restrictions :: Core.Maybe Types.BatchRestrictions
    -- ^ A @BatchRestrictions@ object that specifies the restrictions for the batch build.
  , serviceRole :: Core.Maybe Types.NonEmptyString
    -- ^ Specifies the service role ARN for the batch build project.
  , timeoutInMins :: Core.Maybe Core.Int
    -- ^ Specifies the maximum amount of time, in minutes, that the batch build must be completed in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectBuildBatchConfig' value with any optional fields omitted.
mkProjectBuildBatchConfig
    :: ProjectBuildBatchConfig
mkProjectBuildBatchConfig
  = ProjectBuildBatchConfig'{combineArtifacts = Core.Nothing,
                             restrictions = Core.Nothing, serviceRole = Core.Nothing,
                             timeoutInMins = Core.Nothing}

-- | Specifies if the build artifacts for the batch build should be combined into a single artifact location.
--
-- /Note:/ Consider using 'combineArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcCombineArtifacts :: Lens.Lens' ProjectBuildBatchConfig (Core.Maybe Core.Bool)
pbbcCombineArtifacts = Lens.field @"combineArtifacts"
{-# INLINEABLE pbbcCombineArtifacts #-}
{-# DEPRECATED combineArtifacts "Use generic-lens or generic-optics with 'combineArtifacts' instead"  #-}

-- | A @BatchRestrictions@ object that specifies the restrictions for the batch build.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcRestrictions :: Lens.Lens' ProjectBuildBatchConfig (Core.Maybe Types.BatchRestrictions)
pbbcRestrictions = Lens.field @"restrictions"
{-# INLINEABLE pbbcRestrictions #-}
{-# DEPRECATED restrictions "Use generic-lens or generic-optics with 'restrictions' instead"  #-}

-- | Specifies the service role ARN for the batch build project.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcServiceRole :: Lens.Lens' ProjectBuildBatchConfig (Core.Maybe Types.NonEmptyString)
pbbcServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE pbbcServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | Specifies the maximum amount of time, in minutes, that the batch build must be completed in.
--
-- /Note:/ Consider using 'timeoutInMins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcTimeoutInMins :: Lens.Lens' ProjectBuildBatchConfig (Core.Maybe Core.Int)
pbbcTimeoutInMins = Lens.field @"timeoutInMins"
{-# INLINEABLE pbbcTimeoutInMins #-}
{-# DEPRECATED timeoutInMins "Use generic-lens or generic-optics with 'timeoutInMins' instead"  #-}

instance Core.FromJSON ProjectBuildBatchConfig where
        toJSON ProjectBuildBatchConfig{..}
          = Core.object
              (Core.catMaybes
                 [("combineArtifacts" Core..=) Core.<$> combineArtifacts,
                  ("restrictions" Core..=) Core.<$> restrictions,
                  ("serviceRole" Core..=) Core.<$> serviceRole,
                  ("timeoutInMins" Core..=) Core.<$> timeoutInMins])

instance Core.FromJSON ProjectBuildBatchConfig where
        parseJSON
          = Core.withObject "ProjectBuildBatchConfig" Core.$
              \ x ->
                ProjectBuildBatchConfig' Core.<$>
                  (x Core..:? "combineArtifacts") Core.<*> x Core..:? "restrictions"
                    Core.<*> x Core..:? "serviceRole"
                    Core.<*> x Core..:? "timeoutInMins"
