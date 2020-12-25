{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildGroup
  ( BuildGroup (..),

    -- * Smart constructor
    mkBuildGroup,

    -- * Lenses
    bgCurrentBuildSummary,
    bgDependsOn,
    bgIdentifier,
    bgIgnoreFailure,
    bgPriorBuildSummaryList,
  )
where

import qualified Network.AWS.CodeBuild.Types.BuildSummary as Types
import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a batch build build group. Build groups are used to combine builds that can run in parallel, while still being able to set dependencies on other build groups.
--
-- /See:/ 'mkBuildGroup' smart constructor.
data BuildGroup = BuildGroup'
  { -- | A @BuildSummary@ object that contains a summary of the current build group.
    currentBuildSummary :: Core.Maybe Types.BuildSummary,
    -- | An array of strings that contain the identifiers of the build groups that this build group depends on.
    dependsOn :: Core.Maybe [Types.NonEmptyString],
    -- | Contains the identifier of the build group.
    identifier :: Core.Maybe Types.String,
    -- | Specifies if failures in this build group can be ignored.
    ignoreFailure :: Core.Maybe Core.Bool,
    -- | An array of @BuildSummary@ objects that contain summaries of previous build groups.
    priorBuildSummaryList :: Core.Maybe [Types.BuildSummary]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BuildGroup' value with any optional fields omitted.
mkBuildGroup ::
  BuildGroup
mkBuildGroup =
  BuildGroup'
    { currentBuildSummary = Core.Nothing,
      dependsOn = Core.Nothing,
      identifier = Core.Nothing,
      ignoreFailure = Core.Nothing,
      priorBuildSummaryList = Core.Nothing
    }

-- | A @BuildSummary@ object that contains a summary of the current build group.
--
-- /Note:/ Consider using 'currentBuildSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgCurrentBuildSummary :: Lens.Lens' BuildGroup (Core.Maybe Types.BuildSummary)
bgCurrentBuildSummary = Lens.field @"currentBuildSummary"
{-# DEPRECATED bgCurrentBuildSummary "Use generic-lens or generic-optics with 'currentBuildSummary' instead." #-}

-- | An array of strings that contain the identifiers of the build groups that this build group depends on.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgDependsOn :: Lens.Lens' BuildGroup (Core.Maybe [Types.NonEmptyString])
bgDependsOn = Lens.field @"dependsOn"
{-# DEPRECATED bgDependsOn "Use generic-lens or generic-optics with 'dependsOn' instead." #-}

-- | Contains the identifier of the build group.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgIdentifier :: Lens.Lens' BuildGroup (Core.Maybe Types.String)
bgIdentifier = Lens.field @"identifier"
{-# DEPRECATED bgIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | Specifies if failures in this build group can be ignored.
--
-- /Note:/ Consider using 'ignoreFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgIgnoreFailure :: Lens.Lens' BuildGroup (Core.Maybe Core.Bool)
bgIgnoreFailure = Lens.field @"ignoreFailure"
{-# DEPRECATED bgIgnoreFailure "Use generic-lens or generic-optics with 'ignoreFailure' instead." #-}

-- | An array of @BuildSummary@ objects that contain summaries of previous build groups.
--
-- /Note:/ Consider using 'priorBuildSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgPriorBuildSummaryList :: Lens.Lens' BuildGroup (Core.Maybe [Types.BuildSummary])
bgPriorBuildSummaryList = Lens.field @"priorBuildSummaryList"
{-# DEPRECATED bgPriorBuildSummaryList "Use generic-lens or generic-optics with 'priorBuildSummaryList' instead." #-}

instance Core.FromJSON BuildGroup where
  parseJSON =
    Core.withObject "BuildGroup" Core.$
      \x ->
        BuildGroup'
          Core.<$> (x Core..:? "currentBuildSummary")
          Core.<*> (x Core..:? "dependsOn")
          Core.<*> (x Core..:? "identifier")
          Core.<*> (x Core..:? "ignoreFailure")
          Core.<*> (x Core..:? "priorBuildSummaryList")
