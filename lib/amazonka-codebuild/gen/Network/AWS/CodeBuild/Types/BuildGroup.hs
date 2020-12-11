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
    bgIdentifier,
    bgDependsOn,
    bgIgnoreFailure,
    bgCurrentBuildSummary,
    bgPriorBuildSummaryList,
  )
where

import Network.AWS.CodeBuild.Types.BuildSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a batch build build group. Build groups are used to combine builds that can run in parallel, while still being able to set dependencies on other build groups.
--
-- /See:/ 'mkBuildGroup' smart constructor.
data BuildGroup = BuildGroup'
  { identifier :: Lude.Maybe Lude.Text,
    dependsOn :: Lude.Maybe [Lude.Text],
    ignoreFailure :: Lude.Maybe Lude.Bool,
    currentBuildSummary :: Lude.Maybe BuildSummary,
    priorBuildSummaryList :: Lude.Maybe [BuildSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildGroup' with the minimum fields required to make a request.
--
-- * 'currentBuildSummary' - A @BuildSummary@ object that contains a summary of the current build group.
-- * 'dependsOn' - An array of strings that contain the identifiers of the build groups that this build group depends on.
-- * 'identifier' - Contains the identifier of the build group.
-- * 'ignoreFailure' - Specifies if failures in this build group can be ignored.
-- * 'priorBuildSummaryList' - An array of @BuildSummary@ objects that contain summaries of previous build groups.
mkBuildGroup ::
  BuildGroup
mkBuildGroup =
  BuildGroup'
    { identifier = Lude.Nothing,
      dependsOn = Lude.Nothing,
      ignoreFailure = Lude.Nothing,
      currentBuildSummary = Lude.Nothing,
      priorBuildSummaryList = Lude.Nothing
    }

-- | Contains the identifier of the build group.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgIdentifier :: Lens.Lens' BuildGroup (Lude.Maybe Lude.Text)
bgIdentifier = Lens.lens (identifier :: BuildGroup -> Lude.Maybe Lude.Text) (\s a -> s {identifier = a} :: BuildGroup)
{-# DEPRECATED bgIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | An array of strings that contain the identifiers of the build groups that this build group depends on.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgDependsOn :: Lens.Lens' BuildGroup (Lude.Maybe [Lude.Text])
bgDependsOn = Lens.lens (dependsOn :: BuildGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {dependsOn = a} :: BuildGroup)
{-# DEPRECATED bgDependsOn "Use generic-lens or generic-optics with 'dependsOn' instead." #-}

-- | Specifies if failures in this build group can be ignored.
--
-- /Note:/ Consider using 'ignoreFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgIgnoreFailure :: Lens.Lens' BuildGroup (Lude.Maybe Lude.Bool)
bgIgnoreFailure = Lens.lens (ignoreFailure :: BuildGroup -> Lude.Maybe Lude.Bool) (\s a -> s {ignoreFailure = a} :: BuildGroup)
{-# DEPRECATED bgIgnoreFailure "Use generic-lens or generic-optics with 'ignoreFailure' instead." #-}

-- | A @BuildSummary@ object that contains a summary of the current build group.
--
-- /Note:/ Consider using 'currentBuildSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgCurrentBuildSummary :: Lens.Lens' BuildGroup (Lude.Maybe BuildSummary)
bgCurrentBuildSummary = Lens.lens (currentBuildSummary :: BuildGroup -> Lude.Maybe BuildSummary) (\s a -> s {currentBuildSummary = a} :: BuildGroup)
{-# DEPRECATED bgCurrentBuildSummary "Use generic-lens or generic-optics with 'currentBuildSummary' instead." #-}

-- | An array of @BuildSummary@ objects that contain summaries of previous build groups.
--
-- /Note:/ Consider using 'priorBuildSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgPriorBuildSummaryList :: Lens.Lens' BuildGroup (Lude.Maybe [BuildSummary])
bgPriorBuildSummaryList = Lens.lens (priorBuildSummaryList :: BuildGroup -> Lude.Maybe [BuildSummary]) (\s a -> s {priorBuildSummaryList = a} :: BuildGroup)
{-# DEPRECATED bgPriorBuildSummaryList "Use generic-lens or generic-optics with 'priorBuildSummaryList' instead." #-}

instance Lude.FromJSON BuildGroup where
  parseJSON =
    Lude.withObject
      "BuildGroup"
      ( \x ->
          BuildGroup'
            Lude.<$> (x Lude..:? "identifier")
            Lude.<*> (x Lude..:? "dependsOn" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ignoreFailure")
            Lude.<*> (x Lude..:? "currentBuildSummary")
            Lude.<*> (x Lude..:? "priorBuildSummaryList" Lude..!= Lude.mempty)
      )
