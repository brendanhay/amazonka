-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildSummary
  ( BuildSummary (..),

    -- * Smart constructor
    mkBuildSummary,

    -- * Lenses
    bsSecondaryArtifacts,
    bsPrimaryArtifact,
    bsArn,
    bsBuildStatus,
    bsRequestedOn,
  )
where

import Network.AWS.CodeBuild.Types.ResolvedArtifact
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a batch build group.
--
-- /See:/ 'mkBuildSummary' smart constructor.
data BuildSummary = BuildSummary'
  { secondaryArtifacts ::
      Lude.Maybe [ResolvedArtifact],
    primaryArtifact :: Lude.Maybe ResolvedArtifact,
    arn :: Lude.Maybe Lude.Text,
    buildStatus :: Lude.Maybe StatusType,
    requestedOn :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The batch build ARN.
-- * 'buildStatus' - The status of the build group.
--
--
--     * FAILED
--
--     * The build group failed.
--
--
--     * FAULT
--
--     * The build group faulted.
--
--
--     * IN_PROGRESS
--
--     * The build group is still in progress.
--
--
--     * STOPPED
--
--     * The build group stopped.
--
--
--     * SUCCEEDED
--
--     * The build group succeeded.
--
--
--     * TIMED_OUT
--
--     * The build group timed out.
--
--
-- * 'primaryArtifact' - A @ResolvedArtifact@ object that represents the primary build artifacts for the build group.
-- * 'requestedOn' - When the build was started, expressed in Unix time format.
-- * 'secondaryArtifacts' - An array of @ResolvedArtifact@ objects that represents the secondary build artifacts for the build group.
mkBuildSummary ::
  BuildSummary
mkBuildSummary =
  BuildSummary'
    { secondaryArtifacts = Lude.Nothing,
      primaryArtifact = Lude.Nothing,
      arn = Lude.Nothing,
      buildStatus = Lude.Nothing,
      requestedOn = Lude.Nothing
    }

-- | An array of @ResolvedArtifact@ objects that represents the secondary build artifacts for the build group.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsSecondaryArtifacts :: Lens.Lens' BuildSummary (Lude.Maybe [ResolvedArtifact])
bsSecondaryArtifacts = Lens.lens (secondaryArtifacts :: BuildSummary -> Lude.Maybe [ResolvedArtifact]) (\s a -> s {secondaryArtifacts = a} :: BuildSummary)
{-# DEPRECATED bsSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | A @ResolvedArtifact@ object that represents the primary build artifacts for the build group.
--
-- /Note:/ Consider using 'primaryArtifact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsPrimaryArtifact :: Lens.Lens' BuildSummary (Lude.Maybe ResolvedArtifact)
bsPrimaryArtifact = Lens.lens (primaryArtifact :: BuildSummary -> Lude.Maybe ResolvedArtifact) (\s a -> s {primaryArtifact = a} :: BuildSummary)
{-# DEPRECATED bsPrimaryArtifact "Use generic-lens or generic-optics with 'primaryArtifact' instead." #-}

-- | The batch build ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsArn :: Lens.Lens' BuildSummary (Lude.Maybe Lude.Text)
bsArn = Lens.lens (arn :: BuildSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: BuildSummary)
{-# DEPRECATED bsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The status of the build group.
--
--
--     * FAILED
--
--     * The build group failed.
--
--
--     * FAULT
--
--     * The build group faulted.
--
--
--     * IN_PROGRESS
--
--     * The build group is still in progress.
--
--
--     * STOPPED
--
--     * The build group stopped.
--
--
--     * SUCCEEDED
--
--     * The build group succeeded.
--
--
--     * TIMED_OUT
--
--     * The build group timed out.
--
--
--
-- /Note:/ Consider using 'buildStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBuildStatus :: Lens.Lens' BuildSummary (Lude.Maybe StatusType)
bsBuildStatus = Lens.lens (buildStatus :: BuildSummary -> Lude.Maybe StatusType) (\s a -> s {buildStatus = a} :: BuildSummary)
{-# DEPRECATED bsBuildStatus "Use generic-lens or generic-optics with 'buildStatus' instead." #-}

-- | When the build was started, expressed in Unix time format.
--
-- /Note:/ Consider using 'requestedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsRequestedOn :: Lens.Lens' BuildSummary (Lude.Maybe Lude.Timestamp)
bsRequestedOn = Lens.lens (requestedOn :: BuildSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {requestedOn = a} :: BuildSummary)
{-# DEPRECATED bsRequestedOn "Use generic-lens or generic-optics with 'requestedOn' instead." #-}

instance Lude.FromJSON BuildSummary where
  parseJSON =
    Lude.withObject
      "BuildSummary"
      ( \x ->
          BuildSummary'
            Lude.<$> (x Lude..:? "secondaryArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "primaryArtifact")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "buildStatus")
            Lude.<*> (x Lude..:? "requestedOn")
      )
