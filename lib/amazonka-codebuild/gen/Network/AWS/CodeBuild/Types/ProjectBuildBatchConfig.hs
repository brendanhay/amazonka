-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
  ( ProjectBuildBatchConfig (..),

    -- * Smart constructor
    mkProjectBuildBatchConfig,

    -- * Lenses
    pbbcCombineArtifacts,
    pbbcTimeoutInMins,
    pbbcRestrictions,
    pbbcServiceRole,
  )
where

import Network.AWS.CodeBuild.Types.BatchRestrictions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains configuration information about a batch build project.
--
-- /See:/ 'mkProjectBuildBatchConfig' smart constructor.
data ProjectBuildBatchConfig = ProjectBuildBatchConfig'
  { combineArtifacts ::
      Lude.Maybe Lude.Bool,
    timeoutInMins :: Lude.Maybe Lude.Int,
    restrictions ::
      Lude.Maybe BatchRestrictions,
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectBuildBatchConfig' with the minimum fields required to make a request.
--
-- * 'combineArtifacts' - Specifies if the build artifacts for the batch build should be combined into a single artifact location.
-- * 'restrictions' - A @BatchRestrictions@ object that specifies the restrictions for the batch build.
-- * 'serviceRole' - Specifies the service role ARN for the batch build project.
-- * 'timeoutInMins' - Specifies the maximum amount of time, in minutes, that the batch build must be completed in.
mkProjectBuildBatchConfig ::
  ProjectBuildBatchConfig
mkProjectBuildBatchConfig =
  ProjectBuildBatchConfig'
    { combineArtifacts = Lude.Nothing,
      timeoutInMins = Lude.Nothing,
      restrictions = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | Specifies if the build artifacts for the batch build should be combined into a single artifact location.
--
-- /Note:/ Consider using 'combineArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcCombineArtifacts :: Lens.Lens' ProjectBuildBatchConfig (Lude.Maybe Lude.Bool)
pbbcCombineArtifacts = Lens.lens (combineArtifacts :: ProjectBuildBatchConfig -> Lude.Maybe Lude.Bool) (\s a -> s {combineArtifacts = a} :: ProjectBuildBatchConfig)
{-# DEPRECATED pbbcCombineArtifacts "Use generic-lens or generic-optics with 'combineArtifacts' instead." #-}

-- | Specifies the maximum amount of time, in minutes, that the batch build must be completed in.
--
-- /Note:/ Consider using 'timeoutInMins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcTimeoutInMins :: Lens.Lens' ProjectBuildBatchConfig (Lude.Maybe Lude.Int)
pbbcTimeoutInMins = Lens.lens (timeoutInMins :: ProjectBuildBatchConfig -> Lude.Maybe Lude.Int) (\s a -> s {timeoutInMins = a} :: ProjectBuildBatchConfig)
{-# DEPRECATED pbbcTimeoutInMins "Use generic-lens or generic-optics with 'timeoutInMins' instead." #-}

-- | A @BatchRestrictions@ object that specifies the restrictions for the batch build.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcRestrictions :: Lens.Lens' ProjectBuildBatchConfig (Lude.Maybe BatchRestrictions)
pbbcRestrictions = Lens.lens (restrictions :: ProjectBuildBatchConfig -> Lude.Maybe BatchRestrictions) (\s a -> s {restrictions = a} :: ProjectBuildBatchConfig)
{-# DEPRECATED pbbcRestrictions "Use generic-lens or generic-optics with 'restrictions' instead." #-}

-- | Specifies the service role ARN for the batch build project.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbbcServiceRole :: Lens.Lens' ProjectBuildBatchConfig (Lude.Maybe Lude.Text)
pbbcServiceRole = Lens.lens (serviceRole :: ProjectBuildBatchConfig -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: ProjectBuildBatchConfig)
{-# DEPRECATED pbbcServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON ProjectBuildBatchConfig where
  parseJSON =
    Lude.withObject
      "ProjectBuildBatchConfig"
      ( \x ->
          ProjectBuildBatchConfig'
            Lude.<$> (x Lude..:? "combineArtifacts")
            Lude.<*> (x Lude..:? "timeoutInMins")
            Lude.<*> (x Lude..:? "restrictions")
            Lude.<*> (x Lude..:? "serviceRole")
      )

instance Lude.ToJSON ProjectBuildBatchConfig where
  toJSON ProjectBuildBatchConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("combineArtifacts" Lude..=) Lude.<$> combineArtifacts,
            ("timeoutInMins" Lude..=) Lude.<$> timeoutInMins,
            ("restrictions" Lude..=) Lude.<$> restrictions,
            ("serviceRole" Lude..=) Lude.<$> serviceRole
          ]
      )
