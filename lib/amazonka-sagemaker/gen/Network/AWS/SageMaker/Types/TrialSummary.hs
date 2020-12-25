{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSummary
  ( TrialSummary (..),

    -- * Smart constructor
    mkTrialSummary,

    -- * Lenses
    tsCreationTime,
    tsDisplayName,
    tsLastModifiedTime,
    tsTrialArn,
    tsTrialName,
    tsTrialSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ExperimentEntityName as Types
import qualified Network.AWS.SageMaker.Types.TrialArn as Types
import qualified Network.AWS.SageMaker.Types.TrialSource as Types

-- | A summary of the properties of a trial. To get the complete set of properties, call the 'DescribeTrial' API and provide the @TrialName@ .
--
-- /See:/ 'mkTrialSummary' smart constructor.
data TrialSummary = TrialSummary'
  { -- | When the trial was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
    displayName :: Core.Maybe Types.ExperimentEntityName,
    -- | When the trial was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Types.TrialArn,
    -- | The name of the trial.
    trialName :: Core.Maybe Types.ExperimentEntityName,
    trialSource :: Core.Maybe Types.TrialSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TrialSummary' value with any optional fields omitted.
mkTrialSummary ::
  TrialSummary
mkTrialSummary =
  TrialSummary'
    { creationTime = Core.Nothing,
      displayName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      trialArn = Core.Nothing,
      trialName = Core.Nothing,
      trialSource = Core.Nothing
    }

-- | When the trial was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCreationTime :: Lens.Lens' TrialSummary (Core.Maybe Core.NominalDiffTime)
tsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDisplayName :: Lens.Lens' TrialSummary (Core.Maybe Types.ExperimentEntityName)
tsDisplayName = Lens.field @"displayName"
{-# DEPRECATED tsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | When the trial was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLastModifiedTime :: Lens.Lens' TrialSummary (Core.Maybe Core.NominalDiffTime)
tsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED tsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTrialArn :: Lens.Lens' TrialSummary (Core.Maybe Types.TrialArn)
tsTrialArn = Lens.field @"trialArn"
{-# DEPRECATED tsTrialArn "Use generic-lens or generic-optics with 'trialArn' instead." #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTrialName :: Lens.Lens' TrialSummary (Core.Maybe Types.ExperimentEntityName)
tsTrialName = Lens.field @"trialName"
{-# DEPRECATED tsTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trialSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTrialSource :: Lens.Lens' TrialSummary (Core.Maybe Types.TrialSource)
tsTrialSource = Lens.field @"trialSource"
{-# DEPRECATED tsTrialSource "Use generic-lens or generic-optics with 'trialSource' instead." #-}

instance Core.FromJSON TrialSummary where
  parseJSON =
    Core.withObject "TrialSummary" Core.$
      \x ->
        TrialSummary'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "TrialArn")
          Core.<*> (x Core..:? "TrialName")
          Core.<*> (x Core..:? "TrialSource")
