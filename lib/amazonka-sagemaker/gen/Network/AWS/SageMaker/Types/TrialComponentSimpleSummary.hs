{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
  ( TrialComponentSimpleSummary (..),

    -- * Smart constructor
    mkTrialComponentSimpleSummary,

    -- * Lenses
    tcssCreatedBy,
    tcssCreationTime,
    tcssTrialComponentArn,
    tcssTrialComponentName,
    tcssTrialComponentSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TrialComponentArn as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentName as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentSource as Types
import qualified Network.AWS.SageMaker.Types.UserContext as Types

-- | A short summary of a trial component.
--
-- /See:/ 'mkTrialComponentSimpleSummary' smart constructor.
data TrialComponentSimpleSummary = TrialComponentSimpleSummary'
  { createdBy :: Core.Maybe Types.UserContext,
    -- | When the component was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The name of the trial component.
    trialComponentName :: Core.Maybe Types.TrialComponentName,
    trialComponentSource :: Core.Maybe Types.TrialComponentSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TrialComponentSimpleSummary' value with any optional fields omitted.
mkTrialComponentSimpleSummary ::
  TrialComponentSimpleSummary
mkTrialComponentSimpleSummary =
  TrialComponentSimpleSummary'
    { createdBy = Core.Nothing,
      creationTime = Core.Nothing,
      trialComponentArn = Core.Nothing,
      trialComponentName = Core.Nothing,
      trialComponentSource = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssCreatedBy :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Types.UserContext)
tcssCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED tcssCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssCreationTime :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Core.NominalDiffTime)
tcssCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tcssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssTrialComponentArn :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Types.TrialComponentArn)
tcssTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED tcssTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssTrialComponentName :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Types.TrialComponentName)
tcssTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED tcssTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trialComponentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcssTrialComponentSource :: Lens.Lens' TrialComponentSimpleSummary (Core.Maybe Types.TrialComponentSource)
tcssTrialComponentSource = Lens.field @"trialComponentSource"
{-# DEPRECATED tcssTrialComponentSource "Use generic-lens or generic-optics with 'trialComponentSource' instead." #-}

instance Core.FromJSON TrialComponentSimpleSummary where
  parseJSON =
    Core.withObject "TrialComponentSimpleSummary" Core.$
      \x ->
        TrialComponentSimpleSummary'
          Core.<$> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "TrialComponentArn")
          Core.<*> (x Core..:? "TrialComponentName")
          Core.<*> (x Core..:? "TrialComponentSource")
