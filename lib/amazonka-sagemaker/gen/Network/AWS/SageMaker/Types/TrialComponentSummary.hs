{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentSummary
  ( TrialComponentSummary (..)
  -- * Smart constructor
  , mkTrialComponentSummary
  -- * Lenses
  , tcsCreatedBy
  , tcsCreationTime
  , tcsDisplayName
  , tcsEndTime
  , tcsLastModifiedBy
  , tcsLastModifiedTime
  , tcsStartTime
  , tcsStatus
  , tcsTrialComponentArn
  , tcsTrialComponentName
  , tcsTrialComponentSource
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ExperimentEntityName as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentArn as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentSource as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentStatus as Types
import qualified Network.AWS.SageMaker.Types.UserContext as Types

-- | A summary of the properties of a trial component. To get all the properties, call the 'DescribeTrialComponent' API and provide the @TrialComponentName@ .
--
-- /See:/ 'mkTrialComponentSummary' smart constructor.
data TrialComponentSummary = TrialComponentSummary'
  { createdBy :: Core.Maybe Types.UserContext
    -- ^ Who created the component.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component was created.
  , displayName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component ended.
  , lastModifiedBy :: Core.Maybe Types.UserContext
    -- ^ Who last modified the component.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component was last modified.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component started.
  , status :: Core.Maybe Types.TrialComponentStatus
    -- ^ The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
  , trialComponentArn :: Core.Maybe Types.TrialComponentArn
    -- ^ The ARN of the trial component.
  , trialComponentName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the trial component.
  , trialComponentSource :: Core.Maybe Types.TrialComponentSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TrialComponentSummary' value with any optional fields omitted.
mkTrialComponentSummary
    :: TrialComponentSummary
mkTrialComponentSummary
  = TrialComponentSummary'{createdBy = Core.Nothing,
                           creationTime = Core.Nothing, displayName = Core.Nothing,
                           endTime = Core.Nothing, lastModifiedBy = Core.Nothing,
                           lastModifiedTime = Core.Nothing, startTime = Core.Nothing,
                           status = Core.Nothing, trialComponentArn = Core.Nothing,
                           trialComponentName = Core.Nothing,
                           trialComponentSource = Core.Nothing}

-- | Who created the component.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsCreatedBy :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.UserContext)
tcsCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE tcsCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsCreationTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.NominalDiffTime)
tcsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tcsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsDisplayName :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.ExperimentEntityName)
tcsDisplayName = Lens.field @"displayName"
{-# INLINEABLE tcsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsEndTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.NominalDiffTime)
tcsEndTime = Lens.field @"endTime"
{-# INLINEABLE tcsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Who last modified the component.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsLastModifiedBy :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.UserContext)
tcsLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE tcsLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsLastModifiedTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.NominalDiffTime)
tcsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE tcsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsStartTime :: Lens.Lens' TrialComponentSummary (Core.Maybe Core.NominalDiffTime)
tcsStartTime = Lens.field @"startTime"
{-# INLINEABLE tcsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsStatus :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.TrialComponentStatus)
tcsStatus = Lens.field @"status"
{-# INLINEABLE tcsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ARN of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTrialComponentArn :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.TrialComponentArn)
tcsTrialComponentArn = Lens.field @"trialComponentArn"
{-# INLINEABLE tcsTrialComponentArn #-}
{-# DEPRECATED trialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead"  #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTrialComponentName :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.ExperimentEntityName)
tcsTrialComponentName = Lens.field @"trialComponentName"
{-# INLINEABLE tcsTrialComponentName #-}
{-# DEPRECATED trialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trialComponentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTrialComponentSource :: Lens.Lens' TrialComponentSummary (Core.Maybe Types.TrialComponentSource)
tcsTrialComponentSource = Lens.field @"trialComponentSource"
{-# INLINEABLE tcsTrialComponentSource #-}
{-# DEPRECATED trialComponentSource "Use generic-lens or generic-optics with 'trialComponentSource' instead"  #-}

instance Core.FromJSON TrialComponentSummary where
        parseJSON
          = Core.withObject "TrialComponentSummary" Core.$
              \ x ->
                TrialComponentSummary' Core.<$>
                  (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TrialComponentArn"
                    Core.<*> x Core..:? "TrialComponentName"
                    Core.<*> x Core..:? "TrialComponentSource"
