{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ComputeEnvironmentDetail
  ( ComputeEnvironmentDetail (..)
  -- * Smart constructor
  , mkComputeEnvironmentDetail
  -- * Lenses
  , cedComputeEnvironmentName
  , cedComputeEnvironmentArn
  , cedEcsClusterArn
  , cedComputeResources
  , cedServiceRole
  , cedState
  , cedStatus
  , cedStatusReason
  , cedTags
  , cedType
  ) where

import qualified Network.AWS.Batch.Types.CEState as Types
import qualified Network.AWS.Batch.Types.CEStatus as Types
import qualified Network.AWS.Batch.Types.CEType as Types
import qualified Network.AWS.Batch.Types.ComputeResource as Types
import qualified Network.AWS.Batch.Types.TagKey as Types
import qualified Network.AWS.Batch.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch compute environment.
--
-- /See:/ 'mkComputeEnvironmentDetail' smart constructor.
data ComputeEnvironmentDetail = ComputeEnvironmentDetail'
  { computeEnvironmentName :: Core.Text
    -- ^ The name of the compute environment.
  , computeEnvironmentArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the compute environment.
  , ecsClusterArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
  , computeResources :: Core.Maybe Types.ComputeResource
    -- ^ The compute resources defined for the compute environment.
  , serviceRole :: Core.Maybe Core.Text
    -- ^ The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
  , state :: Core.Maybe Types.CEState
    -- ^ The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ .
--
-- If the state is @ENABLED@ , then the AWS Batch scheduler can attempt to place jobs from an associated job queue on the compute resources within the environment. If the compute environment is managed, then it can scale its instances out or in automatically, based on the job queue demand.
-- If the state is @DISABLED@ , then the AWS Batch scheduler does not attempt to place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@ state continue to progress normally. Managed compute environments in the @DISABLED@ state do not scale out. However, they scale in to @minvCpus@ value after instances become idle.
  , status :: Core.Maybe Types.CEStatus
    -- ^ The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
  , statusReason :: Core.Maybe Core.Text
    -- ^ A short, human-readable string to provide additional details about the current status of the compute environment.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags applied to the compute environment.
  , type' :: Core.Maybe Types.CEType
    -- ^ The type of the compute environment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeEnvironmentDetail' value with any optional fields omitted.
mkComputeEnvironmentDetail
    :: Core.Text -- ^ 'computeEnvironmentName'
    -> Core.Text -- ^ 'computeEnvironmentArn'
    -> Core.Text -- ^ 'ecsClusterArn'
    -> ComputeEnvironmentDetail
mkComputeEnvironmentDetail computeEnvironmentName
  computeEnvironmentArn ecsClusterArn
  = ComputeEnvironmentDetail'{computeEnvironmentName,
                              computeEnvironmentArn, ecsClusterArn,
                              computeResources = Core.Nothing, serviceRole = Core.Nothing,
                              state = Core.Nothing, status = Core.Nothing,
                              statusReason = Core.Nothing, tags = Core.Nothing,
                              type' = Core.Nothing}

-- | The name of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedComputeEnvironmentName :: Lens.Lens' ComputeEnvironmentDetail Core.Text
cedComputeEnvironmentName = Lens.field @"computeEnvironmentName"
{-# INLINEABLE cedComputeEnvironmentName #-}
{-# DEPRECATED computeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedComputeEnvironmentArn :: Lens.Lens' ComputeEnvironmentDetail Core.Text
cedComputeEnvironmentArn = Lens.field @"computeEnvironmentArn"
{-# INLINEABLE cedComputeEnvironmentArn #-}
{-# DEPRECATED computeEnvironmentArn "Use generic-lens or generic-optics with 'computeEnvironmentArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
--
-- /Note:/ Consider using 'ecsClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedEcsClusterArn :: Lens.Lens' ComputeEnvironmentDetail Core.Text
cedEcsClusterArn = Lens.field @"ecsClusterArn"
{-# INLINEABLE cedEcsClusterArn #-}
{-# DEPRECATED ecsClusterArn "Use generic-lens or generic-optics with 'ecsClusterArn' instead"  #-}

-- | The compute resources defined for the compute environment.
--
-- /Note:/ Consider using 'computeResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedComputeResources :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe Types.ComputeResource)
cedComputeResources = Lens.field @"computeResources"
{-# INLINEABLE cedComputeResources #-}
{-# DEPRECATED computeResources "Use generic-lens or generic-optics with 'computeResources' instead"  #-}

-- | The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedServiceRole :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe Core.Text)
cedServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE cedServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ .
--
-- If the state is @ENABLED@ , then the AWS Batch scheduler can attempt to place jobs from an associated job queue on the compute resources within the environment. If the compute environment is managed, then it can scale its instances out or in automatically, based on the job queue demand.
-- If the state is @DISABLED@ , then the AWS Batch scheduler does not attempt to place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@ state continue to progress normally. Managed compute environments in the @DISABLED@ state do not scale out. However, they scale in to @minvCpus@ value after instances become idle.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedState :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe Types.CEState)
cedState = Lens.field @"state"
{-# INLINEABLE cedState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStatus :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe Types.CEStatus)
cedStatus = Lens.field @"status"
{-# INLINEABLE cedStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A short, human-readable string to provide additional details about the current status of the compute environment.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStatusReason :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe Core.Text)
cedStatusReason = Lens.field @"statusReason"
{-# INLINEABLE cedStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | The tags applied to the compute environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedTags :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cedTags = Lens.field @"tags"
{-# INLINEABLE cedTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The type of the compute environment.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedType :: Lens.Lens' ComputeEnvironmentDetail (Core.Maybe Types.CEType)
cedType = Lens.field @"type'"
{-# INLINEABLE cedType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ComputeEnvironmentDetail where
        parseJSON
          = Core.withObject "ComputeEnvironmentDetail" Core.$
              \ x ->
                ComputeEnvironmentDetail' Core.<$>
                  (x Core..: "computeEnvironmentName") Core.<*>
                    x Core..: "computeEnvironmentArn"
                    Core.<*> x Core..: "ecsClusterArn"
                    Core.<*> x Core..:? "computeResources"
                    Core.<*> x Core..:? "serviceRole"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusReason"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "type"
