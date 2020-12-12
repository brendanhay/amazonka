{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeEnvironmentDetail
  ( ComputeEnvironmentDetail (..),

    -- * Smart constructor
    mkComputeEnvironmentDetail,

    -- * Lenses
    cedStatus,
    cedState,
    cedComputeResources,
    cedStatusReason,
    cedType,
    cedServiceRole,
    cedTags,
    cedComputeEnvironmentName,
    cedComputeEnvironmentARN,
    cedEcsClusterARN,
  )
where

import Network.AWS.Batch.Types.CEState
import Network.AWS.Batch.Types.CEStatus
import Network.AWS.Batch.Types.CEType
import Network.AWS.Batch.Types.ComputeResource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an AWS Batch compute environment.
--
-- /See:/ 'mkComputeEnvironmentDetail' smart constructor.
data ComputeEnvironmentDetail = ComputeEnvironmentDetail'
  { status ::
      Lude.Maybe CEStatus,
    state :: Lude.Maybe CEState,
    computeResources ::
      Lude.Maybe ComputeResource,
    statusReason :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe CEType,
    serviceRole :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    computeEnvironmentName :: Lude.Text,
    computeEnvironmentARN :: Lude.Text,
    ecsClusterARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeEnvironmentDetail' with the minimum fields required to make a request.
--
-- * 'computeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
-- * 'computeEnvironmentName' - The name of the compute environment.
-- * 'computeResources' - The compute resources defined for the compute environment.
-- * 'ecsClusterARN' - The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
-- * 'serviceRole' - The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
-- * 'state' - The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ .
--
-- If the state is @ENABLED@ , then the AWS Batch scheduler can attempt to place jobs from an associated job queue on the compute resources within the environment. If the compute environment is managed, then it can scale its instances out or in automatically, based on the job queue demand.
-- If the state is @DISABLED@ , then the AWS Batch scheduler does not attempt to place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@ state continue to progress normally. Managed compute environments in the @DISABLED@ state do not scale out. However, they scale in to @minvCpus@ value after instances become idle.
-- * 'status' - The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
-- * 'statusReason' - A short, human-readable string to provide additional details about the current status of the compute environment.
-- * 'tags' - The tags applied to the compute environment.
-- * 'type'' - The type of the compute environment.
mkComputeEnvironmentDetail ::
  -- | 'computeEnvironmentName'
  Lude.Text ->
  -- | 'computeEnvironmentARN'
  Lude.Text ->
  -- | 'ecsClusterARN'
  Lude.Text ->
  ComputeEnvironmentDetail
mkComputeEnvironmentDetail
  pComputeEnvironmentName_
  pComputeEnvironmentARN_
  pEcsClusterARN_ =
    ComputeEnvironmentDetail'
      { status = Lude.Nothing,
        state = Lude.Nothing,
        computeResources = Lude.Nothing,
        statusReason = Lude.Nothing,
        type' = Lude.Nothing,
        serviceRole = Lude.Nothing,
        tags = Lude.Nothing,
        computeEnvironmentName = pComputeEnvironmentName_,
        computeEnvironmentARN = pComputeEnvironmentARN_,
        ecsClusterARN = pEcsClusterARN_
      }

-- | The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStatus :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe CEStatus)
cedStatus = Lens.lens (status :: ComputeEnvironmentDetail -> Lude.Maybe CEStatus) (\s a -> s {status = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ .
--
-- If the state is @ENABLED@ , then the AWS Batch scheduler can attempt to place jobs from an associated job queue on the compute resources within the environment. If the compute environment is managed, then it can scale its instances out or in automatically, based on the job queue demand.
-- If the state is @DISABLED@ , then the AWS Batch scheduler does not attempt to place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@ state continue to progress normally. Managed compute environments in the @DISABLED@ state do not scale out. However, they scale in to @minvCpus@ value after instances become idle.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedState :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe CEState)
cedState = Lens.lens (state :: ComputeEnvironmentDetail -> Lude.Maybe CEState) (\s a -> s {state = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The compute resources defined for the compute environment.
--
-- /Note:/ Consider using 'computeResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedComputeResources :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe ComputeResource)
cedComputeResources = Lens.lens (computeResources :: ComputeEnvironmentDetail -> Lude.Maybe ComputeResource) (\s a -> s {computeResources = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedComputeResources "Use generic-lens or generic-optics with 'computeResources' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the compute environment.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStatusReason :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe Lude.Text)
cedStatusReason = Lens.lens (statusReason :: ComputeEnvironmentDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The type of the compute environment.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedType :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe CEType)
cedType = Lens.lens (type' :: ComputeEnvironmentDetail -> Lude.Maybe CEType) (\s a -> s {type' = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedServiceRole :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe Lude.Text)
cedServiceRole = Lens.lens (serviceRole :: ComputeEnvironmentDetail -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The tags applied to the compute environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedTags :: Lens.Lens' ComputeEnvironmentDetail (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cedTags = Lens.lens (tags :: ComputeEnvironmentDetail -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedComputeEnvironmentName :: Lens.Lens' ComputeEnvironmentDetail Lude.Text
cedComputeEnvironmentName = Lens.lens (computeEnvironmentName :: ComputeEnvironmentDetail -> Lude.Text) (\s a -> s {computeEnvironmentName = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedComputeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedComputeEnvironmentARN :: Lens.Lens' ComputeEnvironmentDetail Lude.Text
cedComputeEnvironmentARN = Lens.lens (computeEnvironmentARN :: ComputeEnvironmentDetail -> Lude.Text) (\s a -> s {computeEnvironmentARN = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedComputeEnvironmentARN "Use generic-lens or generic-optics with 'computeEnvironmentARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
--
-- /Note:/ Consider using 'ecsClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedEcsClusterARN :: Lens.Lens' ComputeEnvironmentDetail Lude.Text
cedEcsClusterARN = Lens.lens (ecsClusterARN :: ComputeEnvironmentDetail -> Lude.Text) (\s a -> s {ecsClusterARN = a} :: ComputeEnvironmentDetail)
{-# DEPRECATED cedEcsClusterARN "Use generic-lens or generic-optics with 'ecsClusterARN' instead." #-}

instance Lude.FromJSON ComputeEnvironmentDetail where
  parseJSON =
    Lude.withObject
      "ComputeEnvironmentDetail"
      ( \x ->
          ComputeEnvironmentDetail'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "computeResources")
            Lude.<*> (x Lude..:? "statusReason")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "serviceRole")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "computeEnvironmentName")
            Lude.<*> (x Lude..: "computeEnvironmentArn")
            Lude.<*> (x Lude..: "ecsClusterArn")
      )
