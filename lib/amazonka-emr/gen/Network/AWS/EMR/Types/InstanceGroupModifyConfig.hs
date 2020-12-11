-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupModifyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupModifyConfig
  ( InstanceGroupModifyConfig (..),

    -- * Smart constructor
    mkInstanceGroupModifyConfig,

    -- * Lenses
    igmcInstanceCount,
    igmcConfigurations,
    igmcEC2InstanceIdsToTerminate,
    igmcShrinkPolicy,
    igmcInstanceGroupId,
  )
where

import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.ShrinkPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Modify the size or configurations of an instance group.
--
-- /See:/ 'mkInstanceGroupModifyConfig' smart constructor.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    configurations ::
      Lude.Maybe [Configuration],
    ec2InstanceIdsToTerminate ::
      Lude.Maybe [Lude.Text],
    shrinkPolicy :: Lude.Maybe ShrinkPolicy,
    instanceGroupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceGroupModifyConfig' with the minimum fields required to make a request.
--
-- * 'configurations' - A list of new or modified configurations to apply for an instance group.
-- * 'ec2InstanceIdsToTerminate' - The EC2 InstanceIds to terminate. After you terminate the instances, the instance group will not return to its original requested size.
-- * 'instanceCount' - Target size for the instance group.
-- * 'instanceGroupId' - Unique ID of the instance group to modify.
-- * 'shrinkPolicy' - Policy for customizing shrink operations.
mkInstanceGroupModifyConfig ::
  -- | 'instanceGroupId'
  Lude.Text ->
  InstanceGroupModifyConfig
mkInstanceGroupModifyConfig pInstanceGroupId_ =
  InstanceGroupModifyConfig'
    { instanceCount = Lude.Nothing,
      configurations = Lude.Nothing,
      ec2InstanceIdsToTerminate = Lude.Nothing,
      shrinkPolicy = Lude.Nothing,
      instanceGroupId = pInstanceGroupId_
    }

-- | Target size for the instance group.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcInstanceCount :: Lens.Lens' InstanceGroupModifyConfig (Lude.Maybe Lude.Int)
igmcInstanceCount = Lens.lens (instanceCount :: InstanceGroupModifyConfig -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: InstanceGroupModifyConfig)
{-# DEPRECATED igmcInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | A list of new or modified configurations to apply for an instance group.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcConfigurations :: Lens.Lens' InstanceGroupModifyConfig (Lude.Maybe [Configuration])
igmcConfigurations = Lens.lens (configurations :: InstanceGroupModifyConfig -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: InstanceGroupModifyConfig)
{-# DEPRECATED igmcConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The EC2 InstanceIds to terminate. After you terminate the instances, the instance group will not return to its original requested size.
--
-- /Note:/ Consider using 'ec2InstanceIdsToTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcEC2InstanceIdsToTerminate :: Lens.Lens' InstanceGroupModifyConfig (Lude.Maybe [Lude.Text])
igmcEC2InstanceIdsToTerminate = Lens.lens (ec2InstanceIdsToTerminate :: InstanceGroupModifyConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {ec2InstanceIdsToTerminate = a} :: InstanceGroupModifyConfig)
{-# DEPRECATED igmcEC2InstanceIdsToTerminate "Use generic-lens or generic-optics with 'ec2InstanceIdsToTerminate' instead." #-}

-- | Policy for customizing shrink operations.
--
-- /Note:/ Consider using 'shrinkPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcShrinkPolicy :: Lens.Lens' InstanceGroupModifyConfig (Lude.Maybe ShrinkPolicy)
igmcShrinkPolicy = Lens.lens (shrinkPolicy :: InstanceGroupModifyConfig -> Lude.Maybe ShrinkPolicy) (\s a -> s {shrinkPolicy = a} :: InstanceGroupModifyConfig)
{-# DEPRECATED igmcShrinkPolicy "Use generic-lens or generic-optics with 'shrinkPolicy' instead." #-}

-- | Unique ID of the instance group to modify.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcInstanceGroupId :: Lens.Lens' InstanceGroupModifyConfig Lude.Text
igmcInstanceGroupId = Lens.lens (instanceGroupId :: InstanceGroupModifyConfig -> Lude.Text) (\s a -> s {instanceGroupId = a} :: InstanceGroupModifyConfig)
{-# DEPRECATED igmcInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

instance Lude.ToJSON InstanceGroupModifyConfig where
  toJSON InstanceGroupModifyConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceCount" Lude..=) Lude.<$> instanceCount,
            ("Configurations" Lude..=) Lude.<$> configurations,
            ("EC2InstanceIdsToTerminate" Lude..=)
              Lude.<$> ec2InstanceIdsToTerminate,
            ("ShrinkPolicy" Lude..=) Lude.<$> shrinkPolicy,
            Lude.Just ("InstanceGroupId" Lude..= instanceGroupId)
          ]
      )
