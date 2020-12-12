{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProvider
  ( CapacityProvider (..),

    -- * Smart constructor
    mkCapacityProvider,

    -- * Lenses
    cpStatus,
    cpUpdateStatusReason,
    cpAutoScalingGroupProvider,
    cpName,
    cpUpdateStatus,
    cpCapacityProviderARN,
    cpTags,
  )
where

import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.CapacityProviderStatus
import Network.AWS.ECS.Types.CapacityProviderUpdateStatus
import Network.AWS.ECS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a capacity provider.
--
-- /See:/ 'mkCapacityProvider' smart constructor.
data CapacityProvider = CapacityProvider'
  { status ::
      Lude.Maybe CapacityProviderStatus,
    updateStatusReason :: Lude.Maybe Lude.Text,
    autoScalingGroupProvider ::
      Lude.Maybe AutoScalingGroupProvider,
    name :: Lude.Maybe Lude.Text,
    updateStatus :: Lude.Maybe CapacityProviderUpdateStatus,
    capacityProviderARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityProvider' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupProvider' - The Auto Scaling group settings for the capacity provider.
-- * 'capacityProviderARN' - The Amazon Resource Name (ARN) that identifies the capacity provider.
-- * 'name' - The name of the capacity provider.
-- * 'status' - The current status of the capacity provider. Only capacity providers in an @ACTIVE@ state can be used in a cluster. When a capacity provider is successfully deleted, it will have an @INACTIVE@ status.
-- * 'tags' - The metadata that you apply to the capacity provider to help you categorize and organize it. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
-- * 'updateStatus' - The update status of the capacity provider. The following are the possible states that will be returned.
--
--
--     * DELETE_IN_PROGRESS
--
--     * The capacity provider is in the process of being deleted.
--
--
--     * DELETE_COMPLETE
--
--     * The capacity provider has been successfully deleted and will have an @INACTIVE@ status.
--
--
--     * DELETE_FAILED
--
--     * The capacity provider was unable to be deleted. The update status reason will provide further details about why the delete failed.
--
--
-- * 'updateStatusReason' - The update status reason. This provides further details about the update status for the capacity provider.
mkCapacityProvider ::
  CapacityProvider
mkCapacityProvider =
  CapacityProvider'
    { status = Lude.Nothing,
      updateStatusReason = Lude.Nothing,
      autoScalingGroupProvider = Lude.Nothing,
      name = Lude.Nothing,
      updateStatus = Lude.Nothing,
      capacityProviderARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The current status of the capacity provider. Only capacity providers in an @ACTIVE@ state can be used in a cluster. When a capacity provider is successfully deleted, it will have an @INACTIVE@ status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStatus :: Lens.Lens' CapacityProvider (Lude.Maybe CapacityProviderStatus)
cpStatus = Lens.lens (status :: CapacityProvider -> Lude.Maybe CapacityProviderStatus) (\s a -> s {status = a} :: CapacityProvider)
{-# DEPRECATED cpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The update status reason. This provides further details about the update status for the capacity provider.
--
-- /Note:/ Consider using 'updateStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUpdateStatusReason :: Lens.Lens' CapacityProvider (Lude.Maybe Lude.Text)
cpUpdateStatusReason = Lens.lens (updateStatusReason :: CapacityProvider -> Lude.Maybe Lude.Text) (\s a -> s {updateStatusReason = a} :: CapacityProvider)
{-# DEPRECATED cpUpdateStatusReason "Use generic-lens or generic-optics with 'updateStatusReason' instead." #-}

-- | The Auto Scaling group settings for the capacity provider.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAutoScalingGroupProvider :: Lens.Lens' CapacityProvider (Lude.Maybe AutoScalingGroupProvider)
cpAutoScalingGroupProvider = Lens.lens (autoScalingGroupProvider :: CapacityProvider -> Lude.Maybe AutoScalingGroupProvider) (\s a -> s {autoScalingGroupProvider = a} :: CapacityProvider)
{-# DEPRECATED cpAutoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead." #-}

-- | The name of the capacity provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CapacityProvider (Lude.Maybe Lude.Text)
cpName = Lens.lens (name :: CapacityProvider -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CapacityProvider)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The update status of the capacity provider. The following are the possible states that will be returned.
--
--
--     * DELETE_IN_PROGRESS
--
--     * The capacity provider is in the process of being deleted.
--
--
--     * DELETE_COMPLETE
--
--     * The capacity provider has been successfully deleted and will have an @INACTIVE@ status.
--
--
--     * DELETE_FAILED
--
--     * The capacity provider was unable to be deleted. The update status reason will provide further details about why the delete failed.
--
--
--
-- /Note:/ Consider using 'updateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUpdateStatus :: Lens.Lens' CapacityProvider (Lude.Maybe CapacityProviderUpdateStatus)
cpUpdateStatus = Lens.lens (updateStatus :: CapacityProvider -> Lude.Maybe CapacityProviderUpdateStatus) (\s a -> s {updateStatus = a} :: CapacityProvider)
{-# DEPRECATED cpUpdateStatus "Use generic-lens or generic-optics with 'updateStatus' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the capacity provider.
--
-- /Note:/ Consider using 'capacityProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCapacityProviderARN :: Lens.Lens' CapacityProvider (Lude.Maybe Lude.Text)
cpCapacityProviderARN = Lens.lens (capacityProviderARN :: CapacityProvider -> Lude.Maybe Lude.Text) (\s a -> s {capacityProviderARN = a} :: CapacityProvider)
{-# DEPRECATED cpCapacityProviderARN "Use generic-lens or generic-optics with 'capacityProviderARN' instead." #-}

-- | The metadata that you apply to the capacity provider to help you categorize and organize it. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CapacityProvider (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CapacityProvider -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CapacityProvider)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON CapacityProvider where
  parseJSON =
    Lude.withObject
      "CapacityProvider"
      ( \x ->
          CapacityProvider'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "updateStatusReason")
            Lude.<*> (x Lude..:? "autoScalingGroupProvider")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "updateStatus")
            Lude.<*> (x Lude..:? "capacityProviderArn")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
