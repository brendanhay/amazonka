-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTarget
  ( MaintenanceWindowTarget (..),

    -- * Smart constructor
    mkMaintenanceWindowTarget,

    -- * Lenses
    mResourceType,
    mOwnerInformation,
    mWindowTargetId,
    mName,
    mTargets,
    mDescription,
    mWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.MaintenanceWindowResourceType
import Network.AWS.SSM.Types.Target

-- | The target registered with the maintenance window.
--
-- /See:/ 'mkMaintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { resourceType ::
      Lude.Maybe MaintenanceWindowResourceType,
    ownerInformation ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    windowTargetId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe [Target],
    description ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    windowId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowTarget' with the minimum fields required to make a request.
--
-- * 'description' - A description for the target.
-- * 'name' - The name for the maintenance window target.
-- * 'ownerInformation' - A user-provided value that will be included in any CloudWatch events that are raised while running tasks for these targets in this maintenance window.
-- * 'resourceType' - The type of target that is being registered with the maintenance window.
-- * 'targets' - The targets, either instances or tags.
--
-- Specify instances using the following format:
-- @Key=instanceids,Values=<instanceid1>,<instanceid2>@
-- Tags are specified using the following format:
-- @Key=<tag name>,Values=<tag value>@ .
-- * 'windowId' - The ID of the maintenance window to register the target with.
-- * 'windowTargetId' - The ID of the target.
mkMaintenanceWindowTarget ::
  MaintenanceWindowTarget
mkMaintenanceWindowTarget =
  MaintenanceWindowTarget'
    { resourceType = Lude.Nothing,
      ownerInformation = Lude.Nothing,
      windowTargetId = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      description = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | The type of target that is being registered with the maintenance window.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mResourceType :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe MaintenanceWindowResourceType)
mResourceType = Lens.lens (resourceType :: MaintenanceWindowTarget -> Lude.Maybe MaintenanceWindowResourceType) (\s a -> s {resourceType = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A user-provided value that will be included in any CloudWatch events that are raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mOwnerInformation :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe (Lude.Sensitive Lude.Text))
mOwnerInformation = Lens.lens (ownerInformation :: MaintenanceWindowTarget -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | The ID of the target.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mWindowTargetId :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe Lude.Text)
mWindowTargetId = Lens.lens (windowTargetId :: MaintenanceWindowTarget -> Lude.Maybe Lude.Text) (\s a -> s {windowTargetId = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The name for the maintenance window target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mName :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe Lude.Text)
mName = Lens.lens (name :: MaintenanceWindowTarget -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The targets, either instances or tags.
--
-- Specify instances using the following format:
-- @Key=instanceids,Values=<instanceid1>,<instanceid2>@
-- Tags are specified using the following format:
-- @Key=<tag name>,Values=<tag value>@ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTargets :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe [Target])
mTargets = Lens.lens (targets :: MaintenanceWindowTarget -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A description for the target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDescription :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe (Lude.Sensitive Lude.Text))
mDescription = Lens.lens (description :: MaintenanceWindowTarget -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the maintenance window to register the target with.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mWindowId :: Lens.Lens' MaintenanceWindowTarget (Lude.Maybe Lude.Text)
mWindowId = Lens.lens (windowId :: MaintenanceWindowTarget -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: MaintenanceWindowTarget)
{-# DEPRECATED mWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.FromJSON MaintenanceWindowTarget where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowTarget"
      ( \x ->
          MaintenanceWindowTarget'
            Lude.<$> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "OwnerInformation")
            Lude.<*> (x Lude..:? "WindowTargetId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "WindowId")
      )
