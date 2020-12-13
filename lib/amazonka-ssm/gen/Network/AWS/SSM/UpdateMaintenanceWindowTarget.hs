{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target of an existing maintenance window. You can change the following:
--
--
--     * Name
--
--
--     * Description
--
--
--     * Owner
--
--
--     * IDs for an ID target
--
--
--     * Tags for a Tag target
--
--
--     * From any supported tag type to another. The three supported tag types are ID target, Tag target, and resource group. For more information, see 'Target' .
module Network.AWS.SSM.UpdateMaintenanceWindowTarget
  ( -- * Creating a request
    UpdateMaintenanceWindowTarget (..),
    mkUpdateMaintenanceWindowTarget,

    -- ** Request lenses
    uReplace,
    uOwnerInformation,
    uWindowTargetId,
    uName,
    uTargets,
    uDescription,
    uWindowId,

    -- * Destructuring the response
    UpdateMaintenanceWindowTargetResponse (..),
    mkUpdateMaintenanceWindowTargetResponse,

    -- ** Response lenses
    ursOwnerInformation,
    ursWindowTargetId,
    ursName,
    ursTargets,
    ursDescription,
    ursWindowId,
    ursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateMaintenanceWindowTarget' smart constructor.
data UpdateMaintenanceWindowTarget = UpdateMaintenanceWindowTarget'
  { -- | If True, then all fields that are required by the RegisterTargetWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
    replace :: Lude.Maybe Lude.Bool,
    -- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
    ownerInformation :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The target ID to modify.
    windowTargetId :: Lude.Text,
    -- | A name for the update.
    name :: Lude.Maybe Lude.Text,
    -- | The targets to add or replace.
    targets :: Lude.Maybe [Target],
    -- | An optional description for the update.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The maintenance window ID with which to modify the target.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceWindowTarget' with the minimum fields required to make a request.
--
-- * 'replace' - If True, then all fields that are required by the RegisterTargetWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
-- * 'ownerInformation' - User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
-- * 'windowTargetId' - The target ID to modify.
-- * 'name' - A name for the update.
-- * 'targets' - The targets to add or replace.
-- * 'description' - An optional description for the update.
-- * 'windowId' - The maintenance window ID with which to modify the target.
mkUpdateMaintenanceWindowTarget ::
  -- | 'windowTargetId'
  Lude.Text ->
  -- | 'windowId'
  Lude.Text ->
  UpdateMaintenanceWindowTarget
mkUpdateMaintenanceWindowTarget pWindowTargetId_ pWindowId_ =
  UpdateMaintenanceWindowTarget'
    { replace = Lude.Nothing,
      ownerInformation = Lude.Nothing,
      windowTargetId = pWindowTargetId_,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      description = Lude.Nothing,
      windowId = pWindowId_
    }

-- | If True, then all fields that are required by the RegisterTargetWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uReplace :: Lens.Lens' UpdateMaintenanceWindowTarget (Lude.Maybe Lude.Bool)
uReplace = Lens.lens (replace :: UpdateMaintenanceWindowTarget -> Lude.Maybe Lude.Bool) (\s a -> s {replace = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uReplace "Use generic-lens or generic-optics with 'replace' instead." #-}

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uOwnerInformation :: Lens.Lens' UpdateMaintenanceWindowTarget (Lude.Maybe (Lude.Sensitive Lude.Text))
uOwnerInformation = Lens.lens (ownerInformation :: UpdateMaintenanceWindowTarget -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | The target ID to modify.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uWindowTargetId :: Lens.Lens' UpdateMaintenanceWindowTarget Lude.Text
uWindowTargetId = Lens.lens (windowTargetId :: UpdateMaintenanceWindowTarget -> Lude.Text) (\s a -> s {windowTargetId = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | A name for the update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateMaintenanceWindowTarget (Lude.Maybe Lude.Text)
uName = Lens.lens (name :: UpdateMaintenanceWindowTarget -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The targets to add or replace.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTargets :: Lens.Lens' UpdateMaintenanceWindowTarget (Lude.Maybe [Target])
uTargets = Lens.lens (targets :: UpdateMaintenanceWindowTarget -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | An optional description for the update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDescription :: Lens.Lens' UpdateMaintenanceWindowTarget (Lude.Maybe (Lude.Sensitive Lude.Text))
uDescription = Lens.lens (description :: UpdateMaintenanceWindowTarget -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The maintenance window ID with which to modify the target.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uWindowId :: Lens.Lens' UpdateMaintenanceWindowTarget Lude.Text
uWindowId = Lens.lens (windowId :: UpdateMaintenanceWindowTarget -> Lude.Text) (\s a -> s {windowId = a} :: UpdateMaintenanceWindowTarget)
{-# DEPRECATED uWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.AWSRequest UpdateMaintenanceWindowTarget where
  type
    Rs UpdateMaintenanceWindowTarget =
      UpdateMaintenanceWindowTargetResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTargetResponse'
            Lude.<$> (x Lude..?> "OwnerInformation")
            Lude.<*> (x Lude..?> "WindowTargetId")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Targets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "WindowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMaintenanceWindowTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateMaintenanceWindowTarget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMaintenanceWindowTarget where
  toJSON UpdateMaintenanceWindowTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Replace" Lude..=) Lude.<$> replace,
            ("OwnerInformation" Lude..=) Lude.<$> ownerInformation,
            Lude.Just ("WindowTargetId" Lude..= windowTargetId),
            ("Name" Lude..=) Lude.<$> name,
            ("Targets" Lude..=) Lude.<$> targets,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath UpdateMaintenanceWindowTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMaintenanceWindowTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMaintenanceWindowTargetResponse' smart constructor.
data UpdateMaintenanceWindowTargetResponse = UpdateMaintenanceWindowTargetResponse'
  { -- | The updated owner.
    ownerInformation :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The target ID specified in the update request.
    windowTargetId :: Lude.Maybe Lude.Text,
    -- | The updated name.
    name :: Lude.Maybe Lude.Text,
    -- | The updated targets.
    targets :: Lude.Maybe [Target],
    -- | The updated description.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The maintenance window ID specified in the update request.
    windowId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceWindowTargetResponse' with the minimum fields required to make a request.
--
-- * 'ownerInformation' - The updated owner.
-- * 'windowTargetId' - The target ID specified in the update request.
-- * 'name' - The updated name.
-- * 'targets' - The updated targets.
-- * 'description' - The updated description.
-- * 'windowId' - The maintenance window ID specified in the update request.
-- * 'responseStatus' - The response status code.
mkUpdateMaintenanceWindowTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMaintenanceWindowTargetResponse
mkUpdateMaintenanceWindowTargetResponse pResponseStatus_ =
  UpdateMaintenanceWindowTargetResponse'
    { ownerInformation =
        Lude.Nothing,
      windowTargetId = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      description = Lude.Nothing,
      windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated owner.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursOwnerInformation :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ursOwnerInformation = Lens.lens (ownerInformation :: UpdateMaintenanceWindowTargetResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | The target ID specified in the update request.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursWindowTargetId :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Lude.Maybe Lude.Text)
ursWindowTargetId = Lens.lens (windowTargetId :: UpdateMaintenanceWindowTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTargetId = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The updated name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursName :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Lude.Maybe Lude.Text)
ursName = Lens.lens (name :: UpdateMaintenanceWindowTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursTargets :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Lude.Maybe [Target])
ursTargets = Lens.lens (targets :: UpdateMaintenanceWindowTargetResponse -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The updated description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursDescription :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ursDescription = Lens.lens (description :: UpdateMaintenanceWindowTargetResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The maintenance window ID specified in the update request.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursWindowId :: Lens.Lens' UpdateMaintenanceWindowTargetResponse (Lude.Maybe Lude.Text)
ursWindowId = Lens.lens (windowId :: UpdateMaintenanceWindowTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateMaintenanceWindowTargetResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateMaintenanceWindowTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMaintenanceWindowTargetResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
