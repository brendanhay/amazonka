{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a target with a maintenance window.
module Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
  ( -- * Creating a request
    RegisterTargetWithMaintenanceWindow (..),
    mkRegisterTargetWithMaintenanceWindow,

    -- ** Request lenses
    rResourceType,
    rClientToken,
    rOwnerInformation,
    rName,
    rTargets,
    rDescription,
    rWindowId,

    -- * Destructuring the response
    RegisterTargetWithMaintenanceWindowResponse (..),
    mkRegisterTargetWithMaintenanceWindowResponse,

    -- ** Response lenses
    rrsWindowTargetId,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkRegisterTargetWithMaintenanceWindow' smart constructor.
data RegisterTargetWithMaintenanceWindow = RegisterTargetWithMaintenanceWindow'
  { -- | The type of target being registered with the maintenance window.
    resourceType :: MaintenanceWindowResourceType,
    -- | User-provided idempotency token.
    clientToken :: Lude.Maybe Lude.Text,
    -- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
    ownerInformation :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | An optional name for the target.
    name :: Lude.Maybe Lude.Text,
    -- | The targets to register with the maintenance window. In other words, the instances to run commands on when the maintenance window runs.
    --
    -- You can specify targets using instance IDs, resource group names, or tags that have been applied to instances.
    -- __Example 1__ : Specify instance IDs
    -- @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
    -- __Example 2__ : Use tag key-pairs applied to instances
    -- @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
    -- __Example 3__ : Use tag-keys applied to instances
    -- @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
    -- __Example 4__ : Use resource group names
    -- @Key=resource-groups:Name,Values=/resource-group-name/ @
    -- __Example 5__ : Use filters for resource group types
    -- @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
    -- For more information about these examples formats, including the best use case for each one, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window> in the /AWS Systems Manager User Guide/ .
    targets :: [Target],
    -- | An optional description for the target.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the maintenance window the target should be registered with.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTargetWithMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of target being registered with the maintenance window.
-- * 'clientToken' - User-provided idempotency token.
-- * 'ownerInformation' - User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
-- * 'name' - An optional name for the target.
-- * 'targets' - The targets to register with the maintenance window. In other words, the instances to run commands on when the maintenance window runs.
--
-- You can specify targets using instance IDs, resource group names, or tags that have been applied to instances.
-- __Example 1__ : Specify instance IDs
-- @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
-- __Example 2__ : Use tag key-pairs applied to instances
-- @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
-- __Example 3__ : Use tag-keys applied to instances
-- @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
-- __Example 4__ : Use resource group names
-- @Key=resource-groups:Name,Values=/resource-group-name/ @
-- __Example 5__ : Use filters for resource group types
-- @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
-- For more information about these examples formats, including the best use case for each one, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window> in the /AWS Systems Manager User Guide/ .
-- * 'description' - An optional description for the target.
-- * 'windowId' - The ID of the maintenance window the target should be registered with.
mkRegisterTargetWithMaintenanceWindow ::
  -- | 'resourceType'
  MaintenanceWindowResourceType ->
  -- | 'windowId'
  Lude.Text ->
  RegisterTargetWithMaintenanceWindow
mkRegisterTargetWithMaintenanceWindow pResourceType_ pWindowId_ =
  RegisterTargetWithMaintenanceWindow'
    { resourceType =
        pResourceType_,
      clientToken = Lude.Nothing,
      ownerInformation = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.mempty,
      description = Lude.Nothing,
      windowId = pWindowId_
    }

-- | The type of target being registered with the maintenance window.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' RegisterTargetWithMaintenanceWindow MaintenanceWindowResourceType
rResourceType = Lens.lens (resourceType :: RegisterTargetWithMaintenanceWindow -> MaintenanceWindowResourceType) (\s a -> s {resourceType = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClientToken :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Lude.Maybe Lude.Text)
rClientToken = Lens.lens (clientToken :: RegisterTargetWithMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOwnerInformation :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Lude.Maybe (Lude.Sensitive Lude.Text))
rOwnerInformation = Lens.lens (ownerInformation :: RegisterTargetWithMaintenanceWindow -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {ownerInformation = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

-- | An optional name for the target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: RegisterTargetWithMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The targets to register with the maintenance window. In other words, the instances to run commands on when the maintenance window runs.
--
-- You can specify targets using instance IDs, resource group names, or tags that have been applied to instances.
-- __Example 1__ : Specify instance IDs
-- @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
-- __Example 2__ : Use tag key-pairs applied to instances
-- @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
-- __Example 3__ : Use tag-keys applied to instances
-- @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
-- __Example 4__ : Use resource group names
-- @Key=resource-groups:Name,Values=/resource-group-name/ @
-- __Example 5__ : Use filters for resource group types
-- @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
-- For more information about these examples formats, including the best use case for each one, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTargets :: Lens.Lens' RegisterTargetWithMaintenanceWindow [Target]
rTargets = Lens.lens (targets :: RegisterTargetWithMaintenanceWindow -> [Target]) (\s a -> s {targets = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | An optional description for the target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Lude.Maybe (Lude.Sensitive Lude.Text))
rDescription = Lens.lens (description :: RegisterTargetWithMaintenanceWindow -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the maintenance window the target should be registered with.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWindowId :: Lens.Lens' RegisterTargetWithMaintenanceWindow Lude.Text
rWindowId = Lens.lens (windowId :: RegisterTargetWithMaintenanceWindow -> Lude.Text) (\s a -> s {windowId = a} :: RegisterTargetWithMaintenanceWindow)
{-# DEPRECATED rWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.AWSRequest RegisterTargetWithMaintenanceWindow where
  type
    Rs RegisterTargetWithMaintenanceWindow =
      RegisterTargetWithMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterTargetWithMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "WindowTargetId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterTargetWithMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.RegisterTargetWithMaintenanceWindow" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterTargetWithMaintenanceWindow where
  toJSON RegisterTargetWithMaintenanceWindow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceType" Lude..= resourceType),
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("OwnerInformation" Lude..=) Lude.<$> ownerInformation,
            ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("Targets" Lude..= targets),
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath RegisterTargetWithMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterTargetWithMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterTargetWithMaintenanceWindowResponse' smart constructor.
data RegisterTargetWithMaintenanceWindowResponse = RegisterTargetWithMaintenanceWindowResponse'
  { -- | The ID of the target definition in this maintenance window.
    windowTargetId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTargetWithMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'windowTargetId' - The ID of the target definition in this maintenance window.
-- * 'responseStatus' - The response status code.
mkRegisterTargetWithMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTargetWithMaintenanceWindowResponse
mkRegisterTargetWithMaintenanceWindowResponse pResponseStatus_ =
  RegisterTargetWithMaintenanceWindowResponse'
    { windowTargetId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the target definition in this maintenance window.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsWindowTargetId :: Lens.Lens' RegisterTargetWithMaintenanceWindowResponse (Lude.Maybe Lude.Text)
rrsWindowTargetId = Lens.lens (windowTargetId :: RegisterTargetWithMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTargetId = a} :: RegisterTargetWithMaintenanceWindowResponse)
{-# DEPRECATED rrsWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RegisterTargetWithMaintenanceWindowResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: RegisterTargetWithMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTargetWithMaintenanceWindowResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
