{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iArn,
    iCreatedTime,
    iId,
    iIdentityManagementType,
    iInboundCallsEnabled,
    iInstanceAlias,
    iInstanceStatus,
    iOutboundCallsEnabled,
    iServiceRole,
    iStatusReason,
  )
where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.DirectoryAlias as Types
import qualified Network.AWS.Connect.Types.DirectoryType as Types
import qualified Network.AWS.Connect.Types.InstanceId as Types
import qualified Network.AWS.Connect.Types.InstanceStatus as Types
import qualified Network.AWS.Connect.Types.InstanceStatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon Connect instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Core.Maybe Types.ARN,
    -- | When the instance was created.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier of the Amazon Connect instance.
    id :: Core.Maybe Types.InstanceId,
    -- | The identity management type.
    identityManagementType :: Core.Maybe Types.DirectoryType,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Core.Maybe Core.Bool,
    -- | The alias of instance.
    instanceAlias :: Core.Maybe Types.DirectoryAlias,
    -- | The state of the instance.
    instanceStatus :: Core.Maybe Types.InstanceStatus,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Core.Maybe Core.Bool,
    -- | The service role of the instance.
    serviceRole :: Core.Maybe Types.ARN,
    -- | Relevant details why the instance was not successfully created.
    statusReason :: Core.Maybe Types.InstanceStatusReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { arn = Core.Nothing,
      createdTime = Core.Nothing,
      id = Core.Nothing,
      identityManagementType = Core.Nothing,
      inboundCallsEnabled = Core.Nothing,
      instanceAlias = Core.Nothing,
      instanceStatus = Core.Nothing,
      outboundCallsEnabled = Core.Nothing,
      serviceRole = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArn :: Lens.Lens' Instance (Core.Maybe Types.ARN)
iArn = Lens.field @"arn"
{-# DEPRECATED iArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the instance was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedTime :: Lens.Lens' Instance (Core.Maybe Core.NominalDiffTime)
iCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED iCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iId = Lens.field @"id"
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The identity management type.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIdentityManagementType :: Lens.Lens' Instance (Core.Maybe Types.DirectoryType)
iIdentityManagementType = Lens.field @"identityManagementType"
{-# DEPRECATED iIdentityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead." #-}

-- | Whether inbound calls are enabled.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInboundCallsEnabled :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iInboundCallsEnabled = Lens.field @"inboundCallsEnabled"
{-# DEPRECATED iInboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead." #-}

-- | The alias of instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceAlias :: Lens.Lens' Instance (Core.Maybe Types.DirectoryAlias)
iInstanceAlias = Lens.field @"instanceAlias"
{-# DEPRECATED iInstanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead." #-}

-- | The state of the instance.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceStatus :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatus)
iInstanceStatus = Lens.field @"instanceStatus"
{-# DEPRECATED iInstanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead." #-}

-- | Whether outbound calls are enabled.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOutboundCallsEnabled :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iOutboundCallsEnabled = Lens.field @"outboundCallsEnabled"
{-# DEPRECATED iOutboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead." #-}

-- | The service role of the instance.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iServiceRole :: Lens.Lens' Instance (Core.Maybe Types.ARN)
iServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED iServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Relevant details why the instance was not successfully created.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatusReason :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatusReason)
iStatusReason = Lens.field @"statusReason"
{-# DEPRECATED iStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject "Instance" Core.$
      \x ->
        Instance'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IdentityManagementType")
          Core.<*> (x Core..:? "InboundCallsEnabled")
          Core.<*> (x Core..:? "InstanceAlias")
          Core.<*> (x Core..:? "InstanceStatus")
          Core.<*> (x Core..:? "OutboundCallsEnabled")
          Core.<*> (x Core..:? "ServiceRole")
          Core.<*> (x Core..:? "StatusReason")
