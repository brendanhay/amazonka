{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iArn
  , iCreatedTime
  , iId
  , iIdentityManagementType
  , iInboundCallsEnabled
  , iInstanceAlias
  , iInstanceStatus
  , iOutboundCallsEnabled
  , iServiceRole
  , iStatusReason
  ) where

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
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the instance.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the instance was created.
  , id :: Core.Maybe Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , identityManagementType :: Core.Maybe Types.DirectoryType
    -- ^ The identity management type.
  , inboundCallsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether inbound calls are enabled.
  , instanceAlias :: Core.Maybe Types.DirectoryAlias
    -- ^ The alias of instance.
  , instanceStatus :: Core.Maybe Types.InstanceStatus
    -- ^ The state of the instance.
  , outboundCallsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether outbound calls are enabled.
  , serviceRole :: Core.Maybe Types.ARN
    -- ^ The service role of the instance.
  , statusReason :: Core.Maybe Types.InstanceStatusReason
    -- ^ Relevant details why the instance was not successfully created. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Instance
mkInstance
  = Instance'{arn = Core.Nothing, createdTime = Core.Nothing,
              id = Core.Nothing, identityManagementType = Core.Nothing,
              inboundCallsEnabled = Core.Nothing, instanceAlias = Core.Nothing,
              instanceStatus = Core.Nothing, outboundCallsEnabled = Core.Nothing,
              serviceRole = Core.Nothing, statusReason = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArn :: Lens.Lens' Instance (Core.Maybe Types.ARN)
iArn = Lens.field @"arn"
{-# INLINEABLE iArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | When the instance was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedTime :: Lens.Lens' Instance (Core.Maybe Core.NominalDiffTime)
iCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE iCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iId = Lens.field @"id"
{-# INLINEABLE iId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The identity management type.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIdentityManagementType :: Lens.Lens' Instance (Core.Maybe Types.DirectoryType)
iIdentityManagementType = Lens.field @"identityManagementType"
{-# INLINEABLE iIdentityManagementType #-}
{-# DEPRECATED identityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead"  #-}

-- | Whether inbound calls are enabled.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInboundCallsEnabled :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iInboundCallsEnabled = Lens.field @"inboundCallsEnabled"
{-# INLINEABLE iInboundCallsEnabled #-}
{-# DEPRECATED inboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead"  #-}

-- | The alias of instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceAlias :: Lens.Lens' Instance (Core.Maybe Types.DirectoryAlias)
iInstanceAlias = Lens.field @"instanceAlias"
{-# INLINEABLE iInstanceAlias #-}
{-# DEPRECATED instanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead"  #-}

-- | The state of the instance.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceStatus :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatus)
iInstanceStatus = Lens.field @"instanceStatus"
{-# INLINEABLE iInstanceStatus #-}
{-# DEPRECATED instanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead"  #-}

-- | Whether outbound calls are enabled.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOutboundCallsEnabled :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iOutboundCallsEnabled = Lens.field @"outboundCallsEnabled"
{-# INLINEABLE iOutboundCallsEnabled #-}
{-# DEPRECATED outboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead"  #-}

-- | The service role of the instance.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iServiceRole :: Lens.Lens' Instance (Core.Maybe Types.ARN)
iServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE iServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | Relevant details why the instance was not successfully created. 
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatusReason :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatusReason)
iStatusReason = Lens.field @"statusReason"
{-# INLINEABLE iStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

instance Core.FromJSON Instance where
        parseJSON
          = Core.withObject "Instance" Core.$
              \ x ->
                Instance' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreatedTime" Core.<*>
                    x Core..:? "Id"
                    Core.<*> x Core..:? "IdentityManagementType"
                    Core.<*> x Core..:? "InboundCallsEnabled"
                    Core.<*> x Core..:? "InstanceAlias"
                    Core.<*> x Core..:? "InstanceStatus"
                    Core.<*> x Core..:? "OutboundCallsEnabled"
                    Core.<*> x Core..:? "ServiceRole"
                    Core.<*> x Core..:? "StatusReason"
