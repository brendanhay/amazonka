{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.InstanceSummary
  ( InstanceSummary (..)
  -- * Smart constructor
  , mkInstanceSummary
  -- * Lenses
  , isArn
  , isCreatedTime
  , isId
  , isIdentityManagementType
  , isInboundCallsEnabled
  , isInstanceAlias
  , isInstanceStatus
  , isOutboundCallsEnabled
  , isServiceRole
  ) where

import qualified Network.AWS.Connect.Types.Arn as Types
import qualified Network.AWS.Connect.Types.DirectoryType as Types
import qualified Network.AWS.Connect.Types.InstanceAlias as Types
import qualified Network.AWS.Connect.Types.InstanceId as Types
import qualified Network.AWS.Connect.Types.InstanceStatus as Types
import qualified Network.AWS.Connect.Types.ServiceRole as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the instance.
--
-- /See:/ 'mkInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the instance.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the instance was created.
  , id :: Core.Maybe Types.InstanceId
    -- ^ The identifier of the instance.
  , identityManagementType :: Core.Maybe Types.DirectoryType
    -- ^ The identity management type of the instance.
  , inboundCallsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether inbound calls are enabled.
  , instanceAlias :: Core.Maybe Types.InstanceAlias
    -- ^ The alias of the instance.
  , instanceStatus :: Core.Maybe Types.InstanceStatus
    -- ^ The state of the instance.
  , outboundCallsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether outbound calls are enabled.
  , serviceRole :: Core.Maybe Types.ServiceRole
    -- ^ The service role of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceSummary' value with any optional fields omitted.
mkInstanceSummary
    :: InstanceSummary
mkInstanceSummary
  = InstanceSummary'{arn = Core.Nothing, createdTime = Core.Nothing,
                     id = Core.Nothing, identityManagementType = Core.Nothing,
                     inboundCallsEnabled = Core.Nothing, instanceAlias = Core.Nothing,
                     instanceStatus = Core.Nothing, outboundCallsEnabled = Core.Nothing,
                     serviceRole = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isArn :: Lens.Lens' InstanceSummary (Core.Maybe Types.Arn)
isArn = Lens.field @"arn"
{-# INLINEABLE isArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | When the instance was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCreatedTime :: Lens.Lens' InstanceSummary (Core.Maybe Core.NominalDiffTime)
isCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE isCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The identifier of the instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isId :: Lens.Lens' InstanceSummary (Core.Maybe Types.InstanceId)
isId = Lens.field @"id"
{-# INLINEABLE isId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The identity management type of the instance.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIdentityManagementType :: Lens.Lens' InstanceSummary (Core.Maybe Types.DirectoryType)
isIdentityManagementType = Lens.field @"identityManagementType"
{-# INLINEABLE isIdentityManagementType #-}
{-# DEPRECATED identityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead"  #-}

-- | Whether inbound calls are enabled.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInboundCallsEnabled :: Lens.Lens' InstanceSummary (Core.Maybe Core.Bool)
isInboundCallsEnabled = Lens.field @"inboundCallsEnabled"
{-# INLINEABLE isInboundCallsEnabled #-}
{-# DEPRECATED inboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead"  #-}

-- | The alias of the instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceAlias :: Lens.Lens' InstanceSummary (Core.Maybe Types.InstanceAlias)
isInstanceAlias = Lens.field @"instanceAlias"
{-# INLINEABLE isInstanceAlias #-}
{-# DEPRECATED instanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead"  #-}

-- | The state of the instance.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceStatus :: Lens.Lens' InstanceSummary (Core.Maybe Types.InstanceStatus)
isInstanceStatus = Lens.field @"instanceStatus"
{-# INLINEABLE isInstanceStatus #-}
{-# DEPRECATED instanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead"  #-}

-- | Whether outbound calls are enabled.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isOutboundCallsEnabled :: Lens.Lens' InstanceSummary (Core.Maybe Core.Bool)
isOutboundCallsEnabled = Lens.field @"outboundCallsEnabled"
{-# INLINEABLE isOutboundCallsEnabled #-}
{-# DEPRECATED outboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead"  #-}

-- | The service role of the instance.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isServiceRole :: Lens.Lens' InstanceSummary (Core.Maybe Types.ServiceRole)
isServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE isServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

instance Core.FromJSON InstanceSummary where
        parseJSON
          = Core.withObject "InstanceSummary" Core.$
              \ x ->
                InstanceSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreatedTime" Core.<*>
                    x Core..:? "Id"
                    Core.<*> x Core..:? "IdentityManagementType"
                    Core.<*> x Core..:? "InboundCallsEnabled"
                    Core.<*> x Core..:? "InstanceAlias"
                    Core.<*> x Core..:? "InstanceStatus"
                    Core.<*> x Core..:? "OutboundCallsEnabled"
                    Core.<*> x Core..:? "ServiceRole"
