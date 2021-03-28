{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RdsDbInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.RdsDbInstance
  ( RdsDbInstance (..)
  -- * Smart constructor
  , mkRdsDbInstance
  -- * Lenses
  , rdiAddress
  , rdiDbInstanceIdentifier
  , rdiDbPassword
  , rdiDbUser
  , rdiEngine
  , rdiMissingOnRds
  , rdiRdsDbInstanceArn
  , rdiRegion
  , rdiStackId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'mkRdsDbInstance' smart constructor.
data RdsDbInstance = RdsDbInstance'
  { address :: Core.Maybe Core.Text
    -- ^ The instance's address.
  , dbInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The DB instance identifier.
  , dbPassword :: Core.Maybe Core.Text
    -- ^ AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
  , dbUser :: Core.Maybe Core.Text
    -- ^ The master user name.
  , engine :: Core.Maybe Core.Text
    -- ^ The instance's database engine.
  , missingOnRds :: Core.Maybe Core.Bool
    -- ^ Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
  , rdsDbInstanceArn :: Core.Maybe Core.Text
    -- ^ The instance's ARN.
  , region :: Core.Maybe Core.Text
    -- ^ The instance's AWS region.
  , stackId :: Core.Maybe Core.Text
    -- ^ The ID of the stack with which the instance is registered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RdsDbInstance' value with any optional fields omitted.
mkRdsDbInstance
    :: RdsDbInstance
mkRdsDbInstance
  = RdsDbInstance'{address = Core.Nothing,
                   dbInstanceIdentifier = Core.Nothing, dbPassword = Core.Nothing,
                   dbUser = Core.Nothing, engine = Core.Nothing,
                   missingOnRds = Core.Nothing, rdsDbInstanceArn = Core.Nothing,
                   region = Core.Nothing, stackId = Core.Nothing}

-- | The instance's address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiAddress :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiAddress = Lens.field @"address"
{-# INLINEABLE rdiAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The DB instance identifier.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDbInstanceIdentifier :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiDbInstanceIdentifier = Lens.field @"dbInstanceIdentifier"
{-# INLINEABLE rdiDbInstanceIdentifier #-}
{-# DEPRECATED dbInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead"  #-}

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDbPassword :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiDbPassword = Lens.field @"dbPassword"
{-# INLINEABLE rdiDbPassword #-}
{-# DEPRECATED dbPassword "Use generic-lens or generic-optics with 'dbPassword' instead"  #-}

-- | The master user name.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDbUser :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiDbUser = Lens.field @"dbUser"
{-# INLINEABLE rdiDbUser #-}
{-# DEPRECATED dbUser "Use generic-lens or generic-optics with 'dbUser' instead"  #-}

-- | The instance's database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiEngine :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiEngine = Lens.field @"engine"
{-# INLINEABLE rdiEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
--
-- /Note:/ Consider using 'missingOnRds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiMissingOnRds :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Bool)
rdiMissingOnRds = Lens.field @"missingOnRds"
{-# INLINEABLE rdiMissingOnRds #-}
{-# DEPRECATED missingOnRds "Use generic-lens or generic-optics with 'missingOnRds' instead"  #-}

-- | The instance's ARN.
--
-- /Note:/ Consider using 'rdsDbInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiRdsDbInstanceArn :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiRdsDbInstanceArn = Lens.field @"rdsDbInstanceArn"
{-# INLINEABLE rdiRdsDbInstanceArn #-}
{-# DEPRECATED rdsDbInstanceArn "Use generic-lens or generic-optics with 'rdsDbInstanceArn' instead"  #-}

-- | The instance's AWS region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiRegion :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiRegion = Lens.field @"region"
{-# INLINEABLE rdiRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The ID of the stack with which the instance is registered.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiStackId :: Lens.Lens' RdsDbInstance (Core.Maybe Core.Text)
rdiStackId = Lens.field @"stackId"
{-# INLINEABLE rdiStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.FromJSON RdsDbInstance where
        parseJSON
          = Core.withObject "RdsDbInstance" Core.$
              \ x ->
                RdsDbInstance' Core.<$>
                  (x Core..:? "Address") Core.<*> x Core..:? "DbInstanceIdentifier"
                    Core.<*> x Core..:? "DbPassword"
                    Core.<*> x Core..:? "DbUser"
                    Core.<*> x Core..:? "Engine"
                    Core.<*> x Core..:? "MissingOnRds"
                    Core.<*> x Core..:? "RdsDbInstanceArn"
                    Core.<*> x Core..:? "Region"
                    Core.<*> x Core..:? "StackId"
