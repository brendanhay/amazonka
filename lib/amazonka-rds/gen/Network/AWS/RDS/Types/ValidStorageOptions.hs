{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ValidStorageOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ValidStorageOptions
  ( ValidStorageOptions (..)
  -- * Smart constructor
  , mkValidStorageOptions
  -- * Lenses
  , vsoIopsToStorageRatio
  , vsoProvisionedIops
  , vsoStorageSize
  , vsoStorageType
  , vsoSupportsStorageAutoscaling
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DoubleRange as Types
import qualified Network.AWS.RDS.Types.Range as Types

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the @DescribeValidDBInstanceModifications@ action. 
--
-- /See:/ 'mkValidStorageOptions' smart constructor.
data ValidStorageOptions = ValidStorageOptions'
  { iopsToStorageRatio :: Core.Maybe [Types.DoubleRange]
    -- ^ The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage. 
  , provisionedIops :: Core.Maybe [Types.Range]
    -- ^ The valid range of provisioned IOPS. For example, 1000-20000. 
  , storageSize :: Core.Maybe [Types.Range]
    -- ^ The valid range of storage in gibibytes. For example, 100 to 16384. 
  , storageType :: Core.Maybe Core.Text
    -- ^ The valid storage types for your DB instance. For example, gp2, io1. 
  , supportsStorageAutoscaling :: Core.Maybe Core.Bool
    -- ^ Whether or not Amazon RDS can automatically scale storage for DB instances that use the new instance class.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidStorageOptions' value with any optional fields omitted.
mkValidStorageOptions
    :: ValidStorageOptions
mkValidStorageOptions
  = ValidStorageOptions'{iopsToStorageRatio = Core.Nothing,
                         provisionedIops = Core.Nothing, storageSize = Core.Nothing,
                         storageType = Core.Nothing,
                         supportsStorageAutoscaling = Core.Nothing}

-- | The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage. 
--
-- /Note:/ Consider using 'iopsToStorageRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoIopsToStorageRatio :: Lens.Lens' ValidStorageOptions (Core.Maybe [Types.DoubleRange])
vsoIopsToStorageRatio = Lens.field @"iopsToStorageRatio"
{-# INLINEABLE vsoIopsToStorageRatio #-}
{-# DEPRECATED iopsToStorageRatio "Use generic-lens or generic-optics with 'iopsToStorageRatio' instead"  #-}

-- | The valid range of provisioned IOPS. For example, 1000-20000. 
--
-- /Note:/ Consider using 'provisionedIops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoProvisionedIops :: Lens.Lens' ValidStorageOptions (Core.Maybe [Types.Range])
vsoProvisionedIops = Lens.field @"provisionedIops"
{-# INLINEABLE vsoProvisionedIops #-}
{-# DEPRECATED provisionedIops "Use generic-lens or generic-optics with 'provisionedIops' instead"  #-}

-- | The valid range of storage in gibibytes. For example, 100 to 16384. 
--
-- /Note:/ Consider using 'storageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoStorageSize :: Lens.Lens' ValidStorageOptions (Core.Maybe [Types.Range])
vsoStorageSize = Lens.field @"storageSize"
{-# INLINEABLE vsoStorageSize #-}
{-# DEPRECATED storageSize "Use generic-lens or generic-optics with 'storageSize' instead"  #-}

-- | The valid storage types for your DB instance. For example, gp2, io1. 
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoStorageType :: Lens.Lens' ValidStorageOptions (Core.Maybe Core.Text)
vsoStorageType = Lens.field @"storageType"
{-# INLINEABLE vsoStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | Whether or not Amazon RDS can automatically scale storage for DB instances that use the new instance class.
--
-- /Note:/ Consider using 'supportsStorageAutoscaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsoSupportsStorageAutoscaling :: Lens.Lens' ValidStorageOptions (Core.Maybe Core.Bool)
vsoSupportsStorageAutoscaling = Lens.field @"supportsStorageAutoscaling"
{-# INLINEABLE vsoSupportsStorageAutoscaling #-}
{-# DEPRECATED supportsStorageAutoscaling "Use generic-lens or generic-optics with 'supportsStorageAutoscaling' instead"  #-}

instance Core.FromXML ValidStorageOptions where
        parseXML x
          = ValidStorageOptions' Core.<$>
              (x Core..@? "IopsToStorageRatio" Core..<@>
                 Core.parseXMLList "DoubleRange")
                Core.<*>
                x Core..@? "ProvisionedIops" Core..<@> Core.parseXMLList "Range"
                Core.<*>
                x Core..@? "StorageSize" Core..<@> Core.parseXMLList "Range"
                Core.<*> x Core..@? "StorageType"
                Core.<*> x Core..@? "SupportsStorageAutoscaling"
