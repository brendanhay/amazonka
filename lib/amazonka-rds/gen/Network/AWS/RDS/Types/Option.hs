{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Option
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Option
  ( Option (..)
  -- * Smart constructor
  , mkOption
  -- * Lenses
  , oDBSecurityGroupMemberships
  , oOptionDescription
  , oOptionName
  , oOptionSettings
  , oOptionVersion
  , oPermanent
  , oPersistent
  , oPort
  , oVpcSecurityGroupMemberships
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBSecurityGroupMembership as Types
import qualified Network.AWS.RDS.Types.OptionSetting as Types
import qualified Network.AWS.RDS.Types.VpcSecurityGroupMembership as Types

-- | Option details.
--
-- /See:/ 'mkOption' smart constructor.
data Option = Option'
  { dBSecurityGroupMemberships :: Core.Maybe [Types.DBSecurityGroupMembership]
    -- ^ If the option requires access to a port, then this DB security group allows access to the port.
  , optionDescription :: Core.Maybe Core.Text
    -- ^ The description of the option.
  , optionName :: Core.Maybe Core.Text
    -- ^ The name of the option.
  , optionSettings :: Core.Maybe [Types.OptionSetting]
    -- ^ The option settings for this option.
  , optionVersion :: Core.Maybe Core.Text
    -- ^ The version of the option.
  , permanent :: Core.Maybe Core.Bool
    -- ^ Indicate if this option is permanent.
  , persistent :: Core.Maybe Core.Bool
    -- ^ Indicate if this option is persistent.
  , port :: Core.Maybe Core.Int
    -- ^ If required, the port configured for this option to use.
  , vpcSecurityGroupMemberships :: Core.Maybe [Types.VpcSecurityGroupMembership]
    -- ^ If the option requires access to a port, then this VPC security group allows access to the port.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Option' value with any optional fields omitted.
mkOption
    :: Option
mkOption
  = Option'{dBSecurityGroupMemberships = Core.Nothing,
            optionDescription = Core.Nothing, optionName = Core.Nothing,
            optionSettings = Core.Nothing, optionVersion = Core.Nothing,
            permanent = Core.Nothing, persistent = Core.Nothing,
            port = Core.Nothing, vpcSecurityGroupMemberships = Core.Nothing}

-- | If the option requires access to a port, then this DB security group allows access to the port.
--
-- /Note:/ Consider using 'dBSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDBSecurityGroupMemberships :: Lens.Lens' Option (Core.Maybe [Types.DBSecurityGroupMembership])
oDBSecurityGroupMemberships = Lens.field @"dBSecurityGroupMemberships"
{-# INLINEABLE oDBSecurityGroupMemberships #-}
{-# DEPRECATED dBSecurityGroupMemberships "Use generic-lens or generic-optics with 'dBSecurityGroupMemberships' instead"  #-}

-- | The description of the option.
--
-- /Note:/ Consider using 'optionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionDescription :: Lens.Lens' Option (Core.Maybe Core.Text)
oOptionDescription = Lens.field @"optionDescription"
{-# INLINEABLE oOptionDescription #-}
{-# DEPRECATED optionDescription "Use generic-lens or generic-optics with 'optionDescription' instead"  #-}

-- | The name of the option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionName :: Lens.Lens' Option (Core.Maybe Core.Text)
oOptionName = Lens.field @"optionName"
{-# INLINEABLE oOptionName #-}
{-# DEPRECATED optionName "Use generic-lens or generic-optics with 'optionName' instead"  #-}

-- | The option settings for this option.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionSettings :: Lens.Lens' Option (Core.Maybe [Types.OptionSetting])
oOptionSettings = Lens.field @"optionSettings"
{-# INLINEABLE oOptionSettings #-}
{-# DEPRECATED optionSettings "Use generic-lens or generic-optics with 'optionSettings' instead"  #-}

-- | The version of the option.
--
-- /Note:/ Consider using 'optionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionVersion :: Lens.Lens' Option (Core.Maybe Core.Text)
oOptionVersion = Lens.field @"optionVersion"
{-# INLINEABLE oOptionVersion #-}
{-# DEPRECATED optionVersion "Use generic-lens or generic-optics with 'optionVersion' instead"  #-}

-- | Indicate if this option is permanent.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPermanent :: Lens.Lens' Option (Core.Maybe Core.Bool)
oPermanent = Lens.field @"permanent"
{-# INLINEABLE oPermanent #-}
{-# DEPRECATED permanent "Use generic-lens or generic-optics with 'permanent' instead"  #-}

-- | Indicate if this option is persistent.
--
-- /Note:/ Consider using 'persistent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPersistent :: Lens.Lens' Option (Core.Maybe Core.Bool)
oPersistent = Lens.field @"persistent"
{-# INLINEABLE oPersistent #-}
{-# DEPRECATED persistent "Use generic-lens or generic-optics with 'persistent' instead"  #-}

-- | If required, the port configured for this option to use.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPort :: Lens.Lens' Option (Core.Maybe Core.Int)
oPort = Lens.field @"port"
{-# INLINEABLE oPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | If the option requires access to a port, then this VPC security group allows access to the port.
--
-- /Note:/ Consider using 'vpcSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVpcSecurityGroupMemberships :: Lens.Lens' Option (Core.Maybe [Types.VpcSecurityGroupMembership])
oVpcSecurityGroupMemberships = Lens.field @"vpcSecurityGroupMemberships"
{-# INLINEABLE oVpcSecurityGroupMemberships #-}
{-# DEPRECATED vpcSecurityGroupMemberships "Use generic-lens or generic-optics with 'vpcSecurityGroupMemberships' instead"  #-}

instance Core.FromXML Option where
        parseXML x
          = Option' Core.<$>
              (x Core..@? "DBSecurityGroupMemberships" Core..<@>
                 Core.parseXMLList "DBSecurityGroup")
                Core.<*> x Core..@? "OptionDescription"
                Core.<*> x Core..@? "OptionName"
                Core.<*>
                x Core..@? "OptionSettings" Core..<@>
                  Core.parseXMLList "OptionSetting"
                Core.<*> x Core..@? "OptionVersion"
                Core.<*> x Core..@? "Permanent"
                Core.<*> x Core..@? "Persistent"
                Core.<*> x Core..@? "Port"
                Core.<*>
                x Core..@? "VpcSecurityGroupMemberships" Core..<@>
                  Core.parseXMLList "VpcSecurityGroupMembership"
