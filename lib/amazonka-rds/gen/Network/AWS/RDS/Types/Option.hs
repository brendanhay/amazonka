{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Option
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Option
  ( Option (..),

    -- * Smart constructor
    mkOption,

    -- * Lenses
    oDBSecurityGroupMemberships,
    oOptionDescription,
    oOptionName,
    oOptionSettings,
    oOptionVersion,
    oPermanent,
    oPersistent,
    oPort,
    oVpcSecurityGroupMemberships,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBSecurityGroupMembership as Types
import qualified Network.AWS.RDS.Types.OptionSetting as Types
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.VpcSecurityGroupMembership as Types

-- | Option details.
--
-- /See:/ 'mkOption' smart constructor.
data Option = Option'
  { -- | If the option requires access to a port, then this DB security group allows access to the port.
    dBSecurityGroupMemberships :: Core.Maybe [Types.DBSecurityGroupMembership],
    -- | The description of the option.
    optionDescription :: Core.Maybe Types.String,
    -- | The name of the option.
    optionName :: Core.Maybe Types.String,
    -- | The option settings for this option.
    optionSettings :: Core.Maybe [Types.OptionSetting],
    -- | The version of the option.
    optionVersion :: Core.Maybe Types.String,
    -- | Indicate if this option is permanent.
    permanent :: Core.Maybe Core.Bool,
    -- | Indicate if this option is persistent.
    persistent :: Core.Maybe Core.Bool,
    -- | If required, the port configured for this option to use.
    port :: Core.Maybe Core.Int,
    -- | If the option requires access to a port, then this VPC security group allows access to the port.
    vpcSecurityGroupMemberships :: Core.Maybe [Types.VpcSecurityGroupMembership]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Option' value with any optional fields omitted.
mkOption ::
  Option
mkOption =
  Option'
    { dBSecurityGroupMemberships = Core.Nothing,
      optionDescription = Core.Nothing,
      optionName = Core.Nothing,
      optionSettings = Core.Nothing,
      optionVersion = Core.Nothing,
      permanent = Core.Nothing,
      persistent = Core.Nothing,
      port = Core.Nothing,
      vpcSecurityGroupMemberships = Core.Nothing
    }

-- | If the option requires access to a port, then this DB security group allows access to the port.
--
-- /Note:/ Consider using 'dBSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDBSecurityGroupMemberships :: Lens.Lens' Option (Core.Maybe [Types.DBSecurityGroupMembership])
oDBSecurityGroupMemberships = Lens.field @"dBSecurityGroupMemberships"
{-# DEPRECATED oDBSecurityGroupMemberships "Use generic-lens or generic-optics with 'dBSecurityGroupMemberships' instead." #-}

-- | The description of the option.
--
-- /Note:/ Consider using 'optionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionDescription :: Lens.Lens' Option (Core.Maybe Types.String)
oOptionDescription = Lens.field @"optionDescription"
{-# DEPRECATED oOptionDescription "Use generic-lens or generic-optics with 'optionDescription' instead." #-}

-- | The name of the option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionName :: Lens.Lens' Option (Core.Maybe Types.String)
oOptionName = Lens.field @"optionName"
{-# DEPRECATED oOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | The option settings for this option.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionSettings :: Lens.Lens' Option (Core.Maybe [Types.OptionSetting])
oOptionSettings = Lens.field @"optionSettings"
{-# DEPRECATED oOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The version of the option.
--
-- /Note:/ Consider using 'optionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionVersion :: Lens.Lens' Option (Core.Maybe Types.String)
oOptionVersion = Lens.field @"optionVersion"
{-# DEPRECATED oOptionVersion "Use generic-lens or generic-optics with 'optionVersion' instead." #-}

-- | Indicate if this option is permanent.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPermanent :: Lens.Lens' Option (Core.Maybe Core.Bool)
oPermanent = Lens.field @"permanent"
{-# DEPRECATED oPermanent "Use generic-lens or generic-optics with 'permanent' instead." #-}

-- | Indicate if this option is persistent.
--
-- /Note:/ Consider using 'persistent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPersistent :: Lens.Lens' Option (Core.Maybe Core.Bool)
oPersistent = Lens.field @"persistent"
{-# DEPRECATED oPersistent "Use generic-lens or generic-optics with 'persistent' instead." #-}

-- | If required, the port configured for this option to use.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPort :: Lens.Lens' Option (Core.Maybe Core.Int)
oPort = Lens.field @"port"
{-# DEPRECATED oPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | If the option requires access to a port, then this VPC security group allows access to the port.
--
-- /Note:/ Consider using 'vpcSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVpcSecurityGroupMemberships :: Lens.Lens' Option (Core.Maybe [Types.VpcSecurityGroupMembership])
oVpcSecurityGroupMemberships = Lens.field @"vpcSecurityGroupMemberships"
{-# DEPRECATED oVpcSecurityGroupMemberships "Use generic-lens or generic-optics with 'vpcSecurityGroupMemberships' instead." #-}

instance Core.FromXML Option where
  parseXML x =
    Option'
      Core.<$> ( x Core..@? "DBSecurityGroupMemberships"
                   Core..<@> Core.parseXMLList "DBSecurityGroup"
               )
      Core.<*> (x Core..@? "OptionDescription")
      Core.<*> (x Core..@? "OptionName")
      Core.<*> ( x Core..@? "OptionSettings"
                   Core..<@> Core.parseXMLList "OptionSetting"
               )
      Core.<*> (x Core..@? "OptionVersion")
      Core.<*> (x Core..@? "Permanent")
      Core.<*> (x Core..@? "Persistent")
      Core.<*> (x Core..@? "Port")
      Core.<*> ( x Core..@? "VpcSecurityGroupMemberships"
                   Core..<@> Core.parseXMLList "VpcSecurityGroupMembership"
               )
