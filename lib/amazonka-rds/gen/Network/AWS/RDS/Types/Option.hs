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
    oOptionName,
    oPermanent,
    oPersistent,
    oOptionDescription,
    oOptionSettings,
    oVPCSecurityGroupMemberships,
    oDBSecurityGroupMemberships,
    oOptionVersion,
    oPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.OptionSetting
import Network.AWS.RDS.Types.VPCSecurityGroupMembership

-- | Option details.
--
-- /See:/ 'mkOption' smart constructor.
data Option = Option'
  { -- | The name of the option.
    optionName :: Lude.Maybe Lude.Text,
    -- | Indicate if this option is permanent.
    permanent :: Lude.Maybe Lude.Bool,
    -- | Indicate if this option is persistent.
    persistent :: Lude.Maybe Lude.Bool,
    -- | The description of the option.
    optionDescription :: Lude.Maybe Lude.Text,
    -- | The option settings for this option.
    optionSettings :: Lude.Maybe [OptionSetting],
    -- | If the option requires access to a port, then this VPC security group allows access to the port.
    vpcSecurityGroupMemberships :: Lude.Maybe [VPCSecurityGroupMembership],
    -- | If the option requires access to a port, then this DB security group allows access to the port.
    dbSecurityGroupMemberships :: Lude.Maybe [DBSecurityGroupMembership],
    -- | The version of the option.
    optionVersion :: Lude.Maybe Lude.Text,
    -- | If required, the port configured for this option to use.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Option' with the minimum fields required to make a request.
--
-- * 'optionName' - The name of the option.
-- * 'permanent' - Indicate if this option is permanent.
-- * 'persistent' - Indicate if this option is persistent.
-- * 'optionDescription' - The description of the option.
-- * 'optionSettings' - The option settings for this option.
-- * 'vpcSecurityGroupMemberships' - If the option requires access to a port, then this VPC security group allows access to the port.
-- * 'dbSecurityGroupMemberships' - If the option requires access to a port, then this DB security group allows access to the port.
-- * 'optionVersion' - The version of the option.
-- * 'port' - If required, the port configured for this option to use.
mkOption ::
  Option
mkOption =
  Option'
    { optionName = Lude.Nothing,
      permanent = Lude.Nothing,
      persistent = Lude.Nothing,
      optionDescription = Lude.Nothing,
      optionSettings = Lude.Nothing,
      vpcSecurityGroupMemberships = Lude.Nothing,
      dbSecurityGroupMemberships = Lude.Nothing,
      optionVersion = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The name of the option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionName :: Lens.Lens' Option (Lude.Maybe Lude.Text)
oOptionName = Lens.lens (optionName :: Option -> Lude.Maybe Lude.Text) (\s a -> s {optionName = a} :: Option)
{-# DEPRECATED oOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | Indicate if this option is permanent.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPermanent :: Lens.Lens' Option (Lude.Maybe Lude.Bool)
oPermanent = Lens.lens (permanent :: Option -> Lude.Maybe Lude.Bool) (\s a -> s {permanent = a} :: Option)
{-# DEPRECATED oPermanent "Use generic-lens or generic-optics with 'permanent' instead." #-}

-- | Indicate if this option is persistent.
--
-- /Note:/ Consider using 'persistent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPersistent :: Lens.Lens' Option (Lude.Maybe Lude.Bool)
oPersistent = Lens.lens (persistent :: Option -> Lude.Maybe Lude.Bool) (\s a -> s {persistent = a} :: Option)
{-# DEPRECATED oPersistent "Use generic-lens or generic-optics with 'persistent' instead." #-}

-- | The description of the option.
--
-- /Note:/ Consider using 'optionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionDescription :: Lens.Lens' Option (Lude.Maybe Lude.Text)
oOptionDescription = Lens.lens (optionDescription :: Option -> Lude.Maybe Lude.Text) (\s a -> s {optionDescription = a} :: Option)
{-# DEPRECATED oOptionDescription "Use generic-lens or generic-optics with 'optionDescription' instead." #-}

-- | The option settings for this option.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionSettings :: Lens.Lens' Option (Lude.Maybe [OptionSetting])
oOptionSettings = Lens.lens (optionSettings :: Option -> Lude.Maybe [OptionSetting]) (\s a -> s {optionSettings = a} :: Option)
{-# DEPRECATED oOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | If the option requires access to a port, then this VPC security group allows access to the port.
--
-- /Note:/ Consider using 'vpcSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVPCSecurityGroupMemberships :: Lens.Lens' Option (Lude.Maybe [VPCSecurityGroupMembership])
oVPCSecurityGroupMemberships = Lens.lens (vpcSecurityGroupMemberships :: Option -> Lude.Maybe [VPCSecurityGroupMembership]) (\s a -> s {vpcSecurityGroupMemberships = a} :: Option)
{-# DEPRECATED oVPCSecurityGroupMemberships "Use generic-lens or generic-optics with 'vpcSecurityGroupMemberships' instead." #-}

-- | If the option requires access to a port, then this DB security group allows access to the port.
--
-- /Note:/ Consider using 'dbSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDBSecurityGroupMemberships :: Lens.Lens' Option (Lude.Maybe [DBSecurityGroupMembership])
oDBSecurityGroupMemberships = Lens.lens (dbSecurityGroupMemberships :: Option -> Lude.Maybe [DBSecurityGroupMembership]) (\s a -> s {dbSecurityGroupMemberships = a} :: Option)
{-# DEPRECATED oDBSecurityGroupMemberships "Use generic-lens or generic-optics with 'dbSecurityGroupMemberships' instead." #-}

-- | The version of the option.
--
-- /Note:/ Consider using 'optionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOptionVersion :: Lens.Lens' Option (Lude.Maybe Lude.Text)
oOptionVersion = Lens.lens (optionVersion :: Option -> Lude.Maybe Lude.Text) (\s a -> s {optionVersion = a} :: Option)
{-# DEPRECATED oOptionVersion "Use generic-lens or generic-optics with 'optionVersion' instead." #-}

-- | If required, the port configured for this option to use.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPort :: Lens.Lens' Option (Lude.Maybe Lude.Int)
oPort = Lens.lens (port :: Option -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Option)
{-# DEPRECATED oPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML Option where
  parseXML x =
    Option'
      Lude.<$> (x Lude..@? "OptionName")
      Lude.<*> (x Lude..@? "Permanent")
      Lude.<*> (x Lude..@? "Persistent")
      Lude.<*> (x Lude..@? "OptionDescription")
      Lude.<*> ( x Lude..@? "OptionSettings" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OptionSetting")
               )
      Lude.<*> ( x Lude..@? "VpcSecurityGroupMemberships" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "VpcSecurityGroupMembership")
               )
      Lude.<*> ( x Lude..@? "DBSecurityGroupMemberships" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBSecurityGroup")
               )
      Lude.<*> (x Lude..@? "OptionVersion")
      Lude.<*> (x Lude..@? "Port")
